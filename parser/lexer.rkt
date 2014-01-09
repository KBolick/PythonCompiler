#lang racket

(define outputPort
  (current-output-port))
(define continue #t)           ;continue lexing, unless abort or eof
(define in-string #f)          ;true if we are in a string
(define string-literal "")     ;the literal string
(define indent (cons 0 empty)) ;indentation stack
(define parens 0)              ;int to increment/decrement ww open/close parens
(define newline #t)            ;true when last token was newline.false otherwise

(require parser-tools/lex)

(require (prefix-in re- parser-tools/lex-sre))

(define-lex-abbrev IDENTIFIER
  (re-seq (union (char-range #\a #\z) (char-range #\A #\Z) "_")
          (re-* (union (char-range #\a #\z) 
                       (char-range #\A #\Z) 
                       (char-range #\1 #\9) "0" "_"))))

(define-lex-abbrev KEYWORD 
  (union "False" "class" "finally" "is" "return" "None" "continue" "for" 
         "lambda" "try" "True" "def" "from" "nonlocal" "while" "and" "del" 
         "global" "not" "with" "as" "elif" "if" "or" "yield" "assert" "else" 
         "import" "pass" "break" "except" "in" "raise"))

(define-lex-abbrev PUNCT 
  (union "(" ")" "[" "]" "{" "}" "," ":" "." ";" "@" "=" "+=" "-=" "*=" "/=" 
         "//=" "%=" "&=" "|=" "^=" ">>=" "<<=" "**=" "+" "-" "*" "**" "/" "//"
         "%" "<<" ">>" "&" "|" "^" "~" "<" ">" "<=" ">=" "==" "!=" "..."))

(define-lex-abbrev COMMENT
  (re-seq (re-* blank) "#" (re-* (char-complement "\n")) "\n"))

(define-lex-abbrev LINEJOIN
  (re-seq (re-* blank) "\\" (re-* blank) "\n" ))

(define-lex-abbrev BADLINEJOIN
  (re-seq "\\" (re-+ (char-complement "\n")) "\n"))

(define-lex-abbrev NEWLINE
  (re-seq (re-* blank) "\n" ))

(define-lex-abbrev BLANKS
  (re-+ blank))

(define-lex-abbrev RAWPREFIX
  (union "br" "Br" "bR" "BR" "rb" "rB" "Rb" "RB" "r" "R"))

(define-lex-abbrev ESCPREFIX
  (union "" "u" "U" "b" "B"))

(define-lex-abbrev STRINGSTART
  (union "'" "\"" (re-= 3 "'") (re-= 3 "\"")))

(define-lex-abbrev ESCSTRINGSTART
  (re-seq ESCPREFIX STRINGSTART))

(define-lex-abbrev RAWSTRINGSTART
  (re-seq RAWPREFIX STRINGSTART))

(define-lex-abbrev NONZERODIGIT
  (char-range #\1 #\9))

(define-lex-abbrev DIGIT
  (union #\0 NONZERODIGIT))

;make bad ints?  ie 125w
(define-lex-abbrev DECIMALINTEGER
  (union (re-+ #\0) (re-seq NONZERODIGIT (re-* DIGIT))))

(define-lex-abbrev OCTDIGIT
  (union #\0 (char-range #\1 #\7)))

;make bad ints?  ie 0o558
(define-lex-abbrev OCTINTEGER
  (re-seq #\0 (union #\o #\O) (re-+ OCTDIGIT)))

(define-lex-abbrev ESCAPEOCT
  (re-seq "\\" (union OCTDIGIT (re-= 2 OCTDIGIT) (re-= 3 OCTDIGIT))))

(define-lex-abbrev HEXDIGIT
  (union DIGIT (char-range #\a #\f) (char-range #\A #\F)))

;make bad ints? ie 0x234AFG
(define-lex-abbrev HEXINTEGER
  (re-seq #\0 (union #\x #\X) (re-+ HEXDIGIT)))

(define-lex-abbrev ESCAPEHEX
  (re-seq "\\x" (re-= 2 HEXDIGIT)))
  
(define-lex-abbrev BINDIGIT
  (union #\0 #\1) )

;make bad ints?  0b10012
(define-lex-abbrev BININTEGER
  (re-seq #\0 (union #\b #\B) (re-+ BINDIGIT)))

(define-lex-abbrev INTEGER
  (union DECIMALINTEGER OCTINTEGER 
         HEXINTEGER BININTEGER))

(define-lex-abbrev FRACTION
  (re-seq #\. (re-+ DIGIT)))

(define-lex-abbrev EXPONENT
  (re-seq (union #\e #\E) (union "" #\+ #\-) (re-+ DIGIT)))

(define-lex-abbrev INTPART
  (re-+ DIGIT))

(define-lex-abbrev POINTFLOAT
  (union (re-seq (union "" INTPART) FRACTION) (re-seq INTPART #\.)))

(define-lex-abbrev EXPONENTFLOAT
  (re-seq (union INTPART POINTFLOAT) EXPONENT))

(define-lex-abbrev FLOATNUMBER
  (union POINTFLOAT EXPONENTFLOAT))

(define-lex-abbrev IMAGNUMBER
  (re-seq (union FLOATNUMBER INTPART) (union #\j #\J)))

(define-lex-abbrev UNKNOWN
  (union "$" "?" "`"))

(define (produce type string)
  (begin (display "(" outputPort)
         (display type outputPort) 
         (display " \"" outputPort)  ; raco exe lexer.rkt
         (display string outputPort) ; ./lexer < test.txt > output.txt
         (display "\")\n" outputPort)))

(define (produce_noquote type string)
  (begin (display "(" outputPort)
         (display type outputPort) 
         (display " " outputPort) 
         (display string outputPort) 
         (display ")\n" outputPort)))

(define (produce_empty type)
  (begin (display "(" outputPort) 
         (display type outputPort) 
         (display ")\n" outputPort)))

(define (dedent length)
  (cond
    ; if value on top of stack is less than length
    ; display error and return -1 (will terminate)
    [(< (car indent) length)
     (begin (produce "ERROR" "mis-indented program") 
            (set! continue #f))]
    ; if value on top of stack is equal to length
    ; do nothing
    [(equal? (car indent) length) void]
    ; if value on top of stack is greater than length
    ; produce a dedent token, pop stack, and recurse
    [(> (car indent) length)
     (begin (produce_empty "DEDENT") 
            (set! indent (cdr indent))
            (dedent length))]))

(define (offstep length)
  (cond
    ; do nothing on equal
    [(equal? length (car indent) ) void]
    ; produce token and push onto indent stack on greater than
    [(> length (car indent) )
     (begin (produce_empty "INDENT")
            (set! indent (cons length indent))
            (set! newline #f))]
    ; call dedent on less than
    [(< length (car indent))
     (begin (dedent length)
            (set! newline #f))]))

(define (handle_parens punc)
  (begin
    (cond [(equal? "(" punc) (set! parens (+ parens 1))]
          [(equal? "[" punc) (set! parens (+ parens 1))]
          [(equal? "{" punc) (set! parens (+ parens 1))]
          [(equal? ")" punc) (set! parens (- parens 1))]
          [(equal? "]" punc) (set! parens (- parens 1))]
          [(equal? "}" punc) (set! parens (- parens 1))])
    (when (< parens 0) (set! parens (+ parens 1)))))

(define (append-literal a)
  (set! string-literal (string-append string-literal a)))

(define rss ;raw short singlequote
  (lexer ["\\\\" (append-literal "\\\\\\\\")]
         ["\\'" (append-literal "\\\\\\'")]
         ["'"  (set! in-string #f)]
         ["\\\n" (append-literal "\\\\\\n")]
         ["\n" (begin (set! continue #f) (produce "ERROR" "newline in string"))]
         ["\"" (append-literal "\\\"")]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "'") (append-literal lexeme)]))

(define rsq ;raw short quote
  (lexer ["\\\\" (append-literal "\\\\\\\\")]
         ["\\\"" (append-literal "\\\\\\\"")]
         ["\""   (set! in-string #f)]
         ["'" (append-literal "\\'")]
         ["\\'" (append-literal "\\\\\\'")]
         ["\\\n" (append-literal "\\\\\\n")]
         ["\n" (begin (set! continue #f) (produce "ERROR" "newline in string"))]
         ["\"" (append-literal "\\\"")]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "\"") (append-literal lexeme)]))

(define rls ;raw long singlequote
  (lexer [(re-= 3 "'") (set! in-string #f)]
         ["\\\\" (append-literal "\\\\\\\\")]
         ["\"" (append-literal "\\\"")]
         ["\\'" (append-literal "\\\\\\'")]
         ["'" (append-literal "\\'")]
         ["\r" (append-literal "\\r")]
         ["\n" (append-literal "\\n")]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "'") (append-literal lexeme)]))

(define rlq ;raw long quote
  (lexer [(re-= 3 "\"") (set! in-string #f)]
         ["\\\\" (append-literal "\\\\\\\\")]
         ["\\\"" (append-literal "\\\\\\\"")]
         ["\"" (append-literal "\\\"")]
         ["\r" (append-literal "\\r")]
         ["\n" (append-literal "\\n")]
         ["'" (append-literal "\\'")]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "\"") (append-literal lexeme)]))

(define ess ;escapable short singlequot
  (lexer ["\\\\" (append-literal lexeme)]
         ["\\'" (append-literal lexeme)]  
         ["\\\"" (append-literal lexeme)]
         ["'"   (set! in-string #f)]
         ["\"" (append-literal "\\\"")]
         ["\\a" (append-literal lexeme)]
         ["\\b"(append-literal lexeme)]
         ["\\f"(append-literal lexeme)]
         ["\\n"(append-literal lexeme)]
         ["\\r"(append-literal lexeme)]
         ["\\t"(append-literal lexeme)]
         ["\\v"(append-literal lexeme)]
         ["\\\n" void]
         ["\n" (begin (set! continue #f) (produce "ERROR" "newline in string"))]
         [ESCAPEOCT 
          (begin (define modified "#o")
                 (set! modified (string-append modified (substring lexeme 1)))
                 (append-literal (string (integer->char (string->number modified)))))]
         [ESCAPEHEX
          (begin (define modified "#x")
                 (set! modified (string-append modified (substring lexeme 2)))
                 (append-literal (string (integer->char (string->number modified)))))]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "'") (append-literal lexeme)]))

(define esq ;escapable short quote
  (lexer ["\\\\" (append-literal lexeme)]
         ["\\'" (append-literal lexeme)]  
         ["\\\"" (append-literal lexeme)]
         ["'"   (append-literal "\\'")]
         ["\"" (set! in-string #f)]
         ["\\a" (append-literal lexeme)]
         ["\\b"(append-literal lexeme)]
         ["\\f"(append-literal lexeme)]
         ["\\n"(append-literal lexeme)]
         ["\\r"(append-literal lexeme)]
         ["\\t"(append-literal lexeme)]
         ["\\v"(append-literal lexeme)]
         ["\\\n" void]
         ["\n" (begin (set! continue #f) (produce "ERROR" "newline in string"))]
         [ESCAPEOCT 
          (begin (define modified "#o")
                 (set! modified (string-append modified (substring lexeme 1)))
                 (append-literal (string (integer->char (string->number modified)))))]
         [ESCAPEHEX
          (begin (define modified "#x")
                 (set! modified (string-append modified (substring lexeme 2)))
                 (append-literal (string (integer->char (string->number modified)))))]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "\"") (append-literal lexeme)]))

(define els ; escapable long singlequote
  (lexer ["\\\\" (append-literal lexeme)]
         [(re-= 3 "'") (set! in-string #f)]
         ["\\'" (append-literal lexeme)]  
         ["\\\"" (append-literal lexeme)]
         ["'"   (append-literal "\\'")]
         ["\"" (append-literal "\\\"")]
         ["\\a" (append-literal lexeme)]
         ["\\b"(append-literal lexeme)]
         ["\\f"(append-literal lexeme)]
         ["\\n"(append-literal lexeme)]
         ["\\r"(append-literal lexeme)]
         ["\\t"(append-literal lexeme)]
         ["\\v"(append-literal lexeme)]
         ["\\\n" void]
         ["\n" (append-literal "\\n")]
         ["\r" (append-literal "\\r")]
         [ESCAPEOCT 
          (begin (define modified "#o")
                 (set! modified (string-append modified (substring lexeme 1)))
                 (append-literal (string (integer->char (string->number modified)))))]
         [ESCAPEHEX
          (begin (define modified "#x")
                 (set! modified (string-append modified (substring lexeme 2)))
                 (append-literal (string (integer->char (string->number modified)))))]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "'") (append-literal lexeme)]))

(define elq ;escapable long quote
  (lexer ["\\\\" (append-literal lexeme)]
         [(re-= 3 "\"") (set! in-string #f)]
         ["\\'" (append-literal lexeme)]  
         ["\\\"" (append-literal lexeme)]
         ["'"   (append-literal "\\'")]
         ["\"" (append-literal "\\\"")]
         ["\\a" (append-literal lexeme)]
         ["\\b"(append-literal lexeme)]
         ["\\f"(append-literal lexeme)]
         ["\\n"(append-literal lexeme)]
         ["\\r"(append-literal lexeme)]
         ["\\t"(append-literal lexeme)]
         ["\\v"(append-literal lexeme)]
         ["\\\n" void]
         ["\n" (append-literal "\\n")]
         ["\r" (append-literal "\\r")]
         [ESCAPEOCT 
          (begin (define modified "#o")
                 (set! modified (string-append modified (substring lexeme 1)))
                 (append-literal (string (integer->char (string->number modified)))))]
         [ESCAPEHEX
          (begin (define modified "#x")
                 (set! modified (string-append modified (substring lexeme 2)))
                 (append-literal (string (integer->char (string->number modified)))))]
         ["\\" (append-literal "\\\\")]
         [(eof) (begin (set! continue #f) (set! in-string #f)
                       (produce "ERROR" "unterminated string literal at EOF"))]
         [(char-complement "\"") (append-literal lexeme)]))

(define (string-stream lexer port)
  (let ([lexing 
         (lexer port)])
    (if (equal? in-string #t) 
        (string-stream lexer port)
        (when (equal? continue #t) 
          (produce "LIT" string-literal)))
    (set! string-literal "")))

(define pylexer
  (lexer
   
   [KEYWORD        (begin 
                     (when (equal? newline #t) (dedent 0))
                     (produce_noquote "KEYWORD" lexeme) 
                     (set! newline #f))]
   
   [ESCSTRINGSTART  (begin
                      (when (equal? newline #t) (dedent 0))
                      (set! in-string #t)
                      (set! newline #f)
                      (cond
                        [(regexp-match #rx"'''" lexeme) 
                         (string-stream els (current-input-port))]
                        [(regexp-match #rx"\"\"\"" lexeme) 
                         (string-stream elq (current-input-port))]
                        [(regexp-match #rx"'" lexeme) 
                         (string-stream ess (current-input-port))]
                        [(regexp-match #rx"\"" lexeme) 
                         (string-stream esq (current-input-port))]))]
   
   [RAWSTRINGSTART  (begin 
                      (when (equal? newline #t) (dedent 0))
                      (set! in-string #t)
                      (set! newline #f)
                      (cond 
                        [(regexp-match #rx"'''" lexeme) 
                         (string-stream rls (current-input-port))]
                        [(regexp-match #rx"\"\"\"" lexeme) 
                         (string-stream rlq (current-input-port))]
                        [(regexp-match #rx"'" lexeme) 
                         (string-stream rss (current-input-port))]
                        [(regexp-match #rx"\"" lexeme) 
                         (string-stream rsq (current-input-port))]))]
   
   [INTEGER        (begin
                     (when (equal? newline #t) (dedent 0))
                     (define modified lexeme)
                     (set! modified (regexp-replace "0b" modified "#b"))
                     (set! modified (regexp-replace "0B" modified "#B"))
                     (set! modified (regexp-replace "0o" modified "#o"))
                     (set! modified (regexp-replace "0O" modified "#O"))
                     (set! modified (regexp-replace "0x" modified "#x"))
                     (set! modified (regexp-replace "0X" modified "#X"))
                     (produce_noquote "LIT" modified)
                     (set! newline #f))]
   
   [FLOATNUMBER    (begin
                     (when (equal? newline #t) (dedent 0))
                     (produce_noquote "LIT" lexeme)
                     (set! newline #f))]
   
   [IMAGNUMBER     (begin
                     (when (equal? newline #t) (dedent 0))
                     (define modified "+" )
                     (set! modified 
                           (string-append modified 
                                          (regexp-replace "j" lexeme "i")))
                     (produce_noquote "LIT" modified)
                     (set! newline #f))]
   
   [IDENTIFIER     (begin
                     (when (equal? newline #t) (dedent 0))
                     (produce "ID" lexeme) 
                     (set! newline #f))]
   
   [PUNCT          (begin
                     (when (equal? newline #t) (dedent 0))
                     (handle_parens lexeme)
                     (produce "PUNCT" lexeme) 
                     (set! newline #f))]
   
   [COMMENT        (when (and (not newline) (equal? 0 parens))
                     (begin (produce_empty "NEWLINE") 
                            (set! newline #t)))]
   
   [LINEJOIN       (set! newline #f)]
   
   [BADLINEJOIN    (begin
                     (set! continue #f)
                     (produce "ERROR" 
                              "unexpected character after line continuation character"))]
   
   [NEWLINE        (cond
                     [(equal? newline #t) void]    ; no token if newline
                     [(> parens 0) void] ; no token if in parens
                     [else (begin (produce_empty "NEWLINE") 
                                  (set! newline #t))])]
   
   [BLANKS         (when (equal? newline #t) 
                     (begin
                       (offstep (string-length lexeme))
                       (when (> (string-length lexeme) 0) 
		       	     (set! newline #f))))]
   
   [UNKNOWN        (begin
                     (define error "unknown char: '")
                     (set! error (string-append error lexeme))
                     (set! error (string-append error "'"))
                     (produce "ERROR" error)
                     (set! continue #f))]
                          
   [(eof)          (begin (dedent 0)
                          (produce_empty "ENDMARKER") 
                          (set! continue #f))]))

(define (lex-stream lexer port)
  (let ([lexing (lexer port)])
    (when (equal? continue #t) (lex-stream lexer port))))

(lex-stream pylexer (current-input-port))