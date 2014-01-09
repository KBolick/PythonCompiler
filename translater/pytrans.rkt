#lang racket

; This is a translator that takes in a parse tree and translates
; it into a high-order intermediate representation (HIR)

; environments are sets of variables/symbols
(define empty-env (set))

; The translator will use environments to track scope.
; When a variable is local, it is in the environment.

(define unique 13)
(define (get-unique-symbol)
  (set! unique (+ 1 unique))
  (string->symbol (string-append "vv" (number->string unique))))

; transform-program : python-program -> hir-program
(define (transform-program program)
  (match program
    [`(program . ,stmts)
     ; =>
     (define stmts* (flatten-stmts stmts))
     (define globals (local-bindings stmts*))
     `(program 
       ,@(for/list ([g globals]) `(define ,g (void)))
       ,@(map (λ (s) (transform-stmt empty-env s)) stmts*))]
    
    [else (error (format "not a program: ~s~n" program))]))

; flattens internal compound statements into the outer block:
(define (flatten-stmts stmts)
  (match stmts
    [`()
     '()]
    
    [(cons `(begin . ,more-stmts) rest)
     (append more-stmts (flatten-stmts rest))]
    
    [(cons stmt rest)
     (cons stmt (flatten-stmts rest))]))


; matches augmented assignment ofperators:
(define-match-expander augassign
  (syntax-rules ()
    [(_) 
     (or "+=" "-=" "*=" "/=" "%="
         "&=" "|=" "^=" "<<=" ">>=" "**=" "//=")]))

; converts an augment assignment operator to the target binop:
(define (select-augassign op)
  (match op
    ["+=" '+]
    ["-=" '-]
    ["*=" '*]
    ["/=" '/]
    ["%=" 'modulo]
    ["&=" 'bitwise-and]
    ["|=" 'bitwise-or]
    ["^=" 'bitwise-xor]
    ["<<=" '<<]
    [">>=" '>>]
    ["**=" 'expt]
    ["//=" 'quotient]))

; determines global bindings from a group of statements:
(define (global-bindings stmts)
  (match stmts
    ['() (set)]
    
    [(cons (or `(raise . ,_) 
               `(return . ,_)
               `(expr ,_)
               `(assert . ,_)
               `(continue)
               `(break)
               `(pass)
               `(del . ,_))
           rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(def (,f . ,_) . ,_) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(= ,lvals ,_) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(,(augassign) ,lvals ,_) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(nonlocal . ,vars) rest)
     ; =>
     (global-bindings rest)]
    
    [(cons `(global . ,vars) rest)
     ; =>
     (set-union (global-bindings rest) (apply set vars))]
    
    [(cons `(begin . ,stmts) rest)
     ; =>
     (global-bindings (append stmts rest))]
    
    [(cons `(cond (,tests ,suites) ...) rest)
     ; =>
     (global-bindings (append (apply append (map suite->stmts suites)) rest))]
    
    [(cons `(while ,_ ,suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite) rest))]
    
    [(cons `(while ,_ ,suite ,else-suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite)
                              (suite->stmts else-suite)
                              rest))]
    
    [(cons `(for ,var ,_ ,suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite) 
                              rest))]
    
    [(cons `(for ,var ,_ ,suite ,else-suite) rest)
     ; =>
     (global-bindings (append (suite->stmts suite)
                              (suite->stmts else-suite)
                              rest))]
    
    
    [(cons `(try ,body-suite ((,_ ,suites) ...) ,else-suite ,finally-suite) rest)
     ; =>
     (global-bindings (append (suite->stmts body-suite)
                              (apply append (map suite->stmts suites))
                              (if else-suite (suite->stmts else-suite) '())
                              (if finally-suite (suite->stmts finally-suite) '())
                              rest))]
    
    [else (error (format "couldn't compute global bindings for ~s~n" (car stmts)))]))

; finds all of the bindings in a list of l-values:
(define (lvals->bindings lvals)
  (match lvals
    ['()   (set)]
    [(cons (and (? symbol?) v) rest)
     (set-add (lvals->bindings rest) v)]
    [(cons _ rest)
     (lvals->bindings rest)]))

; finds all of the local bindings in a list of statements:
(define (local-bindings stmts)
    (match stmts
      ['() (set)]
      
      [(cons (or `(raise . ,_) 
                 `(return . ,_)
                 `(expr ,_)
                 `(assert . ,_)
                 `(continue)
                 `(break)
                 `(pass)
                 `(del . ,_))
             rest)
       ; =>
       (local-bindings rest)]
      
      [(cons `(def (,f . ,_) . ,_) rest)
       ; =>
       (set-add (local-bindings rest) f)]
      
      [(cons `(= ,lvals ,_) rest)
       ; =>
       (set-union (lvals->bindings lvals) (local-bindings rest))]
      
      [(cons `(,(augassign) ,lvals ,_) rest)
       ; =>
       (set-union (lvals->bindings lvals) (local-bindings rest))]
      
      [(cons `(nonlocal . ,vars) rest)
       ; =>
       (set-subtract (local-bindings rest) (apply set vars))]
      
      [(cons `(global . ,vars) rest)
       ; =>
       (set-subtract (local-bindings rest) (apply set vars))]
      
      [(cons `(begin . ,stmts) rest)
       ; =>
       (local-bindings (append stmts rest))]
      
      [(cons `(cond (,tests ,suites) ...) rest)
       ; =>
       (local-bindings (append (apply append (map suite->stmts suites)) rest))]
      
      [(cons `(while ,_ ,suite) rest)
       ; =>
       (local-bindings (append (suite->stmts suite) rest))]
      
      [(cons `(while ,_ ,suite ,else-suite) rest)
       ; =>
       (local-bindings (append (suite->stmts suite)
                               (suite->stmts else-suite)
                               rest))]
      
      [(cons `(for ,var ,_ ,suite) rest)
       ; =>
       (local-bindings (append (list `(= (,var) ,var))
                               (suite->stmts suite) 
                               rest))]
      
      [(cons `(for ,var ,_ ,suite ,else-suite) rest)
       ; =>
       (local-bindings (append (list `(= (,var) ,var))
                               (suite->stmts suite)
                               (suite->stmts else-suite)
                               rest))]
      
      
      [(cons `(try ,body-suite ((,_ ,suites) ...) ,else-suite ,finally-suite) rest)
       ; =>
       (local-bindings (append (suite->stmts body-suite)
                               (apply append (map suite->stmts suites))
                               (if else-suite (suite->stmts else-suite) '())
                               (if finally-suite (suite->stmts finally-suite) '())
                               rest))]
      
      [else 
         (error (format "couldn't compute local bindings for ~s~n" (car stmts)))])
  )

; generates code to set an indexed l-value:
(define (set-index env $base index expr)
  (match index
    [`(subscript ,i)
     (define $i (get-unique-symbol))
     (define $e (get-unique-symbol))
     `(let ([,$e ,$base])
        (let ([,$i ,(transform-expr env i)])
          (cond
            [(tuple? ,$e)  (tuple-set! ,$e ,$i ,(transform-expr env expr))]
            [(py-list? ,$e)    (py-list-set! ,$e ,$i ,(transform-expr env expr))]
            [(dict? ,$e)     (dict-set! ,$e ,$i ,(transform-expr env expr))])))]
    
    [`(dot ,NAME)
     `(set-field! ,$base ,NAME ,(transform-expr env expr))]
    
    [else (error (format "cannot set-index: ~s~n" index))]))

; generates code to augment an indexed l-value:
(define (set-index-augassign env $base index op expr)
  (match index
    
    [`(subscript ,i)
     (define $i (get-unique-symbol))
     (define $e (get-unique-symbol))
     (define $v (get-unique-symbol))
     (define new-env1 (set-add env $i))
     (define new-env2 (set-add new-env1 $e))
     (define new-env (set-add new-env2 $v))
     
     `(let ([,$e ,$base])
        (let ([,$i ,(transform-expr env i)])
          (let ((,$v ,(unwind-trailers new-env $e `((subscript ,$i)))))
          (cond
            [(tuple? ,$e)  (tuple-set! ,$e ,$i (,(select-augassign op) ,$v ,(transform-expr new-env expr)))]
            [(py-list? ,$e)    (py-list-set! ,$e ,$i (,(select-augassign op) ,$v ,(transform-expr new-env expr)))]
            [(dict? ,$e)     (dict-set! ,$e ,$i (,(select-augassign op) ,$v ,(transform-expr new-env expr)))]))))]
    
    [`(dot ,NAME)
     ; =>
     (define $b (get-unique-symbol))
     (define new-env (set-add env $b))
     `(let ([,$b ,$base])
        (set-field! ,$b ,NAME (,(select-augassign op) ,(transform-expr env expr)
                                   ,(transform-expr new-env `(indexed ,$b (dot ,NAME))))))]
    
    [else (error (format "cannot set-index: ~s~n" index))]))

; generates code to delete an indexed l-value:
(define (delete-index env $base index)
  (match index
    [`(subscript ,i)
     (define $i (get-unique-symbol))
     (define $b (get-unique-symbol))
     `(let ((,$b ,$base))
        (let ((,$i ,(transform-expr env i)))
          (cond
            ((tuple? ,$b) (error "Cannot delete from tuples!"))
            ((py-list? ,$b) (py-list-remove! ,$b ,$i))
            ((dict? ,$b) (dict-remove! ,$b ,$i)))))]
    
    [`(dot ,NAME)
     ; =>
     (define $b (get-unique-symbol))
     (define new-env (set-add env $b))
     `(let ([,$b ,$base])
        (remove-field! ,$b ,NAME))]
    
    [else (error (format "cannot delete index: ~s~n" index))]))

; generates code for a statement:
(define (transform-stmt env stmt)
  (match stmt
    [`(def (,f ,vars ...) ,suite)
     ; =>
      (define new-env (set-remove (set-union (apply set vars) env) 'current-exception))
     `(,(if (set-member? env f) 'set! 'set-global!) ,f (lambda ,vars
        (call/ec (lambda (return)
                   ,(transform-body-suite (apply set vars) new-env suite)))))]
    
    [`(= (,(and (? symbol?) var)) ,expr)
     ; =>
     (if (set-member? env var)
         `(set! ,var ,(transform-expr env expr))
         `(set-global! ,var ,(transform-expr env expr)))]
    
    [`(= ((indexed ,base ,trailers ... ,index)) ,value)
     ; =>
     (define $trailers (unwind-trailers env (transform-expr env base) trailers))
     (set-index env $trailers index value)]

    ;(= (a b) (tuple 2 3))
    [`(= ,(and lvals `(,_ ,_ . ,_)) ,expr)
     ; =>
     (define $t (get-unique-symbol))
     (define new-env (set-add env $t))
     (define i -1) 
     
     `(let ((,$t ,(transform-expr new-env expr))) 
        ,@(map (λ (val)
                 (begin (set! i (+ 1 i))
                        (if (set-member? new-env val)
                            `(set! 
                              ,val 
                              ,(unwind-trailers new-env (transform-expr new-env $t) `((subscript ,i))))
                            `(set-global!
                              ,val 
                              ,(unwind-trailers new-env (transform-expr new-env $t) `((subscript ,i)))))))
               lvals))]
    
    [`(,(and aug-op (augassign)) (,(and (? symbol?) lval)) ,expr)
     ; =>
     (define global? (and (symbol? lval) (not (set-member? env lval))))
     (define op (select-augassign aug-op))
     ;(if "transforming with env ~s: ~s~n" env expr)
     (define result `(,op ,(if global? `(get-global ,lval) lval) ,(transform-expr env expr)))
     (if global?
         `(set-global! ,lval ,result)
         `(set! ,lval ,result))]
         
    [`(,(and aug-op (augassign)) ((indexed ,base ,trailers ... ,index)) ,value)
     ; =>
     ;(set-index-augassign env $base index op expr)
     (set-index-augassign env (unwind-trailers env (transform-expr env base) trailers) index aug-op value)]
    
    [`(,(or '= (augassign)) ,_ ,_)
     (error "invalid assignment")]
    
    [`(del (indexed ,base ,trailers ... ,index))
     ; =>
     (delete-index
      env
      (transform-expr env `(indexed ,base ,@trailers))
      index)]
    
    [`(pass)
     ; =>
     '(void)]
    
    ['(break)
     ; =>
     '(break)]
    
    ['(continue)
     ; =>
     '(continue)]
    
    ['(return)
     ; =>
     '(return (void))]
    
    [`(return ,e)
     `(return ,(transform-expr env e))]
    
    [`(return . ,exprs)
     `(return (tuple ,@(map (λ (x) (transform-expr env x)) exprs)))]
    
    [`(raise)
     '(throw current-exception)]
    
    [`(raise ,expr)
     `(throw ,(transform-expr env expr))]
    
    [`(raise ,ex1 ,ex2)
     `(throw (chain-exception ,(transform-expr env ex1)
                              ,(transform-expr env ex2)))]
    
    [`(begin . ,stmts)
     ; =>
     (error (format "mis-placed begin: ~s~n" stmts))]
    
    [`(assert ,expr)
     ; =>
     `(assert ,(transform-expr env expr))]
    
    [`(assert ,expr1 ,expr2)
     ; =>
     `(assert ,(cons (transform-expr env expr1) (transform-expr env expr2)))]
    
    [`(expr ,expr)
     ; =>
     (transform-expr env expr)
     ]
    
    [`(cond [,tests ,suites] ... [else ,otherwise])
     ; =>
     `(cond ,@(map (λ (t s) (list (transform-expr env t)
                                  (transform-suite env s))) tests suites)
            (else ,(transform-suite env otherwise)))]
    
    
    [`(cond [,tests ,suites] ...)
     ; =>
     `(cond ,@(map (λ (t s) (list (transform-expr env t)
                                  (transform-suite env s))) tests suites))]
    
    [`(while ,test ,suite)
     ; =>
     `(while ,(transform-expr env test) ,(transform-suite env suite))]
    
    [`(while ,test ,suite ,else-suite)
     ; =>
     `(while ,(transform-expr env test) ,(transform-suite env suite) ,(transform-suite env else-suite))]
    
    [`(for ,var ,seq ,suite)
     ; =>
     (define $i (get-unique-symbol))
     `(for-each ,$i ,(transform-expr env seq) 
                (begin ,(if (set-member? env var)
                            `(set! ,var ,$i)
                            `(set-global! ,var ,$i)) 
                        ,(transform-suite env suite)))]
    
    [`(for ,var ,seq ,suite ,else-suite)
     ; =>
     (define $i (get-unique-symbol))
     `(for-each ,$i ,(transform-expr env seq) 
                (begin ,(if (set-member? env var)
                            `(set! ,var ,$i)
                            `(set-global! ,var ,$i)) 
                        ,(transform-suite env suite))
                ,(transform-suite env else-suite))]
    
    ;(try (suite (= (x) beef)) (((except) (suite (= (x) tofu)))) #f #f))
    [`(try ,suite (((except) ,on-except)) #f #f)
     ; =>
     ; (try (let () (set-global! x (get-global beef))) (lambda (ex) (let () (set-global! x (get-global tofu)))))
     `(try ,(transform-suite env suite) (lambda (ex) ,(transform-suite env on-except)))]
    
    [`(,(or 'global 'nonlocal) . ,_)
     ; =>
     `(void)]
    
    [else
     (error (format "no match for statement ~s~n" stmt))]))

; a curried form of transform-stmt, useful in conjuction with map:
(define (transform-stmt-with env)
  (λ (stmt)
    (transform-stmt env stmt)))

; selects the HIR comparison op given the Python op:
(define (select-cmp cmp)
  (match cmp
    ["<"   '<]
    [">"   '>]
    ["=="  'equal?]
    [">="  '>=]
    ["<="  '<=]
    ["!="  'not-equal?]
    
    ["in"     'in?]
    ["is"     'eq?]
    ["not-in" 'not-in?]
    ["is-not" 'not-eq?]
    
    ['in     'in?]
    ['is     'eq?]
    ['not-in 'not-in?]
    ['is-not 'not-eq?]))

; selects the HIR shift op given the Python op:
(define (select-shift op)
  (match op
    ["<<"  '<<]
    [">>"  '>>]))

; selects the HIR arithmetic op given the Python op:
(define (select-arith op)
  (match op
    ["-"  '-]
    ["+"  '+]))

; selects the HIR term op given the Python op:
(define (select-term op)
  (match op
    ["*"  '*]
    ["%"  'modulo]
    ["/"  '/]
    ["//" 'quotient]))

; unfolds a comparison exp in Python into an HIR exp:
(define (unwind-comparison env expr ops)
  (match ops
    ['() 
     (transform-expr env expr)]
    
    [`((,cmp ,rhs))
     `(,(select-cmp cmp) ,(transform-expr env expr) ,(transform-expr env rhs))]
    
    [(cons (list cmp rhs) rest)
     (define $cv (get-unique-symbol))
     (define new-env (set-add env $cv))
     `(let ((,$cv ,(transform-expr new-env rhs))) (if (,(select-cmp cmp) ,expr ,$cv) ,(unwind-comparison new-env $cv rest) #f))]
        
    [else (error (format "no match: ~s ~s~n" expr ops))]))

; unfolds a binary op exp in Python into an HIR exp:
(define (unwind-op select-op env $expr ops)
  (match ops
    ['() 
     (transform-expr env $expr)]
    
    [`((,op ,rhs))
     `(,(select-op op) 
       ,$expr
       ,(transform-expr env rhs))]
    
    [(cons (list op rhs) rest)
     (unwind-op select-op 
                env 
                `(,(select-op op) ,$expr
                                  ,(transform-expr env rhs))
                rest)]))

; unfolds a trailer in Python into an HIR ep:
(define (unwind-trailer env $expr trailer)
  (match trailer
    [`(dot ,NAME)
     `(get-field ,$expr ,NAME)]
    
    [`(called . ,args)
     `(,$expr ,@(map (lambda (arg)
                      (transform-expr env arg))
                    args))]
    
    [`(subscript ,i)
     (define $i (get-unique-symbol))
     (define $e (get-unique-symbol))
     ;added a new environment here
     (define new-env (set-add env (set $i $e)))
     `(let ([,$e ,$expr])
        (let ([,$i ,(transform-expr new-env i)])
          (cond
            [(py-list? ,$e)  (py-list-ref ,$e ,$i)]
            [(tuple? ,$e)    (tuple-ref ,$e ,$i)]
            [(dict? ,$e)     (dict-ref ,$e ,$i)]
            [else            (error "cannot index object")])))]
    
    [else (error (format "unknown trailer: ~s~n" trailer))]))

; unfolds a sequence of trailers into an HIR exp:
(define (unwind-trailers env $expr trailers)
  (match trailers
    ['() 
     $expr]
    
    [`(,trailer)
     (unwind-trailer env $expr trailer)]
    
    [(cons trailer rest)
     (unwind-trailers env (unwind-trailer env $expr trailer) rest)]))

; transforms a Python exp into an HIR exp:
(define (transform-expr env expr)
  (begin
    ;(display "In transform-expr\n")
    ;(display expr)
    ;(display "\n")
    (match expr
      ['print       'py-print]
      ['True        #t]
      ['False       #f]
      ['Ellipsis    '(quote Ellipsis)]
      ['None        '(quote None)]
      [(? number?)  expr]
      [(? string?)  expr]
      
      [(? symbol?)  
       (if (set-member? env expr)
           expr
           `(get-global ,expr))]
      
      [`(if ,cond ,true ,false)
       ; =>
       `(if ,(transform-expr env cond)
            ,(transform-expr env true)
            ,(transform-expr env false))]
      
      [`(lambda ,vars ,expr)
       ; =>
       (define new-env (set-union env (apply set vars)))
       `(lambda ,vars ,(transform-expr new-env expr))]
      
      [`(,(and op (and x (or 'or 'and 'bitwise-or 'bitwise-and 'bitwise-xor))) 
         . ,exprs)
       ; =>
       `(,op ,@(map (λ (expression)
                      (transform-expr env expression)
                      )exprs))]
      
      [`(not ,expr)
       ; =>
       `(not ,(transform-expr env expr))]
      
      [`(comparison ,base . ,ops)
       (unwind-comparison env base ops)]
      
    
      [`(shift ,base . ,ops)
       (begin
         ;xformed is an sexpression that we will build up, starting with base.
         (define transformed (transform-expr env base))

         ;for each op in ops
         (for-each (λ (op)
                     ;match the arith operator
                     (match op
                       [`(,shift-op ,expr) 
                        (set! transformed 
                              `(,(select-shift shift-op) 
                                ,transformed 
                                ,(transform-expr env expr)))])
                     )ops)
         transformed)
       ]
      
      ;arith comes in looking something like this
      ;x+y+3 
      ;(arith x ("+" y) ("+" 3))
      [`(arith ,base . ,ops)
       (begin
         ;xformed is an sexpression that we will build up, starting with base.
         (define transformed (transform-expr env base))

         ;for each op in ops
         (for-each (λ (op)
                     ;match the arith operator
                     (match op
                       [`(,arith-op ,expr) 
                        (set! transformed 
                              `(,(select-arith arith-op) 
                                ,transformed 
                                ,(transform-expr env expr)))])
                     )ops)
         transformed)
       ]

      
      [`(term ,base . ,ops)
       (begin
         ;xformed is an sexpression that we will build up, starting with base.
         (define transformed (transform-expr env base))
         
         ;for each op in ops
         (for-each (λ (op)
                     ;match the arith operator
                     (match op
                       [`(,term-op ,expr) 
                        (set! transformed 
                              `(,(select-term term-op) 
                                ,transformed 
                                ,(transform-expr env expr)))])
                     )ops)
         transformed)
       ]
      
      [`("+" ,expr)
       `(+ ,(transform-expr env expr))]
      
      [`("-" ,expr)
       `(- ,(transform-expr env expr))]
      
      [`("~" ,expr)
       `(bitwise-not ,(transform-expr env expr))]
      
      [`(indexed ,expr . ,trailers)
       (unwind-trailers env (transform-expr env expr) trailers)]
      
      [`(power ,base ,expn)
       `(expt ,(transform-expr env base) ,(transform-expr env expn))]
      
      [`(set . ,exprs)
       `(set ,@(map (λ (expression)
                      (transform-expr env expression)
                      )exprs))]
      
      [`(tuple . ,exprs)
       `(tuple ,@(map (λ (expression)
                      (transform-expr env expression)
                      )exprs))]
      
      [`(list . ,exprs)
       (begin
         (define transformed '())
         (for-each (λ (expression)
                    (set! transformed (append transformed `(,(transform-expr env expression))))
                    )exprs)
         `(py-list* ,@transformed))]
      
      [`(dict . ,pairs)
       (begin
         (define transformed '())
         (for-each (λ (pair)
                     (set! transformed (append transformed `((,(transform-expr env (car pair))
                                                              ,(transform-expr env (cadr pair))))))
                     )pairs)
         `(dict ,@transformed))]
      
      [else         
       (error (format "cannot transform expr: ~s~n" expr))])
    )
  )

; transform a suite into a list of statements:
(define (suite->stmts suite)
  (match suite
    [`(suite . ,stmts) stmts]
    [`(begin . ,stmts) stmts]
    [stmt              (list stmt)]))

; transform a suite that begins a new scope:
(define (transform-body-suite params env suite)
  (match suite
    [`(,(or 'suite 'begin) . ,(app flatten-stmts stmts))
     ; =>
     (define locals  (local-bindings stmts))
     (define globals (global-bindings stmts))
     (define new-env (set-subtract (set-union locals env) globals))
     `(let (,@(for/list ([v (set-subtract locals params)])
                (list v '(void))))
        ,@(map (λ (s) (transform-stmt new-env s)) stmts))]
    
    [else (transform-stmt env suite)]))

; transform a suite that does not begin a new scope:
(define (transform-suite env suite)
  (match suite
    [`(,(or 'suite 'begin) . ,(app flatten-stmts stmts))
     ; =>
     `(let ()
        ,@(map (transform-stmt-with env) stmts))]
    
    [else (transform-stmt env suite)]))


(define input #f)


(match (current-command-line-arguments)
  [(vector)
   (define in (current-input-port))
   (set! input (read in))])

(pretty-write (transform-program input))