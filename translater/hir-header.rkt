#lang racket

(require racket/mpair)

(define-syntax program
  (syntax-rules ()
    [(_ body ...) (begin body ...)]))


;; Globals.
(define-syntax (set-global! stx)
  (syntax-case stx ()  
    [(_ var value)
     (with-syntax ([gvar (datum->syntax #'set-global! (syntax->datum #'var))])
       #'(set! gvar value))]))

(define-syntax (get-global stx)
  (syntax-case stx ()  
    [(_ var)
     (with-syntax ([gvar (datum->syntax #'set-global! (syntax->datum #'var))])
       #'gvar)]))


;; Control constructs.
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ cond body else)
     ; =>
     (with-syntax ([break    (datum->syntax #'body 'break)]
                   [continue (datum->syntax #'body 'continue)])
       #'(call/ec (λ (break)
                    (letrec ([loop (λ ()
                                     (when cond
                                       (call/ec (λ (continue)
                                                  body))
                                       (loop)))])
                                                  
                      (loop)
                      else))))]
    
    [(_ cond body)
     ; =>
     #'(while cond body (void))]))


(define-syntax (for-each stx)
  (syntax-case stx ()
    [(_ var seq body else)
     ; =>
     (with-syntax ([break    (datum->syntax #'body 'break)]
                   [continue (datum->syntax #'body 'continue)])
       #'(call/ec (λ (break)
                    (let (($seq seq))
                      (cond
                        [(set? $seq)
                         (for ([var $seq])
                           (call/ec (λ (continue)
                                      body)))]
                        
                        [(tuple? $seq)
                         (for ([var $seq])
                           (call/ec (λ (continue)
                                      body)))]
                        
                        [(py-list? $seq)
                         (for ([var (py-list-mlist $seq)])
                           (call/ec (λ (continue)
                                      body)))]
                        
                        [(hash? $seq)
                         (for ([(var _) $seq])
                           (call/ec (λ (continue)
                                      body)))])
                      else))))]
                          

    
    [(_ var seq body)
     ; =>
     #'(for-each var seq body (void))]))

(define (return) (error "cannot return from this context"))
(define (break) (error "cannot break from this context"))


;; Exceptions.
(define $current-handler (λ (ex) ("no handler installed")))

(define (current-handler) $current-handler)
(define (set-current-handler! handler) (set! $current-handler handler))

(define-syntax (try stx)
  (syntax-case stx ()
    [(_ body handler)
     ; =>
     (with-syntax ([return   (datum->syntax #'body 'return)]
                   [continue (datum->syntax #'body 'continue)]
                   [break    (datum->syntax #'body 'break)])
       #'(let* ([$old-handler   (current-handler)]
                [$old-return    return]
                [$old-continue  continue]
                [$old-break     break]
                [return       (λ args
                                (begin (set-current-handler! $old-handler)
                                       (apply return args)))]
                [continue     (λ ()
                                (begin (set-current-handler! $old-handler)
                                       ($old-continue)))]
                [break        (λ ()
                                (begin (set-current-handler! $old-handler)
                                       ($old-break)))])
           (call/ec (λ (ec)
                      (set-current-handler! 
                       (λ (ex)
                         (set-current-handler! $old-handler)
                         (ec (handler ex))))
                      (let ([rv body])
                        (set-current-handler! $old-handler)
                        rv)))))]))
                    
(define (throw ex)
  ($current-handler ex))
     

(define (Exception) '(Exception))

  

;; Assertion.
(define-syntax assert
  (syntax-rules ()
    [(_ test) 
     (when (not test)
       (error "AssertionFailure"))]
    
    [(_ test kind)
     (when (not test)
       (error (format "AssertionFailure: ~s~n" kind)))]))


;; Data structures.
(define-syntax dict
  (syntax-rules ()
    [(_ (k v) ...)
     ; =>
     (make-hash (list (cons k v) ...))]))

(define dict? hash?)
(define dict-ref hash-ref)
(define dict-set! hash-set!)

(define-syntax tuple
  (syntax-rules ()
    [(_ v ...)
     ; =>
     (vector v ...)]))

(define tuple-ref vector-ref)
(define tuple-set! vector-set!)
(define tuple? vector?)


(define (mlist-set! mlst n value)
  (cond
    [(null? mlst)  (error "mlist-set! -- index too high")]
    [(= n 0)       (set-mcar! mlst value)]
    [else          (mlist-set! (mcdr mlst) (- n 1) value)]))

(define (mlist-remove! mlst n)
  (cond
    [(null? mlist) (error "cannot delete from empty list")]
    [(= n 1)       (set-mcdr! mlst (mcdr (mcdr mlst)))]
    [else          (mlist-remove! (mcdr mlst) (- n 1))]))

     
(define-struct py-list ([mlist #:mutable]))

(define (py-list-set! pl i val)
  (mlist-set! (py-list-mlist pl) i val))

(define (py-list-ref pl i)
  (mlist-ref (py-list-mlist pl) i))

(define (py-list-remove! pl i)
  (cond
    [(< i 0)  (error "index out of bounds for removal")]
    [(= i 0)  (set-py-list-mlist! (mcdr (py-list-mlist pl)))]
    [else     (mlist-remove! (py-list-mlist pl) i)]))
     
(define (py-list* . args)
  (py-list (list->mlist args)))
      

;; Objects.
(define-syntax get-field 
  (syntax-rules ()
    [(_ obj name) (error "get-field not supported")]))

(define-syntax set-field!
  (syntax-rules ()
    [(_ obj name val) (error "set-field! not supported")]))

(define-syntax remove-field!
  (syntax-rules ()
    [(_ obj name) (error "remove-field! not supported")]))
         

;; Operators.
(define (<< a n) (arithmetic-shift a n))
(define (>> a n) (arithmetic-shift a (- n)))

(define (not-equal? a b)
  (not (equal? a b)))

(define-syntax (define/return stx)
  (syntax-case stx ()
    [(_ f-params body ...)
     ; =>
     (with-syntax ([return (datum->syntax #'f-params 'return)])
     #'(define f-params (call/ec (λ (return) body ...))))]))
  
(define/return (in? needle haystack)
  (cond
    [(hash? haystack)     (for ([(x y) haystack])
                            (when (equal? x needle)
                              (return #t)))]
    [(py-list? haystack)  (return (in? needle (py-list-mlist haystack)))]
    [else                 (for ([x haystack])
                            (when (equal? x needle) 
                              (return #t)))])
  #f)
        
(define not-in? (λ (needle haystack) (not (in? needle haystack))))


;; Special variables
(define None 'None)
(define Ellipsis 'Ellipsis)


;; Standard continuations:
; return
; break 
(define continue (λ _ (error "top-level continue")))



;; Library functions.

(define bitwise-or bitwise-ior)

(define (py-object->string o)
  
  (define (commas seq)
    (define first? #t)
    (define ans "")
    (for ([c seq])
      (when (not first?)
        (set! ans (string-append ans ", ")))
      (when first?
        (set! first? #f))
      (set! ans (string-append ans (py-object->string c))))
    ans)
    
  (define (keyvals seq)
    (define first? #t)
    (define ans "")
    (for ([(k v) seq])
      (when (not first?)
        (set! ans (string-append ans ", ")))
      (when first?
        (set! first? #f))
      (set! ans (string-append ans (py-object->string k) ": " (py-object->string v))))
    ans)
    
  
  (cond
    [(py-list? o)   (format "[~a]" (commas (py-list-mlist o)))]
    [(tuple? o)     (format "(~a)" (commas o))]
    [(dict? o)      (format "{~a}" (keyvals o))]
    [(string? o)    (format "~v" o)] 
    [else           (format "~a" o)]))

(define (py-print x) 
  (cond 
    [(string? x)  (display x)]
    [else         (display (py-object->string x))])
  (newline))

; -- 

