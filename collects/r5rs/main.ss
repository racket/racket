
(module main scheme/base
  (require syntax/namespace-reflect
           (for-syntax scheme/base))

  (provide (for-syntax syntax-rules ...)
           (rename-out
            [mcons cons]
            [mcar car]
            [mcdr cdr]
            [set-mcar! set-car!]
            [set-mcdr! set-cdr!]
            [mpair? pair?]
            [mmap map]
            [mfor-each for-each])
           = < > <= >= max min + - * / 
           abs gcd lcm exp log sin cos tan not eq?
           call-with-current-continuation make-string
           symbol->string string->symbol make-rectangular 
           exact->inexact inexact->exact number->string string->number 
           rationalize output-port? current-input-port current-output-port current-error-port 
           open-input-file open-output-file close-input-port close-output-port
           with-output-to-file transcript-on transcript-off flush-output
           string-length string-ci<=? string-ci>=? string-append 
           string-fill!
           (rename-out [string->mlist string->list]
                       [mlist->string list->string])
           vector-length vector-fill!
           (rename-out [vector->mlist vector->list]
                       [mlist->vector list->vector])
           char-alphabetic? char-numeric? char-whitespace? 
           char-upper-case? char-lower-case? char->integer integer->char char-downcase
           call-with-output-file call-with-input-file with-input-from-file
           (rename-out [mapply apply]) symbol?
           null?
           (rename-out [mlist? list?]
                       [mlist list]
                       [mlength length]
                       [mappend append]
                       [mreverse reverse]
                       [mlist-tail list-tail]
                       [mlist-ref list-ref]
                       [mmemq memq]
                       [mmemv memv]
                       [mmember member]
                       [massq assq]
                       [massv assv]
                       [massoc assoc])
           procedure?
           number? complex? real? rational? integer? exact? inexact? zero?
           positive? negative? odd? even? 
           quotient remainder modulo floor ceiling truncate round 
           numerator denominator asin acos atan sqrt
           expt make-polar real-part imag-part angle magnitude input-port?
           (rename-out [mread read])
           read-char peek-char eof-object?
           char-ready? 
           (rename-out [mwrite write]
                       [mdisplay display])
           newline write-char load 
           string? string string-ref string-set! string=? substring string-copy
           string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
           vector? make-vector vector vector-ref vector-set! 
           char? char=? char<? char>? char<=? char>=? 
           char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
           char-upcase boolean? eqv? equal? force
           call-with-values values dynamic-wind
           (rename-out [meval eval])
           scheme-report-environment null-environment interaction-environment)

  ;; Because mcar and mcdr are inlined by the JIT
  (define-syntax provide-inlined-combo
    (syntax-rules ()
      [(_ id orig mc1r mc2r)
       (begin
         (define-syntax id
           (syntax-id-rules (set!)
             [(_ x) (mc1r (mc2r x))]
             [(set! _ v) (set! orig v)]
             [_ (lambda (x) (mc1r (mc2r x)))]))
         (provide (rename-out [id orig])))]))

  (provide-inlined-combo mcaar caar mcar mcar)
  (provide-inlined-combo mcadr cadr mcar mcdr)
  (provide-inlined-combo mcdar cdar mcdr mcar)
  (provide-inlined-combo mcddr cddr mcdr mcdr)

  (define-syntax (provide-combination stx)
    (syntax-case stx ()
      [(_ id)
       (with-syntax ([body
                      (let loop ([ops (let ([s (symbol->string (syntax-e #'id))])
                                        (string->list (substring s 1 (sub1 (string-length s)))))])
                        (if (null? ops)
                            'x
                            `(,(if (equal? (car ops) #\a) 'mcar 'mcdr)
                              ,(loop (cdr ops)))))]
                     [mid (datum->syntax #'id
                                         (string->symbol (format "m~a" (syntax-e #'id)))
                                         #'id)])
         #'(begin
             (define mid (lambda (x) body))
             (provide (rename-out [mid id]))))]
      [(_ id ...) #'(begin (provide-combination id) ...)]))

  (provide-combination caaar caadr cadar caddr cdaar cdadr cddar cdddr
                       caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                       cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)

  (define mmap
    (case-lambda
     [(f l) (let loop ([l l])
              (cond
               [(null? l) null]
               [else (mcons (f (mcar l)) (loop (mcdr l)))]))]
     [(f l1 l2) (let loop ([l1 l1][l2 l2])
                  (cond
                   [(null? l1) null]
                   [else (mcons (f (mcar l1) (mcar l2))
                                (loop (mcdr l1) (mcdr l2)))]))]
     [(f l . ls) (let loop ([l l][ls ls])
                   (cond
                    [(null? l) null]
                    [else (mcons (apply f (mcar l) (map mcar ls))
                                 (loop (mcdr l) (map mcdr ls)))]))]))

  (define mfor-each
    (case-lambda
     [(f l) (let loop ([l l])
              (cond
               [(null? l) (void)]
               [else (f (mcar l))
                     (loop (mcdr l))]))]
     [(f l1 l2) (let loop ([l1 l1][l2 l2])
                  (cond
                   [(null? l1) (void)]
                   [else (f (mcar l1) (mcar l2))
                         (loop (mcdr l1) (mcdr l2))]))]
     [(f l . ls) (let loop ([l l][ls ls])
                   (cond
                    [(null? l) (void)]
                    [else (apply f (mcar l) (map mcar ls))
                          (loop (mcdr l) (map mcdr ls))]))]))

  (define (list->mlist l)
    (cond
     [(null? l) null]
     [else (mcons (car l) (list->mlist (cdr l)))]))

  (define (mlist->list l)
    (cond
     [(null? l) null]
     [else (cons (mcar l) (mlist->list (mcdr l)))]))

  (define (string->mlist s) (list->mlist (string->list s)))
  (define (mlist->string s) (list->string (mlist->list s)))

  (define (vector->mlist s) (list->mlist (vector->list s)))
  (define (mlist->vector s) (list->vector (mlist->list s)))

  (define mapply
    (case-lambda
     [(f l) (apply f (mlist->list l))]
     [(f l0 . l)
      (apply f (let loop ([l (cons l0 l)])
                 (if (null? (cdr l))
                     (mlist->list (car l))
                     (cons (car l) (loop (cdr l))))))]))

  (define mlist
    (case-lambda
     [() null]
     [(a) (mcons a null)]
     [(a b) (mcons a (mcons b null))]
     [(a b c) (mcons a (mcons b (mcons c null)))]
     [(a b c d) (mcons a (mcons b (mcons c (mcons d null))))]
     [l (list->mlist l)]))

  (define (mlist? l)
    (cond
     [(null? l) #t]
     [(mpair? l) (mlist? (mcdr l))]
     [else #f]))

  (define (mlength l)
    (let loop ([l l][len 0])
      (cond
       [(null? l) len]
       [else (loop (mcdr l) (add1 len))])))

  (define mappend
    (case-lambda
     [() null]
     [(a) a]
     [(a b) (let loop ([a a])
              (if (null? a)
                  b
                  (mcons (mcar a) (loop (mcdr a)))))]
     [(a . l) (mappend a (apply mappend l))]))

  (define (mreverse l)
    (let loop ([l l][a null])
      (cond
       [(null? l) a]
       [else (loop (mcdr l) (mcons (mcar l) a))])))
  
  (define (mlist-tail l n)
    (cond
     [(zero? n) l]
     [else (mlist-tail (mcdr l) (sub1 n))]))
  
  (define (mlist-ref l n)
    (cond
     [(zero? n) (mcar l)]
     [else (mlist-ref (mcdr l) (sub1 n))]))

  (define (do-member =? v l)
    (let loop ([l l])
      (cond
       [(null? l) #f]
       [(=? v (mcar l)) l]
       [else (loop (mcdr l))])))

  (define (mmemq v l)
    (do-member eq? v l))

  (define (mmemv v l)
    (do-member eqv? v l))

  (define (mmember v l)
    (do-member equal? v l))

  
  (define (do-assoc =? v l)
    (let loop ([l l])
      (cond
       [(null? l) #f]
       [(=? v (mcar (mcar l))) (mcar l)]
       [else (loop (mcdr l))])))

  (define (massq v l)
    (do-assoc eq? v l))

  (define (massv v l)
    (do-assoc eqv? v l))

  (define (massoc v l)
    (do-assoc equal? v l))

  ;; --------------------------------------------------

  (define mread
    (case-lambda
     [() (mread (current-input-port))]
     [(port) (let loop ([v (read port)])
               (cond
                [(pair? v) (mcons (loop (car v)) (loop (cdr v)))]
                [(vector? v) (list->vector
                              (map loop (vector->list v)))]
                [else v]))]))

  (define mwrite
    (case-lambda
     [(v) (mwrite v (current-output-port))]
     [(v port) (parameterize ([print-mpair-curly-braces #f])
                 (write v port))]))

  (define mdisplay
    (case-lambda
     [(v) (mdisplay v (current-output-port))]
     [(v port) (parameterize ([print-mpair-curly-braces #f])
                 (display v port))]))

  ;; --------------------------------------------------

  (define-syntax (r5rs:quote stx)
    (syntax-case stx ()
      [(_ form)
       ;; Look for quoted pairs:
       (if (let loop ([form #'form])
             (syntax-case form ()
               [(a . b) #t]
               [#(a ...)
                (ormap loop (syntax->list #'(a ...)))]
               [_ #f]))
           ;; quote has to create mpairs:
           (syntax-local-lift-expression (let loop ([form #'form])
                                           (syntax-case form ()
                                             [(a . b)
                                              #`(mcons #,(loop #'a) #,(loop #'b))]
                                             [#(a ...)
                                              #`(vector . #,(map loop (syntax->list #'(a ...))))]
                                             [other #'(quote other)])))
           ;; no pairs to worry about:
           #'(quote form))]))

   (define-syntax (r5rs:quasiquote stx)
     (syntax-case stx ()
       [(_ form)
        ;; Look for unquote or unquote-splicing.
        ;; This should be improved to discount unquote[-splicing]
        ;; under a nested quasiquote.
        (if (let loop ([form #'form])
              (syntax-case form (unquote unquote-splicing)
                [unquote #t]
                [unquote-splicing #t]
                [(a . b) (or (loop #'a) (loop #'b))]
                [#(a ...)
                 (ormap loop (syntax->list #'(a ...)))]
                [_ #f]))
            ;; Found an unquote[-splicing], so convert:
            (let loop ([form #'form][depth 0])
              (syntax-case form (unquote unquote-splicing r5rs:quasiquote)
                [(unquote e) 
                 (if (zero? depth)
                     #'e
                     #`(mcons 'unquote
                              (mcons
                               #,(loop #'e (sub1 depth))
                               null)))]
                [unquote
                 (zero? depth)
                 (raise-syntax-error
                  "invalid content within quasiquote"
                  stx
                  form)]
                [((unquote-splicing e) . rest)
                 (if (zero? depth)
                     #`(mappend e #,(loop #'rest depth))
                     #`(mcons (mcons 'unquote-splicing
                                     (mcons #,(loop #'e (sub1 depth))
                                            null))
                              #,(loop #'rest depth)))]
                [unquote-splicing 
                 (zero? depth)
                 (raise-syntax-error
                  "invalid content within quasiquote"
                  stx
                  form)]
                [(r5rs:quasiquote . e)
                 #`(mcons 'quasiquote #,(loop #'e (add1 depth)))]
                [(a . b)
                 #`(mcons #,(loop #'a depth) #,(loop #'b depth))]
                [#(a ...)
                 #`(vector . #,(map (lambda (e) (loop e depth))
                                    (syntax->list #'(a ...))))]
                [other #'(r5rs:quote other)]))
            ;; None, so just use R5RS quote:
            #'(r5rs:quote form))]))

  ;; Copied from R5rS, but with an added `let' around body,
  ;; and with optimization for precedure letrecs
  (define undefined (letrec ([u u]) u))
  (define-syntax r5rs:letrec
    (syntax-rules (r5rs-lambda)
      ((r5rs:letrec ((var1 (r5rs-lambda . _rest)) ...) body ...)
       (letrec ((var1 (r5rs-lambda . _rest)) ...) body ...))
      ((r5rs:letrec ((var1 init1) ...) body ...)
       (r5rs:letrec "generate_temp_names"
	 (var1 ...)
	 ()
	 ((var1 init1) ...)
	 body ...))
      ((r5rs:letrec "generate_temp_names"
	 ()
	 (temp1 ...)
	 ((var1 init1) ...)
	 body ...)
       (let ((var1 undefined) ...)
	 (let ((temp1 init1) ...)
	   (set! var1 temp1)
	   ...
	   (let ()
	     body ...))))
      ((r5rs:letrec "generate_temp_names"
	 (x y ...)
	 (temp ...)
	 ((var1 init1) ...)
	 body ...)
       (r5rs:letrec "generate_temp_names"
	 (y ...)
	 (newtemp temp ...)
	 ((var1 init1) ...)
	 body ...))))

  (define-syntax r5rs:lambda
    ;; Convert rest-arg list to mlist:
    (syntax-rules ()
      [(_ (id ...) . body)
       (#%plain-lambda (id ...) . body)]
      [(_ (id ... . rest) . body)
       (#%plain-lambda (id ... . rest)
                       (let ([rest (list->mlist rest)])
                         . body))]))

  (define-syntax (r5rs:define stx)
    ;; Use r5rs:lambda
    (syntax-case stx ()
      [(_ (id . args) . body)
       (with-syntax ([proc
                      (syntax/loc stx
                        (r5rs:lambda args . body))])
         (syntax/loc stx
           (define id proc)))]
      [(_ . rest)
       (syntax/loc stx
         (define . rest))]))

  (define-syntax r5rs:if
    (syntax-rules ()
      [(_ test then)
       (if test then (void))]
      [(_ test then else)
       (if test then else)]))

  (provide unquote unquote-splicing 
	   (rename-out [r5rs:quote quote]
                       [r5rs:quasiquote quasiquote]
                       [r5rs:if if]
                       [r5rs:lambda lambda]
                       [r5rs:letrec letrec]
                       [r5rs:define define])
           let and or cond case delay do
	   let* begin set!
	   define-syntax let-syntax letrec-syntax
           => else

	   ;; We have to include the following MzScheme-isms to do anything,
	   ;; but they're not legal R5RS names, anyway.
           (rename-out [#%plain-module-begin #%module-begin])
	   #%app #%datum #%top #%top-interaction 
           #%require #%provide)
  
  ;; --------------------------------------------------

  (define-reflection-anchor here)

  (define (scheme-report-environment n)
    (unless (= n 5)
      (raise-type-error 'scheme-report-environment "5" n))
    (mk-r5rs #f))

  (define (null-environment n)
    (unless (= n 5)
      (raise-type-error 'null-environment "5" n))
    (mk-r5rs #t))

  (define (mk-r5rs stx-only?)
    (let ([n (make-namespace)]
          [orig (reflection-anchor->namespace here)])
      (parameterize ([current-namespace n])
	(namespace-attach-module orig 'r5rs)
        (if stx-only?
            (namespace-require '(only r5rs
                                      quote quasiquote
                                      if lambda letrec
                                      let and or cond case define delay do
                                      let* begin set!
                                      define-syntax let-syntax letrec-syntax
                                      => else
                                      #%app #%datum #%top #%top-interaction
                                      #%require #%provide))
            (begin
              (namespace-require 'r5rs) ; for syntax
              (namespace-require/copy 'r5rs))))
      n))

  (define (interaction-environment)
    (current-namespace))

  (define (meval expr env)
    (eval (let loop ([expr expr])
            (cond
             [(mpair? expr)
              (cons (loop (mcar expr))
                    (loop (mcdr expr)))]
             [(vector? expr)
              (list->vector (map loop (vector->list expr)))]
             [else expr]))
          env)))
