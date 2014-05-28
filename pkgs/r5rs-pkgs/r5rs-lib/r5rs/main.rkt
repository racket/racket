(module main scheme/base
  (require scheme/mpair
           racket/undefined
           (for-syntax scheme/base syntax/kerncase
                       "private/r5rs-trans.rkt")
           (only-in mzscheme transcript-on transcript-off))

  (provide (for-syntax syntax-rules ...
                       (rename-out [syntax-rules-only #%top]
                                   [syntax-rules-only #%app]
                                   [syntax-rules-only #%datum]))
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
           char-upcase boolean? eqv? equal? 
           (rename-out [r5rs:force force])
           call-with-values values dynamic-wind
           (rename-out [meval eval])
           scheme-report-environment null-environment interaction-environment)

  ;; Because mcar and mcdr are inlined by the JIT
  (define-syntax provide-inlined-combo
    (syntax-rules ()
      [(_ id orig mc1r mc2r)
       (begin
         (define proc-id
           (let ([id (lambda (x) (mc1r (mc2r x)))])
             id))
         (define-syntax id
           (syntax-id-rules (set!)
             [(_ x) (mc1r (mc2r x))]
             [(set! _ v) (set! orig v)]
             [(_ . args) (proc-id . args)]
             [_ proc-id]))
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

  (define (to-mutable v)
    (cond
     [(pair? v) (mcons (to-mutable (car v))
                       (to-mutable (cdr v)))]
     [(vector? v) (list->vector
                   (map to-mutable (vector->list v)))]
     [else v]))

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
           (syntax-local-lift-expression #'(to-mutable 'form))
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
                              #,(loop (cdr (syntax-e form)) (sub1 depth))))]
                [unquote
                 (zero? depth)
                 (raise-syntax-error
                  'unquote
                  "invalid context within quasiquote"
                  stx
                  form)]
                [((unquote-splicing e) . rest)
                 (if (zero? depth)
                     (if (null? (syntax-e #'rest))
                         #'e ;; Note: we're not check for a list
                         #`(mappend e #,(loop #'rest depth)))
                     #`(mcons (mcons 'unquote-splicing
                                     #,(loop #'(e) (sub1 depth)))
                              #,(loop #'rest depth)))]
                [unquote-splicing 
                 (zero? depth)
                 (raise-syntax-error
                  'unquote-splicing 
                  "invalid context within quasiquote"
                  stx
                  form)]
                [(r5rs:quasiquote . e)
                 #`(mcons 'quasiquote #,(loop #'e (add1 depth)))]
                [(a . b)
                 #`(mcons #,(loop #'a depth) #,(loop #'b depth))]
                [#(unquote a ...)
                 (raise-syntax-error
                  'unquote
                  "invalid context within quasiquote"
                  stx
                  form)]
                [#(a ...)
                 #`(mlist->vector #,(loop (syntax->list #'(a ...)) depth))]
                [other #'(r5rs:quote other)]))
            ;; None, so just use R5RS quote:
            #'(r5rs:quote form))]))

  ;; The difference between R5RS `letrec` and Racket `letrec` (or
  ;; R6RS `letrec*`) is that all right-hand sides are evaluated
  ;; before any binding is initialized. We can get this effect
  ;; by using `letrec-values` and a single clause:
  (define-syntax (r5rs:letrec stx)
    (syntax-case stx (r5rs:lambda)
      ((r5rs:letrec ((var1 init1) ...) body ...)
       (syntax/loc stx
         (letrec-values ([(var1 ...)
                          (values init1 ...)])
           (r5rs:body
            body ...))))))

  (define-syntax (r5rs:lambda stx)
    ;; Convert rest-arg list to mlist, and use r5rs:body:
    (syntax-case stx ()
      [(_ (id ...) . body)
       (syntax/loc stx (#%plain-lambda (id ...) (r5rs:body . body)))]
      [(_ (id ... . rest) . body)
       (syntax/loc stx
         (#%plain-lambda (id ... . rest)
                         (let ([rest (list->mlist rest)])
                           (r5rs:body . body))))]))

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

  (define-syntax (r5rs:define-syntax stx)
    ;; Disallow in internal-definition contexts:
    (when (pair? (syntax-local-context))
      (raise-syntax-error
       #f
       "disallowed as an internal definition"
       stx))
    (syntax-case stx ()
      [(_ id expr)
       (identifier? #'id)
       (syntax/loc stx
         (define-syntax id expr))]))

  (define-syntax r5rs:if
    (syntax-rules ()
      [(_ test then)
       (if test then (void))]
      [(_ test then else)
       (if test then else)]))

  ;; Essentially from Dybvig:
  (define-syntax r5rs:delay
    (lambda (x)
      (syntax-case x ()
	((delay exp)
	 (syntax/loc x (make-promise (lambda () exp)))))))
  
  (define-struct promise (p) #:mutable)
  
  (define (r5rs:force p)
    (unless (promise? p)
      (raise-type-error 'force "promise" p))
    (let ([v (promise-p p)])
      (if (procedure? v)
	  (let ([v (call-with-values v list)])
	    (when (procedure? (promise-p p))
	      (set-promise-p! p v))
	    (apply values (promise-p p)))
	  (apply values v))))

  (define-syntax r5rs:let
    (syntax-rules ()
      [(_ (binding ...) . body)
       (let (binding ...) (r5rs:body . body))]
      [(_ id (binding ...) . body)
       (let id (binding ...) (r5rs:body . body))]))
  (define-syntax-rule (r5rs:let* bindings . body)
    (let* bindings (r5rs:body . body)))
  (define-syntax-rule (r5rs:let-syntax bindings . body)
    (let-syntax bindings (r5rs:body . body)))
  (define-syntax-rule (r5rs:letrec-syntax bindings . body)
    (letrec-syntax bindings (r5rs:body . body)))

  (define-syntax (r5rs:body stx)
    (syntax-case stx (let)
      [(_ (let () . body))
       #'(let () . body)]
      [_
       ;; Convert internal definitions to `r5rs:letrec', as opposed
       ;; to `letrec'.
       (let ([def-ctx (syntax-local-make-definition-context)]
             [ctx (list (gensym 'intdef))]
             [kernel-forms (kernel-form-identifier-list)]
             [init-exprs (let ([v (syntax->list stx)])
                           (unless v
                             (raise-syntax-error #f "bad syntax" stx))
                           (cdr v))])
         (with-syntax ([(exprs ((id ...) ...) (rhs ...) (stx-ids ...) (stx-rhs ...))
                        (let loop ([exprs init-exprs]
                                   [idss null][rhss null]
                                   [stx-idss null][stx-rhss null])
                          (if (null? exprs)
                              (raise-syntax-error
                               #f
                               "no expression in body"
                               stx)
                              (let ([expr (local-expand (car exprs) ctx kernel-forms def-ctx)])
                                (syntax-case expr (begin define-syntaxes define-values)
                                  [(begin . rest)
                                   (let ([l (syntax->list #'rest)])
                                     (if l
                                         (loop (append l (cdr exprs)) idss rhss stx-idss stx-rhss)
                                         (raise-syntax-error #f expr "bad syntax")))]
                                  [(define-syntaxes (id ...) rhs)
                                   (andmap identifier? (syntax->list #'(id ...)))
                                   (with-syntax ([rhs (local-transformer-expand
                                                       #'rhs
                                                       'expression
                                                       null)])
                                     (syntax-local-bind-syntaxes
                                      (syntax->list #'(id ...))
                                      #'rhs def-ctx)
                                     (loop (cdr exprs)
                                           idss rhss
                                           (cons #'(id ...) stx-idss)
                                           (cons #'rhs stx-rhss)))]
                                  [(define-values (id ...) rhs)
                                   (andmap identifier? (syntax->list #'(id ...)))
                                   (let ([ids (syntax->list #'(id ...))])
                                     (syntax-local-bind-syntaxes ids #f def-ctx)
                                     (loop (cdr exprs)
                                           (cons #'(id ...) idss)
                                           (cons #'rhs rhss)
                                           stx-idss stx-rhss))]
                                  [else 
                                   (list (cons expr
                                               (map (lambda (expr)
                                                      (local-expand expr ctx kernel-forms def-ctx))
                                                    (cdr exprs)))
                                         (reverse idss) (reverse rhss)
                                         (reverse stx-idss) (reverse stx-rhss))]))))])
           (internal-definition-context-seal def-ctx)
           (if (and (null? (syntax-e #'(stx-rhs ...)))
                    (andmap (lambda (ids)
                              (= 1 (length (syntax->list ids))))
                            (syntax->list #'((id ...) ...))))
               ;; This is the normal case: use `r5rs:letrec':
               #`(r5rs:letrec ([id ... rhs] ...)
                              (let () . exprs))
               ;; Unusual case: need to expand to `set!' manually:
               (with-syntax ([((tmp-id ...) ...)
                              (map (lambda (ids)
                                     (generate-temporaries ids))
                                   (syntax->list #'((id ...) ...)))])
                 #`(letrec-syntaxes+values 
                       ([stx-ids stx-rhs] ...)
                       ([(id ...) (values (mk-undefined id) ...)] ...)
                     (let-values ([(tmp-id ...) rhs] ...)
                       (begin (set! id tmp-id) ...) ...
                       . exprs))))))]))

  (define-syntax (r5rs:case stx)
    ;; Racket's `case' uses `equal?' and allows internal definitions,
    ;; this one uses `eqv?' and allows only expressions in clauses.
    (define (convert-case stx)
      (with-syntax ([(clause ...)
                     (map (lambda (clause)
                            (syntax-case clause ()
                              [[datums rhs ...]
                               (syntax/loc clause
                                 [datums (#%expression rhs) ...])]
                              [else 
                               ;; bad syntax
                               clause]))
                          (cddr (syntax->list stx)))]
                    [(_ expr . _) stx])
        (syntax/loc stx
          (case expr clause ...))))
    (define (eqv-is-equal-datum? e)
      (define v (syntax-e e))
      (or (null? v)
          (number? v)
          (char? v)
          (symbol? v)
          (boolean? v)))
    (syntax-case stx (else)
      [(_ expr [(datum ...) . _] ... [else . _])
       (andmap eqv-is-equal-datum? (syntax->list #'(datum ... ...)))
       ;; normal `case' with `else'
       (convert-case stx)]
      [(_ expr [(datum ...) . _] ...)
       ;; normal `case' without `else'
       (andmap eqv-is-equal-datum? (syntax->list #'(datum ... ...)))
       (convert-case stx)]
      [(_ expr [datums rhs ...] ...)
       ;; weird `case' clause
       (with-syntax ([(clause ...)
                      (map (lambda (clause)
                             (syntax-case clause (else)
                               [[else rhs ...]
                                (with-syntax ([(els . _) clause])
                                  (syntax/loc clause
                                    [els (#%expression rhs) ...]))]
                               [[datums rhs ...]
                                (syntax/loc clause
                                  [(memv v '(datums)) (#%expression rhs) ...])]))
                           (cddr (syntax->list stx)))])
         (syntax/loc stx
           (let ([v expr])
             (cond clause ...))))]
      [else
       ;; let `case' complain about syntax:
       (with-syntax ([(_ . rest) stx])
         (syntax/loc stx (case . rest)))]))

  (define-syntax (r5rs:cond stx)
    ;; Racket's `cond' allows internal definitions,
    ;; this one allows only expressions in clauses.
    (syntax-case stx ()
      [(_ clause ...)
       (with-syntax ([(new-clause ...)
                      (map (lambda (clause)
                             (syntax-case clause (else =>)
                               [[else rhs ...]
                                (with-syntax ([(els . _) clause])
                                  (syntax/loc clause
                                    [els (#%expression rhs) ...]))]
                               [[expr => rhs]
                                clause]
                               [[expr rhs ...]
                                (syntax/loc clause
                                  [expr (#%expression rhs) ...])]))
                           (syntax->list #'(clause ...)))])
         (syntax/loc stx
           (cond new-clause ...)))]
      [else
       ;; let `cond' complain about syntax:
       (with-syntax ([(_ . rest) stx])
         (syntax/loc stx (cond . rest)))]))


  (define-syntax-rule (mk-undefined id) undefined)
    
  (provide unquote unquote-splicing 
	   (rename-out [r5rs:quote quote]
                       [r5rs:quasiquote quasiquote]
                       [r5rs:if if]
                       [r5rs:lambda lambda]
                       [r5rs:letrec letrec]
                       [r5rs:define define]
                       [r5rs:define-syntax define-syntax]
                       [r5rs:delay delay]
                       [r5rs:let let]
                       [r5rs:let* let*]
                       [r5rs:let-syntax let-syntax]
                       [r5rs:letrec-syntax letrec-syntax]
                       [r5rs:case case]
                       [r5rs:cond cond])
           and or do
	   begin set!
           => else

	   ;; We have to include the following Racket-isms to do anything,
	   ;; but they're not legal R5RS names, anyway.
           (rename-out [#%plain-module-begin #%module-begin])
	   #%app #%datum #%top #%top-interaction 
           #%require #%provide #%expression)
  
  ;; --------------------------------------------------

  (define-namespace-anchor here)

  (define (scheme-report-environment n)
    (unless (= n 5)
      (raise-type-error 'scheme-report-environment "5" n))
    (mk-r5rs #f))

  (define (null-environment n)
    (unless (= n 5)
      (raise-type-error 'null-environment "5" n))
    (mk-r5rs #t))

  (define (mk-r5rs stx-only?)
    (let ([n (namespace-anchor->empty-namespace here)])
      (parameterize ([current-namespace n])
        (if stx-only?
            (namespace-require '(only r5rs
                                      quote quasiquote
                                      if lambda letrec
                                      let and or cond case define delay do
                                      let* begin set!
                                      define-syntax let-syntax letrec-syntax
                                      => else
                                      #%app #%datum #%top #%top-interaction
                                      #%require #%provide #%expression))
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
