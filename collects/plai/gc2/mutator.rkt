#lang scheme
(require (prefix-in scheme: scheme)
         plai/private/command-line
         (for-syntax plai/private/command-line)
         plai/gc2/private/collector-exports
         plai/gc2/private/gc-core
         scheme/gui/dynamic
         (only-in plai/test-harness
                  exn:plai? equal~?
                  plai-error generic-test test halt-on-errors print-only-errors)
         (for-syntax scheme)
         (for-syntax plai/gc2/private/gc-transformer)
         scheme/stxparam
         (for-syntax scheme/stxparam-exptime))

(provide else require provide #%top
         test/location=? 
         test/value=?
         (rename-out
          [plai-error error]
          
          [mutator-and and]
          [mutator-or or]
          [mutator-cond cond]
          [mutator-case case]
          [mutator-define define]
          [mutator-define-values define-values]
          (mutator-let let)
          [mutator-let* let*]
          [mutator-begin begin]
          
          [mutator-if if]
          [mutator-let-values let-values]
          [mutator-set! set!]
          [mutator-lambda lambda]
          [mutator-lambda λ]
          (mutator-app #%app)
          (mutator-datum #%datum)
          (collector:cons cons)
          (collector:first first)
          (collector:rest rest)
          (mutator-quote quote)
          (mutator-top-interaction #%top-interaction)
          (mutator-module-begin #%module-begin)))

(define-syntax-parameter mutator-name #f)
(define-syntax-parameter mutator-tail-call? #t)
(define-syntax-parameter mutator-env-roots empty)

; Sugar Macros
(define-syntax-rule (->address e) e)
(define-syntax mutator-and
  (syntax-rules ()
    [(_) (mutator-quote #t)]
    [(_ fe) fe]
    [(_ fe e ...) (mutator-if fe (mutator-and e ...) (mutator-quote #f))]))
(define-syntax mutator-or
  (syntax-rules ()
    [(_) (mutator-quote #f)]
    [(_ fe) fe]
    [(_ fe e ...) (mutator-let ([tmp fe]) (mutator-if tmp tmp (mutator-or e ...)))]))
(define-syntax mutator-cond
  (syntax-rules (else)
    [(_) (mutator-begin)]
    [(_ [else e ...]) (mutator-begin e ...)]
    [(_ [q ans] e ...) (mutator-if q ans (mutator-cond e ...))]))
(define-syntax mutator-case
  (syntax-rules (else)
    [(_ value
        [(v ...) e ...]
        ...
        [else ee ...])
     (mutator-let ([tmp value])
                  (mutator-cond [(mutator-app mutator-member? tmp (mutator-quote (v ...)))
                                 e ...]
                                ...
                                [else ee ...]))]
    [(_ value
        [(v ...) e ...]
        ...)
     (mutator-case value
                   [(v ...) e ...]
                   ...
                   [else (mutator-begin)])]))
(define-syntax mutator-define
  (syntax-rules ()
    [(_ (f a ...) e ...)
     (mutator-define-values (f) 
                            (syntax-parameterize ([mutator-name #'f])
                                                 (mutator-lambda (a ...) e ...)))]
    [(_ id e)
     (mutator-define-values (id) 
                            (syntax-parameterize ([mutator-name #'id])
                                                 e))]))
(define-syntax-rule (mutator-let ([id e] ...) be ...)
  (mutator-let-values ([(id) (syntax-parameterize ([mutator-name #'id])
                                                  e)]
                       ...)
                      be ...))
(define-syntax mutator-let*
  (syntax-rules ()
    [(_ () be ...)
     (mutator-begin be ...)]
    [(_ ([fid fe] [rid re] ...) be ...)
     (mutator-let ([fid fe])
                  (mutator-let* ([rid re] ...)
                                be ...))]))
(define-syntax mutator-begin
  (syntax-rules ()
    [(_) (mutator-app void)]
    [(_ e) e]
    [(_ fe e ...)
     (let ([tmp 
            (syntax-parameterize ([mutator-tail-call? #f])
                                 fe)])
       (mutator-begin e ...))]))

; Real Macros
(define-syntax-rule (mutator-define-values (id ...) e)
  (begin (define-values (id ...) 
           (syntax-parameterize ([mutator-tail-call? #f])
                                (->address e)))
         (add-global-root! (make-env-root id))
         ...))
(define-syntax-rule (mutator-if test true false)
  (if (syntax-parameterize ([mutator-tail-call? #f])
                           (collector:deref (->address test)))
      (->address true)
      (->address false)))
(define-syntax-rule (mutator-set! id e)
  (begin
    (set! id (->address e))
    (mutator-app void)))
(define-syntax (mutator-let-values stx)
  (syntax-case stx ()
    [(_ ([(id ...) expr]
         ...)
        body-expr)
     (with-syntax ([((tmp ...) ...)
                    (map generate-temporaries (syntax->list #'((id ...) ...)))])
       (let ([binding-list (syntax->list #'((tmp ...) ...))])
         (with-syntax ([((previous-tmp ...) ...)
                        (build-list (length binding-list) 
                                    (λ (n) (append-map syntax->list (take binding-list n))))])
           (syntax/loc stx
             (let*-values ([(tmp ...) 
                            (syntax-parameterize ([mutator-env-roots 
                                                   (append
                                                    (find-referenced-locals
                                                     (list #'previous-tmp ...)
                                                     #'expr)
                                                    (syntax-parameter-value #'mutator-env-roots))]
                                                  [mutator-tail-call? #f])
                                                 expr)]
                           ...)
               (let-values ([(id ...) (values tmp ...)]
                            ...)
                 (syntax-parameterize ([mutator-env-roots 
                                        (append (find-referenced-locals
                                                 (list #'id ... ...)
                                                 #'body-expr)
                                                (syntax-parameter-value #'mutator-env-roots))])
                                      (->address body-expr))))))))]
    [(_ ([(id ...) expr]
         ...)
        body-expr ...)
     (syntax/loc stx
       (mutator-let-values
        ([(id ...) expr]
         ...)
        (mutator-begin body-expr ...)))]))
(define-syntax (mutator-lambda stx)
  (syntax-case stx ()
    [(_ (id ...) body)
     (let ([env-roots (syntax-parameter-value #'mutator-env-roots)])
       (with-syntax ([(free-id ...) (map syntax-local-introduce (find-referenced-locals env-roots stx))]
                     [(env-id ...) env-roots]
                     [closure (or (syntax-parameter-value #'mutator-name)
                                  (syntax-local-name)
                                  (let ([prop (syntax-property stx 'inferred-name)])
                                    (if (or (identifier? prop)
                                            (symbol? prop))
                                        prop
                                        #f))
                                  (string->symbol "#<proc>"))])
         (quasisyntax/loc stx
           (let ([closure 
                  (closure-code
                   #,(length (syntax->list #'(free-id ...)))
                   (let ([closure
                          (lambda (free-id ... id ...) 
                            (syntax-parameterize ([mutator-env-roots 
                                                   (append
                                                    (find-referenced-locals
                                                     (list #'id ...)
                                                     #'body)
                                                    (list #'free-id ...))]
                                                  [mutator-tail-call? #t])
                                                 (->address body)))])
                     closure))])
             #,(if (syntax-parameter-value #'mutator-tail-call?)
                   (syntax/loc stx
                     (#%app collector:closure closure (vector free-id ...)))
                   (syntax/loc stx
                     (with-continuation-mark 
                      gc-roots-key 
                      (list (make-env-root env-id) ...)
                      (#%app collector:closure closure (vector free-id ...)))))))))]
    [(_ (id ...) body ...)
     (syntax/loc stx
       (mutator-lambda (id ...) (mutator-begin body ...)))]))

(define-syntax (mutator-app stx)
  (syntax-case stx ()
    [(_ e ...)
     (local [(define (do-not-expand? exp)
               (and (identifier? exp)
                    (or (free-identifier=? exp #'empty)
                        (ormap (λ (x) (free-identifier=? x exp))
                               prim-ids))))
             (define exps (syntax->list #'(e ...)))
             (define tmps
               (generate-temporaries #'(e ...)))]
       (with-syntax ([(ne ...)
                      (map (lambda (exp tmp) (if (do-not-expand? exp) exp tmp))
                           exps tmps)])
         (for/fold ([acc (syntax/loc stx (mutator-anf-app ne ...))])
           ([exp (in-list (reverse exps))]
            [tmp (in-list (reverse tmps))])
           (if (do-not-expand? exp)
               acc
               (quasisyntax/loc stx
                 (mutator-let ([#,tmp #,exp])
                              #,acc))))))]))
(define-syntax (mutator-anf-app stx)
  (syntax-case stx ()
    [(_ fe ae ...)
     (let ()
       (define prim-app? (ormap (λ (x) (free-identifier=? x #'fe))
                                prim-ids))
       (with-syntax ([(env-id ...) (syntax-parameter-value #'mutator-env-roots)]
                     [app-exp (if prim-app?
                                  (syntax/loc stx (collector:alloc-flat (fe (collector:deref ae) ...)))
                                  (syntax/loc stx ((deref-proc fe) ae ...)))])
         (if (syntax-parameter-value #'mutator-tail-call?)
             ; If this call is in tail position, we will not need access
             ; to its environment when it returns.
             #'app-exp
             ; If this call is not in tail position, we make the
             ; environment at the call site reachable.
             #`(with-continuation-mark gc-roots-key 
                 (list (make-env-root env-id) ...)
                 app-exp))))]))
(define-syntax mutator-quote
  (syntax-rules ()
    [(_ (a . d))
     (mutator-app collector:cons (mutator-quote a) (mutator-quote d))]
    [(_ s) 
     (mutator-datum . s)]))
(define-syntax (mutator-datum stx)
  (syntax-case stx ()
    [(_ . e) 
     (quasisyntax/loc stx (mutator-anf-app collector:alloc-flat (#%datum . e)))]))

(define-syntax (mutator-top-interaction stx)
  (syntax-case stx (require provide mutator-define mutator-define-values test/value=? import-primitives)
    [(_ . (require . e))
     (syntax/loc stx
       (require . e))]
    [(_ . (provide . e))
     (syntax/loc stx
       (provide . e))]
    [(_ . (mutator-define . e))
     (syntax/loc stx
       (mutator-define . e))]
    [(_ . (mutator-define-values . e))
     (syntax/loc stx
       (mutator-define-values . e))]
    [(_ . (test/value=? . e))
     (syntax/loc stx
       (test/value=? . e))]
    [(_ . (import-primitives . e))
     (syntax/loc stx
       (import-primitives . e))]
    [(_ . expr)
     (syntax/loc stx
       (call-with-values
        (lambda ()
          (syntax-parameterize ([mutator-tail-call? #f])
                               (->address expr)))
        (case-lambda
          [() (void)]
          [(result-addr)
           (cond
             [(procedure? result-addr)
              (printf "Imported procedure:\n")
              result-addr]
             [(location? result-addr)
              (printf "Value at location ~a:\n" result-addr)
              (gc->scheme result-addr)])])))]))

; Module Begin
(define-for-syntax (allocator-setup-internal stx)
  (syntax-case stx ()
    [(collector-module heap-size)
     (with-syntax ([(init-allocator gc:deref gc:alloc-flat gc:cons 
                                    gc:closure gc:closure? gc:closure-code-ptr gc:closure-env-ref
                                    gc:first gc:rest 
                                    gc:flat? gc:cons?
                                    gc:set-first! gc:set-rest!)
                    (map (λ (s) (datum->syntax stx s))
                         '(init-allocator gc:deref gc:alloc-flat gc:cons 
                                          gc:closure gc:closure? gc:closure-code-ptr gc:closure-env-ref
                                          gc:first gc:rest 
                                          gc:flat? gc:cons?
                                          gc:set-first! gc:set-rest!))]) 
       (begin
         #`(begin
             #,(if (alternate-collector)
                   #`(require #,(datum->syntax #'collector-module (alternate-collector)))
                   #`(require collector-module))
             
             (set-collector:deref! gc:deref)
             (set-collector:alloc-flat! gc:alloc-flat)
             (set-collector:cons! gc:cons)
             (set-collector:first! gc:first)
             (set-collector:rest! gc:rest)
             (set-collector:flat?! gc:flat?)
             (set-collector:cons?! gc:cons?)
             (set-collector:set-first!! gc:set-first!)
             (set-collector:set-rest!! gc:set-rest!)
             (set-collector:closure! gc:closure)
             (set-collector:closure?! gc:closure?)
             (set-collector:closure-code-ptr! gc:closure-code-ptr)
             (set-collector:closure-env-ref! gc:closure-env-ref)
             
             (init-heap! (#%datum . heap-size))
             (when (gui-available?) 
               (if (<= (#%datum . heap-size) 500)
                   (set-ui! (dynamic-require `plai/gc2/private/gc-gui 'heap-viz%))
                   (printf "Large heap; the heap visualizer will not be displayed.\n")))
             (init-allocator))))]
    [_ (raise-syntax-error 'mutator 
                           "Mutator must start with an 'allocator-setup' expression, such as: (allocator-setup <module-path> <literal-number>)"
                           stx)]))

(define-for-syntax allocator-setup-error-msg
  "Mutator must start with an 'allocator-setup' expression, such as: (allocator-setup <module-path> <literal-number>)")

(define-syntax (mutator-module-begin stx)
  (syntax-case stx (allocator-setup)
    [(_ (allocator-setup . setup) module-expr ...)
     (begin
       (syntax-case #'setup ()
         [(collector heap-size)
          (begin
            (unless (module-path? (syntax->datum #'collector))
              (raise-syntax-error 'allocator-setup "expected a module path" #'collector))
            (unless (number? (syntax->datum #'heap-size))
              (raise-syntax-error 'allocator-setup "expected a literal number" #'heap-size)))]
         [_
          (raise-syntax-error 'mutator allocator-setup-error-msg (syntax/loc #'setup (allocator-setup . setup)))])
       (quasisyntax/loc stx
         (#%module-begin
          #,(allocator-setup-internal #'setup)
          #,@(for/list ([me (in-list (syntax->list #'(module-expr ...)))])
               (quasisyntax/loc me
                 (mutator-top-interaction . #,me))))))]
    [(_ first-expr module-expr ...)
     (raise-syntax-error 'mutator allocator-setup-error-msg #'first-expr)]
    [(_)
     (raise-syntax-error 'mutator allocator-setup-error-msg)]))

; User Macros
(provide import-primitives)
(define-syntax (import-primitives stx)
  (syntax-case stx ()
    [(_ id ...) 
     (andmap identifier? (syntax->list #'(id ...)))
     (with-syntax ([(renamed-id ...) (generate-temporaries #'(id ...))]
                   [source (syntax-local-get-shadower
                            (syntax-local-introduce #'scheme))])
       #`(begin
           (require (only-in source [id renamed-id] ...))
           ;; XXX make a macro to unify this and provide/lift
           (define id
             (lambda args
               (unless (andmap (lambda (v) (and (location? v) (collector:flat? v))) args)
                 (error 'id (string-append "all arguments must be <heap-value?>s, "
                                           "even if the imported procedure accepts structured "
                                           "data")))
               (let ([result (apply renamed-id (map collector:deref args))])
                 (cond
                   [(void? result) (void)]
                   [(heap-value? result) (collector:alloc-flat result)]
                   [else 
                    (error 'id (string-append "imported primitive must return <heap-value?>, "
                                              "received ~a" result))]))))
           ...))]
    [(_ maybe-id ...) 
     (ormap (λ (v) (and (not (identifier? v)) v)) (syntax->list #'(maybe-id ...)))
     (let ([offending-stx (findf (λ (v) (not (identifier? v))) (syntax->list #'(maybe-id ...)))])
       (raise-syntax-error 
        #f "expected identifier to import" offending-stx))]
    [(_ . __)
     (raise-syntax-error #f "expected list of identifiers to import" stx)]
    [_ (raise-syntax-error #f "expected open parenthesis before import-primitive")]))

(define-for-syntax ((mk-id-macro p-id) stx)
  (syntax-case stx ()
    [id
     (identifier? #'id)
     (raise-syntax-error (syntax-e stx)
                         "primitive must appear in the function position of an application"
                         stx)]
    [(id exp ...)
     #`(mutator-app #,p-id exp ...)]))
    
(define-syntax (provide-flat-prims/lift stx)
  (syntax-case stx ()
    [(_ prim-ids id ...)
     (andmap identifier? (syntax->list #'(id ...)))
     (with-syntax ([(id2 ...) (generate-temporaries #'(id ...))]
                   [(p ...) (generate-temporaries #'(id ...))])
       #'(begin
           (define-for-syntax prim-ids (syntax->list #'(id ...)))
           (provide (rename-out [id2 id] ...))
           (define-syntax id2 (mk-id-macro #'id)) ...))]))

(provide-flat-prims/lift
 prim-ids
 symbol? boolean? number? symbol=?
 add1 sub1 zero? + - * / even? odd? = < > <= >=)

(define (member? v l)
  (and (member v l) #t))
(define (mutator-member? v l)
  (collector:alloc-flat
   (member? (collector:deref v)
            (gc->scheme l))))

(provide (rename-out (mutator-set-first! set-first!)))
(define (mutator-set-first! x y)
  (collector:set-first! x y)
  (void))

(provide (rename-out (mutator-set-rest! set-rest!)))
(define (mutator-set-rest! x y)
  (collector:set-rest! x y)
  (void))

(provide (rename-out [mutator-empty empty]))
(define-syntax mutator-empty
  (syntax-id-rules (mutator-empty)
    [_ (mutator-quote ())]))

(provide (rename-out (mutator-empty? empty?)))
(define (mutator-empty? loc)
  (cond
    [(collector:flat? loc) 
     (collector:alloc-flat (empty? (collector:deref loc)))]
    [else 
     (collector:alloc-flat false)]))

(provide (rename-out [mutator-cons? cons?]))
(define (mutator-cons? loc)
  (collector:alloc-flat (collector:cons? loc)))

(provide (rename-out [mutator-eq? eq?]))
(define (mutator-eq? l1 l2)
  (collector:alloc-flat (= l1 l2)))

(provide (rename-out [mutator-printf printf]))
(define-syntax (mutator-printf stx)
  (syntax-case stx ()
    [(_ fmt arg ...)
     ; We must invoke mutator-app to A-normalize the arguments.
     (syntax/loc stx 
       (begin
         (mutator-app printf (#%datum . fmt)
                      (mutator-app gc->scheme arg) ...)
         (void)))]))

(provide (rename-out
          (mutator-halt-on-errors halt-on-errors)
          (mutator-print-only-errors print-only-errors)))
(define-syntax (mutator-halt-on-errors stx)
  (syntax-case stx ()
    [(_) #'(halt-on-errors)]
    [(_ arg) #'(#%app halt-on-errors (#%datum . arg))]))

(define-syntax (mutator-print-only-errors stx)
  (syntax-case stx ()
    [(_) #'(print-only-errors)]
    [(_ arg) #'(#%app print-only-errors (#%datum . arg))]))

; Implementation Functions
(define (deref-proc proc/loc)
  (define v
    (cond
      [(procedure? proc/loc) proc/loc]
      [(location? proc/loc) (collector:closure-code-ptr proc/loc)]
      [else 
       (error 'procedure-application "expected procedure, given something else")]))
  (cond
   [(procedure? v)
    v]
   [(closure-code? v)
    (lambda args
      (apply (closure-code-proc v) 
             (append 
              (for/list ([i (in-range (closure-code-env-count v))])
                        (collector:closure-env-ref proc/loc i))
              args)))]
   [else
    (error 'procedure-application "expected procedure, given ~e" v)]))

(define (gc->scheme loc)
  (define-struct an-unset ())
  (define unset (make-an-unset))
  (define phs (make-hash))
  (define (unwrap loc)
    (if (hash-has-key? phs loc)
        (hash-ref phs loc)
        (begin
          (local [(define ph (make-placeholder unset))]
            (hash-set! phs loc ph)
            (cond
              [(collector:flat? loc)
               (placeholder-set! ph (collector:deref loc))]
              [(collector:cons? loc)
               (local [(define car-ph (make-placeholder unset))
                       (define cdr-ph (make-placeholder unset))]
                 (placeholder-set! ph (cons car-ph cdr-ph))
                 (placeholder-set! car-ph (unwrap (collector:first loc)))
                 (placeholder-set! cdr-ph (unwrap (collector:rest loc))))]
              [(collector:closure? loc)
               ;; XXX get env?
               (placeholder-set! ph (closure-code-proc (collector:closure-code-ptr loc)))]
              [else 
               (error (format "gc:flat?, gc:cons?, gc:closure? all returned false for ~a" loc))])
            (placeholder-get ph)))))
  (make-reader-graph (unwrap loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing support

(define-syntax (test/location=? stx)
  (syntax-case stx ()
    [(_ e1 e2)
     (quasisyntax/loc stx
       (generic-test 
        (λ () e1) 
        (λ (result-value)
          (define expected-val e2)
          (values
           (cond
             [(exn:plai? result-value) result-value]
             [(equal~? result-value expected-val) true]
             [else false])
           expected-val))
        (quote (heap-loc #,(syntax->datum #'e1)))
        (format "at line ~a" #,(syntax-line stx))))]))

(define-for-syntax (flat-heap-value? v)
  (or (number? v) (boolean? v)))

(define-syntax (expand-scheme stx)
  (syntax-case stx (mutator-quote mutator-datum)
    [(_ val) (flat-heap-value? (syntax->datum #'val)) #'(#%datum . val)]
    [(_ (mutator-datum . val))
     #'(#%datum . val)]
    [(_ (mutator-quote e))
     #'(quote e)]
    [_ 
     (raise-syntax-error 'test/value=? "must be a number, boolean or a quoted value" stx)]))

(define-syntax (test/value=? stx)
  (syntax-case stx (mutator-quote)
    [(_ mutator-expr scheme-datum)
     (quasisyntax/loc stx
       (generic-test 
        (λ () 
          (mutator-let ([v1 mutator-expr])
                       (gc->scheme v1))) 
        (λ (result-value)
          (define expected-val (expand-scheme scheme-datum))
          (values
           (cond
             [(exn:plai? result-value) result-value]
             [(equal~? result-value expected-val) true]
             [else false])
           expected-val))
        (quote #,(syntax->datum #'mutator-expr))
        (format "at line ~a" #,(syntax-line stx))))]))
