#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "simple.rkt"
         "find-definition.rkt"
         "struct-type-info.rkt"
         "mutated-state.rkt"
         "find-known.rkt"
         "infer-known.rkt"
         "letrec.rkt"
         "id-to-var.rkt"
         "aim.rkt")

(provide mutated-in-body
         update-mutated-state!)

;; See "mutated-state.rkt" for information on the content of the
;; `mutated` table.

;; We don't have to worry about errors or escapes that prevent the
;; definition of an identifier, because that will abort the enclosing
;; linklet.

;; This pass is also responsible for recording when a letrec binding
;; must be mutated implicitly via `call/cc`.

(define (mutated-in-body l exports extra-variables prim-knowns knowns imports simples
                         unsafe-mode? target enforce-constant?)
  ;; Find all `set!`ed variables, and also record all bindings
  ;; that might be used too early
  (define mutated (make-hasheq))
  ;; Defined names start out as 'not-ready; start with `exports`,
  ;; because anything exported but not defined is implicitly in an
  ;; undefined state and must be accessed through a `variable`:
  (for ([id (in-hash-keys exports)])
    (hash-set! mutated id 'undefined))
  ;; Find all defined variables, and find variables that are not exported:
  (define unexported-ids
    (for/fold ([unexported-ids '()]) ([form (in-list l)])
      (match form
        [`(define-values (,ids ...) ,rhs)
         (for/fold ([unexported-ids unexported-ids]) ([id (in-list ids)])
           (define u-id (unwrap id))
           (hash-set! mutated u-id (if enforce-constant?
                                       'not-ready
                                       ;; If constants should not be enforced, then
                                       ;; treat all variable as mutated:
                                       'set!ed-too-early))
           (if (hash-ref exports u-id #f)
               unexported-ids
               (cons u-id unexported-ids)))]
        [`,_ unexported-ids])))
  ;; To support jitify, if an unexported and unmutated variable is
  ;; captured in a closure before it is defined, will want to reify
  ;; it like an export; so, set those variables to 'too-early
  ;; until they are really initialized
  (define unexported-ready (and (pair? unexported-ids)
                                (aim? target 'interp)
                                (make-hasheq)))
  (when unexported-ready
    (for ([id (in-list unexported-ids)])
      (hash-set! mutated id (lambda ()
                              (unless (or (hash-ref unexported-ready id #f)
                                          (set!ed-mutated-state? (hash-ref mutated id #f)))
                                (hash-set! mutated id 'too-early))))))
  ;; Walk through the body:
  (for/fold ([prev-knowns knowns]) ([form (in-list l)])
    ;; Accumulate known-binding information in this pass, because it's
    ;; helpful to know which variables are bound to constructors.
    ;; Note that we may tentatively classify a binding as a constructor
    ;; before discovering that its mutated via `set!`, but any use of
    ;; that information is correct, because it dynamically precedes
    ;; the `set!`
    (define-values (knowns info)
      (find-definitions form prim-knowns prev-knowns imports mutated simples unsafe-mode? target
                        #:optimize? #f))
    (match form
      [`(define-values (,ids ...) ,rhs)
       (cond
        [info
         ;; Look just at the "rest" part:
         (for ([e (in-list (struct-type-info-rest info))]
               [pos (in-naturals)])
           (define prop-vals (and (= pos struct-type-info-rest-properties-list-pos)
                                  (pure-properties-list e prim-knowns knowns imports mutated simples)))
           (cond
             [prop-vals
              ;; check individual property values using `ids`, so procedures won't
              ;; count as used until some instace is created
              (for ([e (in-list prop-vals)])
                (find-mutated! e ids prim-knowns knowns imports mutated simples unsafe-mode?))]
             [else
              (find-mutated! e ids prim-knowns knowns imports mutated simples unsafe-mode?)]))]
        [else
         (find-mutated! rhs ids prim-knowns knowns imports mutated simples unsafe-mode?)])
       ;; For any among `ids` that didn't get a delay and wasn't used
       ;; too early, the variable is now ready, so remove from
       ;; `mutated`
       (for ([id (in-list ids)])
         (let ([id (unwrap id)])
           (when (eq? 'not-ready (hash-ref mutated id #f))
             (hash-remove! mutated id))))]
      [`,_
       (find-mutated! form #f prim-knowns knowns imports mutated simples unsafe-mode?)])
    knowns)
  ;; For definitions that are not yet used, force delays:
  (for ([form (in-list l)])
    (match form
      [`(define-values (,ids ...) ,rhs)
       (for ([id (in-list ids)])
         (let ([id (unwrap id)])
           (define state (hash-ref mutated id #f))
           (when unexported-ready
             (when (not (hash-ref exports id #f))
               (hash-set! unexported-ready id #t)))
           (when (delayed-mutated-state? state)
             (hash-remove! mutated id)
             (state))))]
      [`,_ (void)]))
  ;; Check for unexported variables that need to be implemented like exports:
  (unless (or unsafe-mode?
              (aim? target 'system))
    (for ([id (in-list unexported-ids)])
      (define state (hash-ref mutated id #f))
      (when (via-variable-mutated-state? state)
        ;; force creation of variable
        (id-to-variable id exports extra-variables))))
  ;; Everything else in `mutated` is either 'set!ed, 'too-early,
  ;; 'undefined, or unreachable:
  mutated)

;; Schemify `let-values` to `let`, etc., and
;; reorganize struct bindings.
(define (find-mutated! top-v ids prim-knowns knowns imports mutated simples unsafe-mode?)
  (define (delay! ids thunk)
    (define done? #f)
    (define force (lambda () (unless done?
                               (set! done? #t)
                               (thunk))))
    (for ([id (in-list ids)])
      (let ([id (unwrap id)])
        (define m (hash-ref mutated id 'not-ready))
        (cond
          [(eq? 'not-ready m)
           (hash-set! mutated id force)]
          [(procedure? m)
           (hash-set! mutated id (lambda () (m) (force)))]
          [else
           (force)]))))
  (let find-mutated! ([v top-v] [ids ids])
    (define (find-mutated!* l ids)
      (let loop ([l l])
        (cond
         [(null? l) (void)]
         [(null? (cdr l)) (find-mutated! (car l) ids)]
         [else (find-mutated! (car l) #f) (loop (cdr l))])))
    (match v
      [`(lambda ,formals ,body ...)
       (if ids
           (delay! ids (lambda () (find-mutated!* body #f)))
           (find-mutated!* body #f))]
      [`(case-lambda [,formalss ,bodys ...] ...)
       (if ids
           (delay! ids (lambda () (for ([body (in-list bodys)]) (find-mutated!* body #f))))
           (for ([body (in-list bodys)]) (find-mutated!* body #f)))]
      [`(quote ,_) (void)]
      [`(let-values ([,idss ,rhss] ...) ,bodys ...)
       (for ([ids (in-list idss)]
             [rhs (in-list rhss)])
         ;; an `id` in `ids` can't be referenced too early,
         ;; but it might usefully be delayed
         (find-mutated! rhs ids))
       (find-mutated!* bodys ids)]
      [`(letrec-values ([,idss ,rhss] ...) ,bodys ...)
       (cond
         [(letrec-splitable-values-binding? idss rhss)
          (find-mutated! (letrec-split-values-binding idss rhss bodys) ids)]
         [else
          (for* ([ids (in-list idss)]
                 [id (in-wrap-list ids)])
            (hash-set! mutated (unwrap id) 'not-ready))
          (for/fold ([maybe-cc? #f]) ([ids (in-list idss)]
                                      [rhs (in-list rhss)])
            (find-mutated! rhs (unwrap-list ids))
            (define new-maybe-cc? (or maybe-cc?
                                      (not (simple? rhs prim-knowns knowns imports mutated simples unsafe-mode?
                                                    #:pure? #f
                                                    #:result-arity (length ids)))))
            ;; Each `id` in `ids` is now ready (but might also hold a delay):
            (for ([id (in-wrap-list ids)])
              (let ([u-id (unwrap id)])
                (define state (hash-ref mutated u-id))
                (define (add-too-early-name!)
                  (cond
                    [(and (eq? 'too-early state)
                          (wrap-property id 'undefined-error-name))
                     => (lambda (name)
                          (hash-set! mutated u-id (too-early name #f)))]
                    [(and (eq? 'set!ed-too-early state)
                          (wrap-property id 'undefined-error-name))
                     => (lambda (name)
                          (hash-set! mutated u-id (too-early name #t)))]))
                (cond
                  [new-maybe-cc?
                   (cond
                     [(or (eq? 'not-ready state)
                          (delayed-mutated-state? state))
                      (hash-set! mutated u-id 'implicitly-set!ed)]
                     [else (add-too-early-name!)])
                   (when (delayed-mutated-state? state)
                     (state))]
                  [(eq? 'not-ready state)
                   (hash-remove! mutated u-id)]
                  [else (add-too-early-name!)])))
            new-maybe-cc?)
          (find-mutated!* bodys ids)])]
      [`(if ,tst ,thn ,els)
       (find-mutated! tst #f)
       (find-mutated! thn #f)
       (find-mutated! els #f)]
      [`(with-continuation-mark ,key ,val ,body)
       (find-mutated! key #f)
       (find-mutated! val #f)
       (find-mutated! body ids)]
      [`(begin ,exps ...)
       (find-mutated!* exps ids)]
      [`(begin-unsafe ,exps ...)
       (find-mutated!* exps ids)]
      [`(begin0 ,exp ,exps ...)
       (find-mutated! exp ids)
       (find-mutated!* exps #f)]
      [`(set! ,id ,rhs)
       (let ([id (unwrap id)])
         (define old-state (hash-ref mutated id #f))
         (hash-set! mutated id (state->set!ed-state old-state))
         (when (delayed-mutated-state? old-state)
           (old-state)))
       (find-mutated! rhs #f)]
      [`(#%variable-reference . ,_) (void)]
      [`(,rator ,exps ...)
       (cond
         [(and ids
               (let ([rator (unwrap rator)])
                 (and (symbol? rator)
                      (let ([v (find-known rator prim-knowns knowns imports mutated)])
                        (and (or (known-constructor? v)
                                 ;; Some ad hoc constructors that are particularly
                                 ;; useful to struct-type properties:
                                 (eq? rator 'cons)
                                 (eq? rator 'list)
                                 (eq? rator 'vector)
                                 (eq? rator 'make-struct-type-property))
                             (bitwise-bit-set? (known-procedure-arity-mask v) (length exps))))
                      (for/and ([exp (in-list exps)])
                        (simple? exp prim-knowns knowns imports mutated simples unsafe-mode?
                                 #:ordered? #t
                                 #:succeeds? #t)))))
          ;; Can delay construction
          (delay! ids (lambda () (find-mutated!* exps #f)))]
         [else
          (find-mutated! rator #f)
          (find-mutated!* exps #f)])]
      [`,_
       (let ([v (unwrap v)])
         (when (symbol? v)
           (define state (hash-ref mutated v #f))
           (cond
             [(not-ready-mutated-state? state)
              (unless unsafe-mode? ; unsafe => assume too-early won't happen
                (hash-set! mutated v 'too-early))]
             [(delayed-mutated-state? state)
              (cond
                [ids
                 ;; Chain delays
                 (delay! ids (lambda ()
                               (when (eq? (hash-ref mutated v #f) state)
                                 (hash-remove! mutated v))
                               (state)))]
                [else
                 (hash-remove! mutated v)
                 (state)])])))])))

(define (update-mutated-state! l mut-l mutated)
  (cond
    [(wrap-null? mut-l) '()]
    [(eq? l mut-l)
     ;; Check for function definitions at the start of `l`, because we
     ;; can mark all 'too-early variable uses as being ready from now
     ;; on
     (define new-mut-l
       (let loop ([mut-l mut-l])
         (cond
           [(wrap-null? mut-l) '()]
           [else
            (match (wrap-car mut-l)
              [`(define-values (,ids ...) ,rhs)
               (cond
                 [(lambda? rhs #:simple? #t)
                  (for ([id (in-list ids)])
                    (define u-id (unwrap id))
                    (define state (hash-ref mutated u-id #f))
                    (when (and (too-early-mutated-state? state)
                               (not (set!ed-mutated-state? state)))
                      (hash-set! mutated u-id 'too-early/ready)))
                  (loop (wrap-cdr mut-l))]
                 [else mut-l])]
              [`,_ mut-l])])))
     (if (eq? mut-l l)
         (wrap-cdr mut-l)
         l)]
    [else mut-l]))
