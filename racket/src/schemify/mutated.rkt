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
;; might be mutated implicitly via `call/cc` or might be referenced
;; before it has a value.

(define (mutated-in-body l exports extra-variables prim-knowns knowns imports simples
                         unsafe-mode? target enforce-constant?)
  ;; Find all `set!`ed variables, and also record all bindings
  ;; that might be used too early
  (define mutated (make-hasheq))
  ;; Find `set!`s early, since `simple?` needs to know about `set!`s
  ;; anywhere; at the same time, the mutation pass here generally wants
  ;; to similate evaluation order to more precisely track when variables
  ;; are available and mutated; we resolve his mismatch using an
  ;; `eventually-set!ed` record that is mostly skipped over here by
  ;; using `hash-ref-mutated` and `hash-set-mutated!` but that shows up
  ;; as a mutated variable to other parts, including `simple?`
  (for ([form (in-list l)])
    (match form
      [`(define-values (,ids ...) ,rhs)
       (find-set!ed! rhs mutated)]
      [`,_
       (find-set!ed! form mutated)]))
  ;; Defined names start out as 'not-ready; start with `exports`,
  ;; because anything exported but not defined is implicitly in an
  ;; undefined state and must be accessed through a `variable`:
  (for ([id (in-hash-keys exports)])
    (hash-set-mutated! mutated id 'undefined))
  ;; Find all defined variables, and find variables that are not exported:
  (define unexported-ids
    (for/fold ([unexported-ids '()]) ([form (in-list l)])
      (match form
        [`(define-values (,ids ...) ,rhs)
         (for/fold ([unexported-ids unexported-ids]) ([id (in-list ids)])
           (define u-id (unwrap id))
           (hash-set-mutated! mutated u-id
                              (if enforce-constant?
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
      (hash-set-mutated! mutated id (lambda ()
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
           (cond
             [(= pos struct-type-info-rest-properties-list-pos)
              (define prop-vals (pure-properties-list e prim-knowns knowns imports mutated simples))
              (cond
                [prop-vals
                 ;; check individual values for "nice" properties using `ids`, so procedures won't
                 ;; count as used until some instance is created
                 (for ([do-nice? (in-list '(#f #t))])
                   (for ([nice?+key+val (in-list prop-vals)])
                     (match nice?+key+val
                       [`(,nice? ,key . ,val)
                        (cond
                          [nice?
                           (when do-nice?
                             (find-mutated! val ids prim-knowns knowns imports mutated simples unsafe-mode?))]
                          [else
                           (when (not do-nice?)
                             (find-mutated! key #f prim-knowns knowns imports mutated simples unsafe-mode?)
                             (find-mutated! val #f prim-knowns knowns imports mutated simples unsafe-mode?))])])))]
                [else
                 ;; some arguments might be called via a guard
                 (find-mutated! e #f prim-knowns knowns imports mutated simples unsafe-mode?)])]
             [else
              ;; any function arguments in other positions will not be called until an instance
              ;; is created
              (find-mutated! e ids prim-knowns knowns imports mutated simples unsafe-mode?)]))]
        [else
         (find-mutated! rhs ids prim-knowns knowns imports mutated simples unsafe-mode?)])
       ;; For any among `ids` that didn't get a delay and wasn't used
       ;; too early, the variable is now ready, so remove from
       ;; `mutated`
       (for ([id (in-list ids)])
         (let ([id (unwrap id)])
           (when (eq? 'not-ready (hash-ref-mutated mutated id #f))
             (hash-set-mutated! mutated id #f))))]
      [`,_
       (find-mutated! form #f prim-knowns knowns imports mutated simples unsafe-mode?)])
    knowns)
  ;; For definitions that are not yet used, force delays:
  (for ([form (in-list l)])
    (match form
      [`(define-values (,ids ...) ,rhs)
       (for ([id (in-list ids)])
         (let ([id (unwrap id)])
           (define state (hash-ref-mutated mutated id #f))
           (when unexported-ready
             (when (not (hash-ref exports id #f))
               (hash-set! unexported-ready id #t)))
           (when (delayed-mutated-state? state)
             (hash-remove! mutated id)
             (state))))]
      [`,_ (void)]))
  ;; If an `eventually-set!ed?` persist, it means that the `set!` is
  ;; unreachable after all; if a delayed exists, then it means that
  ;; the identifier is unreachable
  (for ([(k v) (in-hash mutated)])
    (cond
      [(eventually-set!ed? v)
       (let ([v (eventually-set!ed-inner v)])
         (cond
           [(not v) (hash-remove! mutated k)]
           [(delayed-mutated-state? v)
            (hash-set! mutated k 'unused)]
           [else
            (hash-set! mutated k v)]))]
      [(delayed-mutated-state? v)
       (hash-set! mutated k 'unused)]))
  ;; Check for unexported variables that need to be implemented like exports:
  (unless (or unsafe-mode?
              (aim? target 'system))
    (for ([id (in-list unexported-ids)])
      (define state (hash-ref-mutated mutated id #f))
      (when (via-variable-mutated-state? state)
        ;; force creation of variable
        (id-to-variable id exports extra-variables))))
  ;; Everything else in `mutated` is either 'set!ed, 'too-early,
  ;; 'undefined, or unreachable:
  mutated)

;; Simple pass to find variables that are directly `set!`ed
(define (find-set!ed! v mutated)
  (let find-set!ed! ([v v])
    (define (find-set!ed!* l)
      (let loop ([l l])
        (cond
         [(null? l) (void)]
         [(null? (cdr l)) (find-set!ed! (car l))]
         [else (find-set!ed! (car l)) (loop (cdr l))])))
    (match v
      [`(lambda ,formals ,body ...)
       (find-set!ed!* body)]
      [`(case-lambda [,formalss ,bodys ...] ...)
       (for ([body (in-list bodys)]) (find-set!ed!* body))]
      [`(quote ,_) (void)]
      [`(let-values ([,idss ,rhss] ...) ,bodys ...)
       (for ([rhs (in-list rhss)])
         (find-set!ed! rhs))
       (find-set!ed!* bodys)]
      [`(letrec-values ([,idss ,rhss] ...) ,bodys ...)
       (for ([rhs (in-list rhss)])
         (find-set!ed! rhs))
       (find-set!ed!* bodys)]
      [`(if ,tst ,thn ,els)
       (find-set!ed! tst)
       (find-set!ed! thn)
       (find-set!ed! els)]
      [`(with-continuation-mark ,key ,val ,body)
       (find-set!ed! key)
       (find-set!ed! val)
       (find-set!ed! body)]
      [`(begin ,exps ...)
       (find-set!ed!* exps)]
      [`(begin-unsafe ,exps ...)
       (find-set!ed!* exps)]
      [`(begin0 ,exp ,exps ...)
       (find-set!ed! exp)
       (find-set!ed!* exps)]
      [`(set! ,id ,rhs)
       (hash-set! mutated (unwrap id) eventually-set!ed-init)
       (find-set!ed! rhs)]
      [`(#%variable-reference . ,_) (void)]
      [`(#%foreign-inline . ,_) (void)]
      [`(,rator ,exps ...)
       (find-set!ed! rator)
       (find-set!ed!* exps)]
      [`,_ (void)])))

;; More complex pass to find variables that are implicitly mutated by
;; being referenced early or bding on the left-hand side of a `letrec`
;; where the right-hand side might capture a continuation that is
;; resumed multiple times. Rely on `find-set!ed!` having already recorded
;; directly mutated variables.
(define (find-mutated! top-v ids prim-knowns knowns imports mutated simples unsafe-mode?)
  (define (delay! ids thunk)
    (define done? #f)
    (define force (lambda () (unless done?
                               (set! done? #t)
                               (thunk))))
    (for ([id (in-list ids)])
      (let ([id (unwrap id)])
        (define m (hash-ref-mutated mutated id 'not-ready))
        (cond
          [(eq? 'not-ready m)
           (hash-set-mutated! mutated id force)]
          [(procedure? m)
           (hash-set-mutated! mutated id (lambda () (m) (force)))]
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
            (hash-set-mutated! mutated (unwrap id) 'not-ready))
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
                (define state (hash-ref-mutated mutated u-id #f))
                (define (add-too-early-name!)
                  (cond
                    [(and (eq? 'too-early state)
                          (wrap-property id 'undefined-error-name))
                     => (lambda (name)
                          (hash-set-mutated! mutated u-id (too-early name #f)))]
                    [(and (eq? 'set!ed-too-early state)
                          (wrap-property id 'undefined-error-name))
                     => (lambda (name)
                          (hash-set-mutated! mutated u-id (too-early name #t)))]))
                (cond
                  [new-maybe-cc?
                   (cond
                     [(or (eq? 'not-ready state)
                          (delayed-mutated-state? state))
                      (hash-set-mutated! mutated u-id 'implicitly-set!ed)]
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
       ;; visit `exps` before `exp`, which matters if `ids` is
       ;; provided; the `ids` will not be assigned the results of
       ;; `exp` until after `exps` are evaluated, so we need to visit
       ;; `exps` first; meanwhile, the relative order of visiting
       ;; sibling expressions (which cannot add bindings for each
       ;; other) does not matter
       (find-mutated!* exps #f)
       (find-mutated! exp ids)]
      [`(set! ,id ,rhs)
       ;; Although the `find-set!ed!` pass has already noted mutated in `set!ed`
       ;; variables, we want to distinguish variables mutated potentially
       ;; too early from those mutated after being defined, and `mutated`
       ;; tracks that difference
       (let ([id (unwrap id)])
         (define old-state (hash-ref-mutated mutated id #f))
         (hash-set! mutated id (state->set!ed-state old-state))
         (when (delayed-mutated-state? old-state)
           (old-state)))
       (find-mutated! rhs #f)]
      [`(#%variable-reference . ,_) (void)]
      [`(#%foreign-inline . ,_) (void)]
      [`(values ,exps ...)
       (cond
         [(and ids (= (length ids) (length exps)))
          (for ([id (in-list ids)]
                [exp (in-list exps)])
            (find-mutated! exp (list id)))]
         [else
          (for ([exp (in-list exps)])
            (find-mutated! exp #f))])]
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
                                 (eq? rator 'list*)
                                 (eq? rator 'vector)
                                 (eq? rator 'hasheq)
                                 (eq? rator 'make-struct-type-property))
                             (bitwise-bit-set? (known-procedure-arity-mask v) (length exps))))
                      (for/and ([exp (in-list exps)])
                        (simple? exp prim-knowns knowns imports mutated simples unsafe-mode?
                                 #:pure? #t
                                 #:parameter-ok? #t)))))
          ;; Can delay construction
          (delay! ids (lambda () (find-mutated!* exps #f)))]
         [else
          (find-mutated! rator #f)
          (find-mutated!* exps #f)])]
      [`,_
       (let ([v (unwrap v)])
         (when (symbol? v)
           (define state (hash-ref-mutated mutated v #f))
           (cond
             [(not-ready-mutated-state? state)
              (unless unsafe-mode? ; unsafe => assume too-early won't happen
                (hash-set-mutated! mutated v 'too-early))]
             [(delayed-mutated-state? state)
              (cond
                [ids
                 ;; Chain delays
                 (delay! ids (lambda ()
                               (when (eq? (hash-ref-mutated mutated v #f) state)
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
                      (hash-set-mutated! mutated u-id 'too-early/ready)))
                  (loop (wrap-cdr mut-l))]
                 [else mut-l])]
              [`,_ mut-l])])))
     (if (eq? mut-l l)
         (wrap-cdr mut-l)
         l)]
    [else mut-l]))
