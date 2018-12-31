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
         "letrec.rkt")

(provide mutated-in-body
         update-mutated-state!)

;; See "mutated-state.rkt" for information on the content of the
;; `mutated` table.

;; We don't have to worry about errors or escapes that prevent the
;; definition of an identifier, because that will abort the enclosing
;; linklet.

(define (mutated-in-body l exports prim-knowns knowns imports unsafe-mode?)
  ;; Find all `set!`ed variables, and also record all bindings
  ;; that might be used too early
  (define mutated (make-hasheq))
  ;; Defined names start out as 'not-ready; start with `exports`,
  ;; because anything exported but not defined is implicitly in an
  ;; undefined state and must be accessed through a `variable`:
  (for ([id (in-hash-keys exports)])
    (hash-set! mutated id 'undefined))
  ;; Find all defined variables:
  (for ([form (in-list l)])
    (match form
      [`(define-values (,ids ...) ,rhs)
       (for ([id (in-list ids)])
         (hash-set! mutated (unwrap id) 'not-ready))]
      [`,_ (void)]))
  ;; Walk through the body:
  (for/fold ([prev-knowns knowns]) ([form (in-list l)])
    ;; Accumulate known-binding information in this pass, because it's
    ;; helpful to know which variables are bound to constructors.
    ;; Note that we may tentatively classify a binding as a constructor
    ;; before discovering that its mutated via `set!`, but any use of
    ;; that information is correct, because it dynamically precedes
    ;; the `set!`
    (define-values (knowns info)
      (find-definitions form prim-knowns prev-knowns imports mutated unsafe-mode?
                        #:optimize? #f))
    (match form
      [`(define-values (,ids ...) ,rhs)
       (cond
        [info
         ;; Look just at the "rest" part:
         (for ([e (in-list (struct-type-info-rest info))]
               [pos (in-naturals)])
           (unless (and (= pos struct-type-info-rest-properties-list-pos)
                        (pure-properties-list? e prim-knowns knowns imports mutated))
             (find-mutated! e ids prim-knowns knowns imports mutated)))]
        [else
         (find-mutated! rhs ids prim-knowns knowns imports mutated)])
       ;; For any among `ids` that didn't get a delay and wasn't used
       ;; too early, the variable is now ready, so remove from
       ;; `mutated`:
       (for ([id (in-list ids)])
         (let ([id (unwrap id)])
           (when (eq? 'not-ready (hash-ref mutated id #f))
             (hash-remove! mutated id))))]
      [`,_
       (find-mutated! form #f prim-knowns knowns imports mutated)])
    knowns)
  ;; For definitions that are not yet used, force delays:
  (for ([form (in-list l)])
    (match form
      [`(define-values (,ids ...) ,rhs)
       (for ([id (in-list ids)])
         (let ([id (unwrap id)])
           (define state (hash-ref mutated id #f))
           (when (delayed-mutated-state? state)
             (hash-remove! mutated id)
             (state))))]
      [`,_ (void)]))
  ;; Everything else in `mutated` is either 'set!ed, 'too-early,
  ;; 'undefined, or unreachable:
  mutated)

;; Schemify `let-values` to `let`, etc., and
;; reorganize struct bindings.
(define (find-mutated! v ids prim-knowns knowns imports mutated)
  (define (delay! ids thunk)
    (define done? #f)
    (define force (lambda () (unless done?
                               (set! done? #t)
                               (thunk))))
    (for ([id (in-list ids)])
      (let ([id (unwrap id)])
        (define m (hash-ref mutated id 'not-ready))
        (if (eq? 'not-ready m)
            (hash-set! mutated id force)
            (force)))))
  (let find-mutated! ([v v] [ids ids])
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
          (for ([ids (in-list idss)]
                [rhs (in-list rhss)])
            (find-mutated! rhs (unwrap-list ids))
            ;; Each `id` in `ids` is now ready (but might also hold a delay):
            (for ([id (in-wrap-list ids)])
              (let ([id (unwrap id)])
                (when (eq? 'not-ready (hash-ref mutated id))
                  (hash-remove! mutated id)))))
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
      [`(begin0 ,exp ,exps ...)
       (find-mutated! exp ids)
       (find-mutated!* exps #f)]
      [`(set! ,id ,rhs)
       (let ([id (unwrap id)])
         (define old-state (hash-ref mutated id #f))
         (hash-set! mutated id 'set!ed)
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
                        (and (known-constructor? v)
                             (bitwise-bit-set? (known-procedure-arity-mask v) (length exps))))
                      (for/and ([exp (in-list exps)])
                        (simple? exp prim-knowns knowns imports mutated)))))
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
              (hash-set! mutated v 'too-early)]
             [(delayed-mutated-state? state)
              (cond
                [ids
                 ;; Chain delays
                 (delay! ids (lambda ()
                               (hash-remove! mutated v)
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
                    (when (too-early-mutated-state? (hash-ref mutated u-id #f))
                      (hash-set! mutated u-id 'too-early/ready)))
                  (loop (wrap-cdr mut-l))]
                 [else mut-l])]
              [`,_ mut-l])])))
     (if (eq? mut-l l)
         (wrap-cdr mut-l)
         l)]
    [else mut-l]))
