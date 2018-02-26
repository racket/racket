#lang racket/base
(require racket/pretty
         "match.rkt"
         "arg.rkt"
         "state.rkt"
         "union.rkt")

(provide make-ref
         (struct-out ref)
         symbol-ref?
         unref

         ref-use!
         ref-implicit-use!
         ref-use-branch-before!
         ref-use-branch-other!
         ref-use-branch-merge!

         wrap-ref)

;; Wrap each refernce to a local variable with a `ref` record, which
;; provides an identity for tracking last references to implement
;; space safety.

;; This pass eliminates `#%app`, so other passes don't have to
;; recognize it.

(struct ref (id [last-use? #:mutable]) #:transparent)
(define (make-ref id) (ref id #f))

(define (symbol-ref? e) (or (symbol? e) (ref? e)))
(define (unref e) (if (ref? e) (ref-id e) e))

;; ----------------------------------------

;; Keep track of last use through a pair of tables:
;;
;;  '#:ref in state --- maps each identifier to a list
;;     of current last uses for the identifier; there
;;     can be more than one last use due to branches
;;
;;  '#:newly-used in '#:ref --- a set of identifiers
;;     that have become newly used; this set is swapped
;;     in and out to merge last uses for branches.

(define (get-ref-state state)
  (or (hash-ref state '#:ref #f)
      (let ([ht (make-hasheq)])
        (hash-set! state '#:ref ht)
        ht)))

(define (ref-use! ref state)
  (when (state-first-pass? state)
    (ref-id-use! (ref-id ref) state (list ref))
    (set-ref-last-use?! ref #t)))

(define (ref-implicit-use! id state)
  (when (state-first-pass? state)
    (ref-id-use! id state '())))

(define (ref-id-use! id state new-uses)
  (define ref-state (get-ref-state state))
  (for ([u-ref (in-list (hash-ref ref-state id '()))])
    (set-ref-last-use?! u-ref #f))
  (hash-set! ref-state id new-uses)
  (let ([newly-used (hash-ref ref-state '#:newly-used #hasheq())])
    (hash-set! ref-state '#:newly-used (hash-set newly-used id #t))))

(define (ref-use-branch-before! state)
  (define ref-state (get-ref-state state))
  (define newly-used (hash-ref ref-state '#:newly-used #hasheq()))
  (hash-set! ref-state '#:newly-used #hasheq())
  ;; `pre` is the original newly-used set:
  newly-used)

(define (ref-use-branch-other! state pre)
  (define ref-state (get-ref-state state))
  (define newly-used (hash-ref ref-state '#:newly-used #hasheq()))
  ;; Remove `then` branch's uses from the table, so they're not
  ;; reset by uses in the `else` branch. The `post` value is a
  ;; table mapping ids to last-use lists (to restore later):
  (define post
    (for/hash ([id (in-hash-keys newly-used)])
      (define old-lasts (hash-ref ref-state id))
      (hash-set! ref-state id '())
      (values id old-lasts)))
  (hash-set! ref-state '#:newly-used #hasheq())
  post)

(define (ref-use-branch-merge! state pre post)
  (define ref-state (get-ref-state state))
  (define newly-used (hash-ref ref-state '#:newly-used #hasheq()))
  ;; Union the newly-useds:
  (hash-set! ref-state '#:newly-used (hash-union (hash-union pre post) newly-used))
  ;; Re-register the `then` branch's uses:
  (for ([(id uses) (in-hash post)])
    (hash-set! ref-state id (append uses (hash-ref ref-state id '())))))

;; ----------------------------------------

(define (wrap-ref e top-names prim-names state)
  (define (wrap-ref e env)
    (match e
      [`(define ,id ,rhs)
       `(define ,id ,(wrap-ref rhs env))]
      [`(define-values (,ids ...) (values ,rhss ...))
       (wrap-ref `(begin
                    ,@(for/list ([id (in-list ids)]
                                 [rhs (in-list rhss)])
                        `(define ,id ,rhs)))
                 env)]
      [`(define-values ,ids ,rhs)
       `(define-values ,ids ,(wrap-ref rhs env))]
      [`(begin ,e)
       (wrap-ref e env)]
      [`(begin ,es ...)
       `(begin . ,(wrap-body-ref es env))]
      [`(begin0 ,es ...)
       `(begin0 . ,(wrap-body-ref es env))]
      [`(lambda ,ids . ,body)
       `(lambda ,ids . ,(wrap-body-ref body (add-args env ids)))]
      [`(case-lambda [,idss . ,bodys] ...)
       `(case-lambda ,@(for/list ([ids (in-list idss)]
                                  [body (in-list bodys)])
                         `[,ids . ,(wrap-body-ref body (add-args env ids))]))]
      [`(quote ,_) e]
      [`(if ,tst ,thn ,els)
       `(if ,(wrap-ref tst env) ,(wrap-ref thn env) ,(wrap-ref els env))]
      [`(with-continuation-mark ,key ,val ,body)
       `(with-continuation-mark ,(wrap-ref key env) ,(wrap-ref val env) ,(wrap-ref body env))]
      [`(let . ,_) (wrap-let-ref e env)]
      [`(letrec . ,_) (wrap-let-ref e env)]
      [`(letrec* . ,_) (wrap-let-ref e env)]
      [`(set! ,id ,rhs)
       `(set! ,(wrap-ref id env) ,(wrap-ref rhs env))]
      [`(call-with-values (lambda () (values ,val-es ...)) (lambda (,arg-ids ...) . ,body))
       (wrap-ref
        `(let ,(for/list ([arg-id (in-list arg-ids)]
                          [val-e (in-list val-es)])
                 `[,arg-id ,val-e])
           . ,body)
        env)]
      [`(call-with-values (lambda () (let ,binds . ,body)) ,target)
       (wrap-ref `(let ,binds (call-with-values (lambda () . ,body) ,target)) env)]
      [`(call-with-values (lambda () (begin ,e)) ,target)
       (wrap-ref `(call-with-values (lambda () ,e) ,target) env)]
      [`(call-with-values (lambda () (begin ,e . ,r)) ,target)
       (wrap-ref `(begin ,e (call-with-values (lambda () (begin . ,r)) ,target)) env)]
      [`(list) 'null]
      [`(#%app . ,r)
       (wrap-ref r env)]
      [`(,rator ,rands ...)
       (wrap-body-ref e env)]
      [`,_
       (cond
         [(symbol? e)
          (let loop ([e e])
            (define r (hash-ref env e #f))
            (cond
              [(propagate? r)
               (loop r)]
              [else
               (if (or (hash-ref top-names e #f)
                       (hash-ref prim-names e #f))
                   e
                   (make-ref e))]))]
         [else e])]))

  (define (wrap-body-ref es env)
    (for/list ([e (in-list es)])
      (wrap-ref e env)))

  (define (wrap-let-ref e env)
    (match e
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (define body-env (for/fold ([env env]) ([id (in-list ids)]
                                               [rhs (in-list rhss)]
                                               #:unless (hash-ref top-names id #f))
                          (hash-set env id (get-propagate id rhs env state))))
       (define rhs-env (if (eq? let-id 'let) env body-env))
       (define new-e
         (cond
           [(for/and ([id (in-list ids)])
              (and (not (hash-ref top-names id #f))
                   (propagate? (hash-ref body-env id #f))))
            ;; All propagated
            (wrap-ref `(begin . ,body) body-env)]
           [else
            `(,let-id ,(for/list ([id (in-list ids)]
                                  [rhs (in-list rhss)]
                                  #:unless (and (not (hash-ref top-names id #f))
                                                (propagate? (hash-ref body-env id #f))))
                         `[,id ,(wrap-ref rhs rhs-env)])
                      . ,(wrap-body-ref body body-env))]))
       ;; For each variable that is propagated, remove
       ;; the use of the right-hand side:
       (for ([id (in-list ids)]
             [rhs (in-list rhss)])
         (define r (hash-ref body-env id #f))
         (when (propagate? r)
           (adjust-state! state r (hash-ref state id 0))
           (adjust-state! state rhs -1)
           (hash-remove! state id)))
       ;; Return form with wrapped refs:
       new-e]))

  (wrap-ref e (top-env e top-names prim-names state)))

;; ----------------------------------------

;; Extract an initial copy-propagation mapping for top names

(define (top-env e top-names prim-names state)
  (define (top-env e for-id env)
    (match e
      [`(define ,id ,rhs)
       (top-env rhs id env)]
      [`(define-values (,ids ...) (values ,rhss ...))
       (for/fold ([env env]) ([id (in-list ids)]
                              [rhs (in-list rhss)])
         (top-env rhs id env))]
      [`(define-values ,ids ,rhs)
       (top-env rhs #f env)]
      [`(begin ,e ,es ...)
       (top-body-env es for-id env)]
      [`(begin0 ,e ,es ...)
       (top-body-env es #f (top-env e for-id env))]
      [`(lambda . ,_) env]
      [`(case-lambda . ,_) env]
      [`(quote ,_) env]
      [`(if ,tst ,thn ,els)
       (top-env tst #f (top-env thn #f (top-env els #f env)))]
      [`(with-continuation-mark ,key ,val ,body)
       (top-env key #f (top-env val #f (top-env body for-id env)))]
      [`(let . ,_) (top-let-env e for-id env)]
      [`(letrec . ,_) (top-let-env e for-id env)]
      [`(letrec* . ,_) (top-let-env e for-id env)]
      [`(set! ,id ,rhs) (top-env rhs #f env)]
      [`(#%app . ,r) (top-env r for-id env)]
      [`(,rator ,rands ...)
       (top-body-env e #f env)]
      [`,_
       (cond
         [(symbol? e)
          (if (and for-id
                   (hash-ref top-names for-id #f)
                   (or (hash-ref top-names e #f)
                       (hash-ref prim-names e #f))
                   (not (mutated? (hash-ref state e #f)))
                   (not (mutated? (hash-ref state for-id #f))))
              (hash-set env for-id e)
              env)]
         [else env])]))

  (define (top-body-env es for-id env)
    (cond
      [(null? es) env]
      [(null? (cdr es)) (top-env (car es) for-id env)]
      [else (top-body-env (cdr es) for-id (top-env (car es) #f env))]))

  (define (top-let-env e for-id env)
    (match e
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (top-body-env
        body
        for-id
        (for/fold ([env env]) ([id (in-list ids)]
                               [rhs (in-list rhss)])
          (top-env rhs id env)))]))

  (top-env e #f #hasheq()))

;; ----------------------------------------

(define (propagate? v) (symbol? v))

(define (get-propagate id rhs env state)
  (match rhs
    #;
    [`(begin ,e)
     (get-propagate id e env state)]
    #;
    [`(values ,e)
     (get-propagate id e env state)]
    [`,_
     (cond
       [(and (symbol? rhs)
             (not (mutated? (hash-ref state rhs #f)))
             (not (mutated? (hash-ref state id #f))))
        (define r (hash-ref env rhs #f))
        (if (propagate? r)
            r
            rhs)]
       [else #t])]))
