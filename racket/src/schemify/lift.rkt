#lang racket/base
(require "match.rkt"
         "wrap.rkt")

;; Reduces closure allocation by lifting bindings that are only used
;; in calls that have the right number of arguments.

;; The output uses `letrec` to bind lifted and closed functions, while
;; `letrec*` is still used for any other recursive binding.

(provide lift-in-schemified-linklet
         lift-in-schemified-body)

;; An identifier registered in `lifts` is one of
;;
;;  * `liftable` - a function binding that is (so far) only referenced
;;                 in an application position with a correct number of
;;                 arguments, so each call can supply the free
;;                 variables of the function and the closure
;;                 allocation (if any) can be lifted to the top level
;;
;; * `indirected` - a variable that is `set!`ed, which means that it can't be
;;                replaced by an argument if it appears as a free
;;                variable in a liftable function; instead, the
;;                argument must be a box
;;
;; There's nothing analogous to `mutator` and `var-ref` for
;; synthesized accessors, because they're relevant only for the second
;; pass and recorded in an `indirected`.
;;
;; The `lifts` table can also contain `lambda` and `case-lambda` forms
;; mapped to '#:empty, meaning that the closure is empty relative to the
;; enclosing linklet and can be lifted so that each is allocated once per
;; linklet.
;;
;; An identifier registered in `locals` maps to either 'ready or 'early,
;; where 'early is used during the right-hand side of a letrec that is
;; not all `lambda`s.

(struct liftable (expr ; a `lambda` or `case-lambda` RHS of the binding
                  [frees #:mutable] ; set of variables free in `expr`, plus any lifted bindings
                  [binds #:mutable])) ; set of variables bound in `expr`

(struct indirected ([check? #:mutable]))

(struct mutator (orig)) ; `orig` maps back to the original identifier
(struct var-ref (orig)) ; ditto

;; As we traverse expressions, we thread through free- and
;; bound-variable sets
(define empty-frees+binds (cons #hasheq() #hasheq()))

(define (lift-in-schemified-linklet v)
  ;; Match outer shape of a linklet produced by `schemify-linklet`
  ;; and lift in the linklet body:
  (let loop ([v v])
    (match v
      [`(lambda ,args . ,body)
       (define new-body (lift-in-schemified-body body))
       (if (for/and ([old (in-list body)]
                     [new (in-list new-body)])
             (eq? old new))
           v
           `(lambda ,args . ,new-body))]
      [`(let* ,bindings ,body)
       (define new-body (loop body))
       (if (eq? body new-body)
           v
           `(let* ,bindings ,new-body))])))

(define (lift-in-schemified-body body)
  (for/list ([v (in-list body)])
    (lift-in-schemified v)))

(define (lift-in-schemified v)
  ;; Quick pre-check: do any lifts appear to be possible?
  (define (lift-in? v)
    (match v
      [`(define ,_ ,rhs)
       (lift-in-expr? rhs)]
      [`(define-values ,_ ,rhs)
       (lift-in-expr? rhs)]
      [`(begin . ,vs)
       (for/or ([v (in-wrap-list vs)])
         (lift-in? v))]
      [`,_ (lift-in-expr? v)]))
    
  (define (lift-in-expr? v)
    (match v
      [`(lambda ,_ . ,body)
       (lift?/seq body)]
      [`(case-lambda [,_ . ,bodys] ...)
       (for/or ([body (in-list bodys)])
         (lift?/seq body))]
      [`(let . ,_) (lift-in-let? v)]
      [`(letrec . ,_) (lift-in-let? v)]
      [`(letrec* . ,_) (lift-in-let? v)]
      [`(let-values . ,_) (error 'internal-error "unexpected let-values")]
      [`(letrec-values . ,_) (error 'internal-error "unexpected letrec-values")]
      [`(begin . ,vs)
       (for/or ([v (in-wrap-list vs)])
         (lift-in-expr? v))]
      [`(if ,tst ,thn ,els)
       (or (lift-in-expr? tst) (lift-in-expr? thn) (lift-in-expr? els))]
      [`(with-continuation-mark ,key ,val ,body)
       (or (lift-in-expr? key) (lift-in-expr? val) (lift-in-expr? body))]
      [`(quote ,_) #f]
      [`(#%variable-reference . ,_) (error 'internal-error "unexpected variable reference")]
      [`(set! ,_ ,rhs)
       (lift-in-expr? rhs)]
      [`(,_ ...)
       (lift-in-seq? v)]
      [`,_ #f]))

  (define (lift-in-let? v)
    (match v
      [`(,_ ([,_ ,rhss] ...) . ,body)
       (or (for/or ([rhs (in-list rhss)])
             (lift-in-expr? rhs))
           (lift-in-seq? body))]))
  
  (define (lift-in-seq? vs)
    (for/or ([v (in-wrap-list vs)])
      (lift-in-expr? v)))

  ;; Under a `lambda`; any local bindings to functions or
  ;; `[case-]lambda`s that might be closed?
  (define (lift? v)
    (match v
      [`(let . ,_) (lift?/let v)]
      [`(letrec . ,_) (lift?/let v)]
      [`(letrec* . ,_) (lift?/let v)]
      [`(let-values . ,_) (lift?/let v)]
      [`(letrec-values . ,_) (lift?/let v)]
      [`(lambda ,_ . ,body) #t #;(lift?/seq body)]
      [`(case-lambda [,_ . ,bodys] ...)
       #t
       #;
       (for/or ([body (in-list bodys)])
         (lift?/seq body))]
      [`(begin . ,vs) (lift?/seq vs)]
      [`(begin0 . ,vs) (lift?/seq vs)]
      [`(quote . ,_) #f]
      [`(if ,tst ,thn ,els)
       (or (lift? tst) (lift? thn) (lift? els))]
      [`(with-continuation-mark ,key ,val ,body)
       (or (lift? key) (lift? val) (lift? body))]
      [`(set! ,_ ,rhs) (lift? rhs)]
      [`(#%variable-reference) #f]
      [`(#%variable-reference ,id) #f]
      [`(,rator . ,rands)
       (or (lift? rator) (lift?/seq rands))]
      [`,_ #f]))
  
  (define (lift?/let v)
    (match v
      [`(,_ ([,_ ,rhss] ...) . ,body)
       (or (for/or ([rhs (in-list rhss)])
             (or (lambda? rhs)
                 (lift? rhs)))
           (lift?/seq body))]))

  (define (lift?/seq vs)
    (for/or ([v (in-wrap-list vs)])
      (lift? v)))

  ;; ----------------------------------------
  
  ;; Look for a `lambda` to lift out of:
  (define (lift-in v)
    (match v
      [`(define ,id ,rhs)
       (reannotate v `(define ,id ,(lift-in-expr rhs)))]
      [`(define-values ,ids ,rhs)
       (reannotate v `(define-values ,ids ,(lift-in-expr rhs)))]
      [`(begin ,vs ...)
       (reannotate v `(begin ,@(for/list ([v (in-wrap-list vs)])
                                 (lift-in v))))]
      [`,_ (lift-in-expr v)]))

  ;; Look for a `lambda` to lift out of:
  (define (lift-in-expr v)
    (match v
      [`(lambda ,args . ,body)
       (define lifts (make-hasheq))
       (define locals (add-args args #hasheq()))
       (define frees+binds/ignored (compute-seq-lifts! body empty-frees+binds lifts locals))
       (let ([lifts (if (zero? (hash-count lifts))
                        lifts
                        (close-and-convert-lifts lifts))])
         (cond
           [(zero? (hash-count lifts)) v]
           [else
            (define empties (box null))
            (define lifted-bindings (extract-lifted-bindings lifts empties))
            (define new-body
              (reannotate v `(lambda ,args . ,(convert-lifted-calls-in-seq/box-mutated body args lifts #hasheq() empties))))
            `(letrec ,(append (unbox empties)
                              lifted-bindings)
               ,new-body)]))]
      [`(case-lambda [,argss . ,bodys] ...)
       ;; Lift each clause separately, then splice results:
       (let ([lams (for/list ([args (in-list argss)]
                              [body (in-list bodys)])
                     (lift-in-expr `(lambda ,args . ,body)))])
         (reannotate
          v
          (let loop ([lams lams] [clauses null] [bindings null])
            (cond
              [(null? lams)
               (if (null? bindings)
                   `(case-lambda ,@(reverse clauses))
                   `(letrec ,bindings ,(loop null clauses null)))]
              [else
               (match (car lams)
                 [`(letrec ,new-bindings ,lam)
                  (loop (cons lam (cdr lams)) clauses (append (unwrap-list new-bindings) bindings))]
                 [`(lambda ,args . ,body)
                  (loop (cdr lams) (cons `[,args . ,body] clauses) bindings)])]))))]
      [`(let . ,_) (lift-in-let v)]
      [`(letrec . ,_) (lift-in-let v)]
      [`(letrec* . ,_) (lift-in-let v)]
      [`(let-values . ,_) (error 'internal-error "unexpected let-values")]
      [`(letrec-values . ,_) (error 'internal-error "unexpected letrec-values")]
      [`(begin . ,vs)
       (reannotate v `(begin ,@(for/list ([v (in-wrap-list vs)])
                                 (lift-in-expr v))))]
      [`(if ,tst ,thn ,els)
       (reannotate v `(if ,(lift-in-expr tst)
                          ,(lift-in-expr thn)
                          ,(lift-in-expr els)))]
      [`(with-continuation-mark ,key ,val ,body)
       (reannotate v `(with-continuation-mark ,(lift-in-expr key)
                                              ,(lift-in-expr val)
                                              ,(lift-in-expr body)))]
      [`(quote ,_) v]
      [`(#%variable-reference . ,_) (error 'internal-error "unexpected variable reference")]
      [`(set! ,id ,rhs)
       (reannotate v `(set! ,id ,(lift-in-expr rhs)))]
      [`(,_ ...)
       (lift-in-seq v)]
      [`,_ v]))

  (define (lift-in-let v)
    (match v
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (reannotate v `(,let-id
                       ,(for/list ([id (in-list ids)]
                                   [rhs (in-list rhss)])
                          `[,id ,(lift-in-expr rhs)])
                       . ,(lift-in-seq body)))]))
  
  (define (lift-in-seq vs)
    (reannotate vs (for/list ([v (in-wrap-list vs)])
                     (lift-in-expr v))))

  ;; ----------------------------------------
  ;; Pass 1: figure out which bindings can be lifted, and also record
  ;; information about mutated and `#%variable-reference` variables.
  ;; We only care about local variables within a top-level `lambda` or
  ;; `case-lambda` form.

  ;; Returns a set of free variables and a set of bound variables
  ;; (paired together) while potentially mutating `lifts`
  (define (compute-lifts! v frees+binds lifts locals [called? #f])
    (match v
      [`(let ([,ids ,rhss] ...) . ,body)
       (for ([id (in-list ids)]
             [rhs (in-list rhss)])
         (when (lambda? rhs)
           ;; RHS is a candidate for lifting
           (hash-set! lifts (unwrap id) (liftable rhs #f #f))))
       (let* ([frees+binds (compute-rhs-lifts! ids rhss frees+binds lifts locals)]
              [frees+binds (compute-seq-lifts! body frees+binds lifts (add-args ids locals))])
         (remove-frees/add-binds ids frees+binds lifts))]
      [`(letrec . ,_)
       (compute-letrec-lifts! v frees+binds lifts locals)]
      [`(letrec* . ,_)
       (compute-letrec-lifts! v frees+binds lifts locals)]
      [`((letrec ([,id ,rhs]) ,rator) ,rands ...)
       (compute-lifts! `(letrec ([,id ,rhs]) (,rator . ,rands)) frees+binds lifts locals)]
      [`((letrec* ([,id ,rhs]) ,rator) ,rands ...)
       (compute-lifts! `(letrec ([,id ,rhs]) (,rator . ,rands)) frees+binds lifts locals)]
      [`(lambda ,args . ,body)
       (let* ([body-frees+binds (cons (car empty-frees+binds) (cdr frees+binds))]
              [body-frees+binds (compute-seq-lifts! body body-frees+binds lifts (add-args args locals))]
              [body-frees+binds (remove-frees/add-binds args body-frees+binds lifts)])
         (when (and (zero? (frees-count body-frees+binds)) (not called?))
           (record-empty-closure! lifts v))
         (cons (union (car body-frees+binds) (car frees+binds))
               (cdr body-frees+binds)))]
      [`(case-lambda [,argss . ,bodys] ...)
       (define init-frees+binds (cons (car empty-frees+binds) (cdr frees+binds)))
       (define new-frees+binds
         (for/fold ([frees+binds init-frees+binds]) ([args (in-list argss)]
                                                     [body (in-list bodys)])
           (let ([frees+binds (compute-seq-lifts! body frees+binds lifts (add-args args locals))])
             (remove-frees/add-binds args frees+binds lifts))))
       (when (and (zero? (frees-count new-frees+binds)) (not called?))
         (record-empty-closure! lifts v))
       (cons (union (car new-frees+binds) (car frees+binds))
             (cdr new-frees+binds))]
      [`(begin . ,vs)
       (compute-seq-lifts! vs frees+binds lifts locals)]
      [`(begin0 . ,vs)
       (compute-seq-lifts! vs frees+binds lifts locals)]
      [`(quote . ,_) frees+binds]
      [`(if ,tst ,thn ,els)
       (let* ([frees+binds (compute-lifts! tst frees+binds lifts locals)]
              [frees+binds (compute-lifts! thn frees+binds lifts locals)]
              [frees+binds (compute-lifts! els frees+binds lifts locals)])
         frees+binds)]
      [`(with-continuation-mark ,key ,val ,body)
       (let* ([frees+binds (compute-lifts! key frees+binds lifts locals)]
              [frees+binds (compute-lifts! val frees+binds lifts locals)]
              [frees+binds (compute-lifts! body frees+binds lifts locals)])
         frees+binds)]
      [`(set! ,id ,rhs)
       (define var (unwrap id))
       (let ([frees+binds (cond
                            [(hash-ref locals var #f)
                             => (lambda (status)
                                  (lookup-indirected-variable lifts var (eq? status 'early))
                                  (add-free frees+binds var))]
                            [else frees+binds])])
         (compute-lifts! rhs frees+binds lifts locals))]
      [`(#%variable-reference . ,_)
       (error 'internal-error "lift: unexpected variable reference")]
      [`(call-with-values ,producer ,consumer)
       (let* ([frees+binds (compute-lifts! producer frees+binds lifts locals #t)]
              [frees+binds (compute-lifts! consumer frees+binds lifts locals #t)])
         frees+binds)]
      [`(,rator . ,rands)
       (define f (unwrap rator))
       (let ([frees+binds
              (cond
                [(symbol? f)
                 (let ([proc (hash-ref lifts f #f)])
                   (when (liftable? proc)
                     (unless (consistent-argument-count? (liftable-expr proc) (length (unwrap-list rands)))
                       (hash-remove! lifts f))))
                 ;; Don't recur on `rator`, because we don't want
                 ;; to mark `f` as unliftable
                 (if (hash-ref locals f #f)
                     (add-free frees+binds f)
                     frees+binds)]
                [else
                 (compute-lifts! rator frees+binds lifts locals)])])
         (compute-seq-lifts! rands frees+binds lifts locals))]
      [`,_
       (define x (unwrap v))
       (cond
         [(or (string? x) (bytes? x) (boolean? x) (number? x))
          frees+binds]
         [else
          (unless (symbol? x)
            (error 'lift-in-schemified
                   "unrecognized expression form: ~e"
                   v))
          ;; If this identifier is mapped to a liftable, then
          ;; the function is not liftable after all, since
          ;; the reference isn't in an application position
          (let ([proc (hash-ref lifts x #f)])
            (when (liftable? proc)
              (hash-remove! lifts x)))
          (let ([loc-status (hash-ref locals x #f)])
            (cond
              [loc-status
               (let ([frees+binds (add-free frees+binds x)])
                 (cond
                   [(eq? loc-status 'early)
                    (lookup-indirected-variable lifts x #t)
                    (add-free frees+binds x)]
                   [else frees+binds]))]
              [else frees+binds]))])]))

  ;; Like `compute-lifts!`, but for a sequence of expressions
  (define (compute-seq-lifts! vs frees+binds lifts locals)
    (for/fold ([frees+binds frees+binds]) ([v (in-wrap-list vs)])
      (compute-lifts! v frees+binds lifts locals)))

  ;; Similar to `compute-seq-lifts!`, but installs free-variable
  ;; information in the `lifts` table for each identifier in `ids`:
  (define (compute-rhs-lifts! ids rhss frees+binds lifts locals)
    (for/fold ([frees+binds frees+binds]) ([id (in-list ids)]
                                           [rhs (in-list rhss)])
      (let ([rhs-frees+binds (compute-lifts! rhs empty-frees+binds lifts locals)]
            [f (unwrap id)])
        (let ([proc (hash-ref lifts f #f)])
          (when (liftable? proc)
            (set-liftable-frees! proc (car rhs-frees+binds))
            (set-liftable-binds! proc (cdr rhs-frees+binds))))
        (cons (union (car rhs-frees+binds) (car frees+binds))
              (union (cdr rhs-frees+binds) (cdr frees+binds))))))

  ;; Handle a letrec[*] form
  (define (compute-letrec-lifts! v frees+binds lifts locals)
    (match v
      [`(,_ ([,ids ,rhss] ...) . ,body)
       (define all-lambda-or-immediate?
         (for/and ([rhs (in-list rhss)])
           (or (lambda? rhs)
               (immediate? rhs))))
       (when all-lambda-or-immediate?
         ;; Each RHS is a candidate for lifting
         (for ([id (in-list ids)]
               [rhs (in-list rhss)])
           (when (lambda? rhs)
             (hash-set! lifts (unwrap id) (liftable rhs #f #f)))))
       (let* ([rhs-locals (add-args ids locals (if all-lambda-or-immediate? 'ready 'early))]
              [frees+binds (compute-rhs-lifts! ids rhss frees+binds lifts rhs-locals)]
              [locals (if all-lambda-or-immediate?
                          rhs-locals
                          (add-args ids locals))]
              [frees+binds (compute-seq-lifts! body frees+binds lifts locals)])
         (remove-frees/add-binds ids frees+binds lifts))]))

  ;; ----------------------------------------
  ;; Bridge between pass 1 and 2: transitive closure of free variables

  ;; Close a liftable's free variables over other variables needed by
  ;; other lifted functions that it calls. Also, clear `mutated` and
  ;; `var-ref` information from `lifts` in the returned table.
  (define (close-and-convert-lifts lifts)
    (define new-lifts (make-hasheq))
    ;; Copy over `liftable`s:
    (for ([(f info) (in-hash lifts)])
      (when (liftable? info)
        (hash-set! new-lifts f info)))
    ;; Compute the closure of free-variable sets, where a function
    ;; to be lifted calls another function to be lifted, and also
    ;; re-register mutators and variable references that are
    ;; used.
    (for ([proc (in-list (hash-values new-lifts))])
      (define frees (liftable-frees proc))
      (define binds (liftable-binds proc))
      (define closed-frees
        (let loop ([frees frees] [todo (hash-keys frees)])
          (cond
            [(null? todo) frees]
            [else
             (define v (car todo))
             (define info (hash-ref lifts v #f))
             (cond
               [(liftable? info)
                ;; A liftable function called by ths liftable function,
                ;; so we'll need to be able to supply all of its free
                ;; variables
                (define v-binds (liftable-binds info))
                (let v-loop ([v-frees (hash-keys (liftable-frees info))]
                             [frees frees]
                             [todo (cdr todo)])
                  (if (null? v-frees)
                      (loop frees todo)
                      (let ([g (car v-frees)])
                        (cond
                          [(or (hash-ref frees g #f)  ; avoid cycles
                               (hash-ref binds g #f) ; don't add if bound in this function
                               (hash-ref v-binds g #f)) ; don't add if local to `v`
                           (v-loop (cdr v-frees) frees todo)]
                          [else
                           (v-loop (cdr v-frees)
                                   (hash-set frees g #t)
                                   (cons g todo))]))))]
               [(indirected? info)
                ;; Preserve recording of this variable as boxed
                (hash-set! new-lifts v info)
                (loop frees (cdr todo))]
               [else
                ;; Normal variable:
                (loop frees (cdr todo))])])))
      (set-liftable-frees! proc closed-frees))
    ;; Remove references to lifted from free-variable sets, and also
    ;; convert free-variable sets to lists for consistent ordering:
    (for ([proc (in-hash-values new-lifts)]
          #:when (liftable? proc))
      (set-liftable-frees! proc (sort (for/list ([f (in-hash-keys (liftable-frees proc))]
                                                 #:unless (liftable? (hash-ref lifts f #f)))
                                        f)
                                      symbol<?)))
    ;; Copy over empty-closure records:
    (for ([(f info) (in-hash lifts)])
      (when (eq? info '#:empty)
        (hash-set! new-lifts f info)))
    ;; Return new lifts
    new-lifts)

  ;; ----------------------------------------
  ;; Pass 2: convert calls based on previously collected information
    
  (define (convert-lifted-calls-in-expr v lifts frees empties)
    (let convert ([v v])
      (match v
        [`(let . ,_)
         (convert-lifted-calls-in-let v lifts frees empties)]
        [`(letrec . ,_)
         (convert-lifted-calls-in-letrec v lifts frees empties)]
        [`(letrec* . ,_)
         (convert-lifted-calls-in-letrec v lifts frees empties)]
        [`((letrec ([,id ,rhs]) ,rator) ,rands ...)
         (convert (reannotate v `(letrec ([,id ,rhs]) (,rator . ,rands))))]
        [`((letrec* ([,id ,rhs]) ,rator) ,rands ...)
         (convert (reannotate v `(letrec* ([,id ,rhs]) (,rator . ,rands))))]
        [`(lambda ,args . ,body)
         (lift-if-empty
          v lifts empties
          (reannotate v `(lambda ,args . ,(convert-lifted-calls-in-seq/box-mutated body args lifts frees empties))))]
        [`(case-lambda [,argss . ,bodys] ...)
         (lift-if-empty
          v lifts empties
          (reannotate v `(case-lambda
                           ,@(for/list ([args (in-list argss)]
                                        [body (in-list bodys)])
                               `[,args . ,(convert-lifted-calls-in-seq/box-mutated body args lifts frees empties)]))))]
        [`(begin . ,vs)
         (reannotate v `(begin . ,(convert-lifted-calls-in-seq vs lifts frees empties)))]
        [`(begin0 . ,vs)
         (reannotate v `(begin0 . ,(convert-lifted-calls-in-seq vs lifts frees empties)))]
        [`(quote . ,_) v]
        [`(if ,tst ,thn ,els)
         (reannotate v `(if ,(convert tst) ,(convert thn) ,(convert els)))]
        [`(with-continuation-mark ,key ,val ,body)
         (reannotate v `(with-continuation-mark ,(convert key) ,(convert val) ,(convert body)))]
        [`(set! ,id ,rhs)
         (define info (and (hash-ref lifts (unwrap id) #f)))
         (cond
           [(indirected? info)
            (reannotate v (if (indirected-check? info)
                              `(set-box!/check-undefined ,id ,(convert rhs) ',id)
                              `(set-box! ,id ,(convert rhs))))]
           [else
            (reannotate v `(set! ,id ,(convert rhs)))])]
        [`(#%variable-reference . ,_)
         (error 'internal-error "lift: unexpected variable reference")]
        [`(,rator . ,rands)
         (let ([rands (convert-lifted-calls-in-seq rands lifts frees empties)])
           (define f (unwrap rator))
           (cond
             [(and (symbol? f)
                   (let ([p (hash-ref lifts f #f)])
                     (and (liftable? p) p)))
              => (lambda (proc)
                   (reannotate v `(,rator ,@(liftable-frees proc) . ,rands)))]
             [else
              (reannotate v `(,(convert rator) . ,rands))]))]
        [`,_
         (define var (unwrap v))
         (define info (and (symbol? var)
                           (hash-ref lifts var #f)))
         (cond
           [(indirected? info)
            (reannotate v (if (indirected-check? info)
                              `(unbox/check-undefined ,v ',v)
                              `(unsafe-unbox* ,v)))]
           [else v])])))
  
  (define (convert-lifted-calls-in-seq vs lifts frees empties)
    (reannotate vs (for/list ([v (in-wrap-list vs)])
                     (convert-lifted-calls-in-expr v lifts frees empties))))

  (define (convert-lifted-calls-in-let v lifts frees empties)
    (match v
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (define bindings
         (for/list ([id (in-list ids)]
                    [rhs (in-list rhss)]
                    #:unless (liftable? (hash-ref lifts (unwrap id) #f)))
           `[,id ,(let ([rhs (convert-lifted-calls-in-expr rhs lifts frees empties)])
                    (if (indirected? (hash-ref lifts (unwrap id) #f))
                        `(box ,rhs)
                        rhs))]))
       (define new-body
         (convert-lifted-calls-in-seq body lifts frees empties))
       (reannotate
        v
        (rebuild-let let-id bindings new-body))]))

  (define (convert-lifted-calls-in-letrec v lifts frees empties)
    (match v
      [`(,let-id ([,ids ,rhss] ...) . ,body)
       (define pre-bindings
         (for/list ([id (in-list ids)]
                    [rhs (in-list rhss)]
                    #:when (indirected? (hash-ref lifts (unwrap id) #f)))
           `[,id (box unsafe-undefined)]))
       (define bindings
         (for/list ([id (in-list ids)]
                    [rhs (in-list rhss)]
                    #:unless (liftable? (hash-ref lifts (unwrap id) #f)))
           (define new-rhs (convert-lifted-calls-in-expr rhs lifts frees empties))
           (cond
             [(indirected? (hash-ref lifts (unwrap id) #f))
              `[,(gensym) (set-box! ,id ,new-rhs)]]
             [else `[,id ,new-rhs]])))
       (define new-bindings
         (if (null? bindings)
             pre-bindings
             (append pre-bindings bindings)))
       (define new-body
         (convert-lifted-calls-in-seq body lifts frees empties))
       (reannotate
        v
        (rebuild-let let-id new-bindings new-body))]))

  (define (convert-lifted-calls-in-seq/box-mutated vs ids lifts frees empties)
    (let loop ([ids ids])
      (cond
        [(wrap-null? ids)
         (convert-lifted-calls-in-seq vs lifts frees empties)]
        [(wrap-pair? ids)
         (define id (wrap-car ids))
         (if (indirected? (hash-ref lifts (unwrap id) #f))
             `((let ([,id (box ,id)])
                 . ,(loop (wrap-cdr ids))))
             (loop (wrap-cdr ids)))]
        [else (loop (list ids))])))

  ;; Create bindings for lifted functions, adding new arguments
  ;; as the functions are lifted
  (define (extract-lifted-bindings lifts empties)
    (for/list ([(f proc) (in-hash lifts)]
               #:when (liftable? proc))
      (let* ([new-args (liftable-frees proc)]
             [frees (for/hash ([arg (in-list new-args)])
                      (values arg #t))]
             [rhs (liftable-expr proc)])
        `[,f ,(match rhs
                [`(lambda ,args . ,body)
                 (let ([body (convert-lifted-calls-in-seq/box-mutated body args lifts frees empties)])
                   (reannotate rhs `(lambda ,(append new-args args) . ,body)))]
                [`(case-lambda [,argss . ,bodys] ...)
                 (reannotate rhs `(case-lambda
                                    ,@(for/list ([args (in-list argss)]
                                                 [body (in-list bodys)])
                                        (let ([body (convert-lifted-calls-in-seq/box-mutated body args lifts frees empties)])
                                          `[,(append new-args args) . ,body]))))])])))


  ;; ----------------------------------------
  ;; Helpers

  (define (lambda? v)
    (match v
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`,_ #f]))

  (define (immediate? v)
    (match v
      [`(quote . ,_) #t]
      [`(,_ . ,_) #f]
      [`,_
       (not (symbol? (unwrap v)))]))

  (define (consistent-argument-count? proc n)
    (define (consistent? args n)
      (let loop ([args args] [n n])
        (cond
          [(negative? n) #f]
          [(wrap-null? args) (zero? n)]
          [(wrap-pair? args)
           (loop (wrap-cdr args) (sub1 n))]
          [else #t])))
    (match proc
      [`(lambda ,args . ,_)
       (consistent? args n)]
      [`(case-lambda [,argss . ,_] ...)
       (for/or ([args (in-list argss)])
         (consistent? args n))]
      [`,_ #f]))

  ;; Find or create an `indirected` record for a variable
  (define (lookup-indirected-variable lifts var need-check?)
    (define ind (hash-ref lifts var #f))
    (or (and (indirected? ind)
             (begin
               (when need-check?
                 (set-indirected-check?! ind #t))
               ind))
        (let ([ind (indirected need-check?)])
          (hash-set! lifts var ind)
          ind)))

  ;; Add a group of arguments (a list or improper list) to a set
  (define (add-args args s [mode 'ready])
    (let loop ([args args] [s s])
      (cond
        [(wrap-null? args) s]
        [(wrap-pair? args)
         (loop (wrap-cdr args)
               (hash-set s (unwrap (wrap-car args)) mode))]
        [else (hash-set s (unwrap args) mode)])))

  ;; Add a free variable
  (define (add-free frees+binds var)
    (cons (hash-set (car frees+binds) var #t)
          (cdr frees+binds)))

  (define (frees-count frees+binds)
    (hash-count (car frees+binds)))

  ;; Remove a group of arguments (a list or improper list) from a set
  ;; as the variable go out of scope, including any associated mutator
  ;; and variable-reference variables, but keep variables for lifted
  ;; functions
  (define (remove-frees/add-binds args frees+binds lifts)
    (define (remove-free/add-bind frees+binds arg)
      (define info (hash-ref lifts arg #f))
      (cond
        [(liftable? info)
         ;; Since `arg` will be lifted to the top, it
         ;; stays in our local set of free variables,
         ;; but also add it to binds so that callers
         ;; will know that they don't need to chain
         (cons (car frees+binds)
               (hash-set (cdr frees+binds) arg #t))]
        [else (cons (hash-remove (car frees+binds) arg)
                    (hash-set (cdr frees+binds) arg #t))]))
    (let loop ([args args] [frees+binds frees+binds])
      (cond
        [(wrap-null? args) frees+binds]
        [(wrap-pair? args)
         (loop (wrap-cdr args)
               (remove-free/add-bind frees+binds (unwrap (wrap-car args))))]
        [else (remove-free/add-bind frees+binds (unwrap args))])))

  ;; Set union
  (define (union s1 s2)
    (cond
      [((hash-count s1) . > . (hash-count s2))
       (union s2 s1)]
      [else
       (for/fold ([s2 s2]) ([k (in-hash-keys s1)])
         (hash-set s2 k #t))]))

   (define (rebuild-let let-id bindings body)
     (cond
       [(not (null? bindings))
        `(,let-id ,bindings . ,body)]
       [(and (pair? body) (null? (cdr body)))
        (car body)]
       [else `(begin . ,body)]))

  (define (record-empty-closure! lifts v)
    (hash-set! lifts v '#:empty))

  (define (lift-if-empty v lifts empties new-v)
    (cond
      [(hash-ref lifts v #f)
       (define id (gensym 'procz))
       (set-box! empties (cons `[,id ,new-v] (unbox empties)))
       id]
      [else new-v]))
   
  ;; ----------------------------------------
  ;; Go
  
  (if (lift-in? v)
      (lift-in v)
      v))
