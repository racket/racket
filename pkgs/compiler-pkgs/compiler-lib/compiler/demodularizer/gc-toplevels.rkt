#lang racket/base

(require racket/match
         racket/list
         racket/dict
         racket/contract
         compiler/zo-parse
         "util.rkt")

; XXX Use efficient set structure
(define (gc-toplevels top)
  (match top
    [(struct compilation-top (max-let-depth top-prefix form))
     (define lift-start
       (prefix-lift-start top-prefix))
     (define max-depgraph-index 
       (+ (prefix-num-lifts top-prefix)
          lift-start))
     (define top-node max-depgraph-index)
     (define DEP-GRAPH (make-vector (add1 top-node) (make-refs empty empty)))
     (define build-graph! (make-build-graph! DEP-GRAPH))
     (define _void (build-graph! (list top-node) form))
     (define-values (used-tls stxs) (graph-dfs DEP-GRAPH top-node))
     (define ordered-used-tls (sort (rest used-tls) <=)) ; This rest drops off the top-node
     (define ordered-stxs (sort stxs <=))
     (define (lift? i) (lift-start . <= . i))
     (define-values (lifts normal-tls) (partition lift? ordered-used-tls))
     (define new-prefix
       (make-prefix 
        (length lifts)
        (for/list ([i normal-tls])
          (list-ref (prefix-toplevels top-prefix) i))
        (for/list ([i ordered-stxs])
          (list-ref (prefix-stxs top-prefix) i))))
     (define new-lift-start
       (prefix-lift-start new-prefix))
     ; XXX This probably breaks max-let-depth
     (define new-form
       ((gc-toplevels-form 
         (lambda (pos) (index<=? pos ordered-used-tls))
         (lambda (pos)
           (if (lift? pos)
               (+ new-lift-start (index<=? pos lifts))
               (index<=? pos normal-tls)))
         (lambda (stx-pos)
           (index<=? stx-pos ordered-stxs))
         (prefix-syntax-start new-prefix))
        form))
     (log-debug (format "Total TLS: ~S" (length normal-tls)))
     (log-debug (format "Used TLS: ~S" normal-tls))
     (log-debug (format "Total lifts: ~S" (length lifts)))
     (log-debug (format "Used lifts: ~S" lifts))
     (log-debug (format "Total stxs: ~S" (length stxs)))
     (log-debug (format "Used stxs: ~S" ordered-stxs))
     (make-compilation-top
      max-let-depth
      new-prefix
      new-form)]))

(define-struct refs (tl stx) #:transparent)

(define (make-build-graph! DEP-GRAPH)
  (define (build-graph!* form lhs)
    (match form
      [(struct def-values (ids rhs))
       (define new-lhs (map toplevel-pos ids))
       ; If we require one, we should require all, so make them reference each other
       (for-each (lambda (tl) (build-graph! new-lhs tl)) ids)
       (build-graph! new-lhs rhs)]
      [(? def-syntaxes?)
       (error 'build-graph "Doesn't handle syntax")]
      [(? seq-for-syntax?)
       (error 'build-graph "Doesn't handle syntax")]
      [(struct inline-variant (direct inline))
       (build-graph! lhs direct)]
      [(struct req (reqs dummy))
       (build-graph! lhs dummy)]
      [(? mod?)
       (error 'build-graph "Doesn't handle modules")]
      [(struct seq (forms))
       (for-each (lambda (f) (build-graph! lhs f)) forms)]
      [(struct splice (forms))
       (for-each (lambda (f) (build-graph! lhs f)) forms)]
      [(and l (struct lam (name flags num-params param-types rest? closure-map closure-types tl-map max-let-depth body)))
       (build-graph! lhs body)]
      [(and c (struct closure (code gen-id)))
       (build-graph! lhs code)]
      [(and cl (struct case-lam (name clauses)))
       (for-each (lambda (l) (build-graph! lhs l))
                 clauses)]
      [(struct let-one (rhs body flonum? unused?))
       (build-graph! lhs rhs)
       (build-graph! lhs body)]
      [(and f (struct let-void (count boxes? body)))
       (build-graph! lhs body)]
      [(and f (struct install-value (_ _ _ rhs body)))
       (build-graph! lhs rhs)
       (build-graph! lhs body)]
      [(struct let-rec (procs body))
       (for-each (lambda (l) (build-graph! lhs l)) procs)
       (build-graph! lhs body)]
      [(and f (struct boxenv (_ body)))
       (build-graph! lhs body)]
      [(and f (struct toplevel (_ pos _ _)))
       (for-each (lambda (lhs)
                   (dict-update! DEP-GRAPH lhs
                                 (match-lambda 
                                   [(struct refs (tls stxs))
                                    (make-refs (list* pos tls) stxs)])))
                 lhs)]
      [(and f (struct topsyntax (_ pos _)))
       (for-each (lambda (lhs)
                   (dict-update! DEP-GRAPH lhs
                                 (match-lambda 
                                   [(struct refs (tls stxs))
                                    (make-refs tls (list* pos stxs))])))
                 lhs)]
      [(struct application (rator rands))
       (for-each (lambda (f) (build-graph! lhs f))
                 (list* rator rands))]
      [(struct branch (test then else))
       (for-each (lambda (f) (build-graph! lhs f))
                 (list test then else))]
      [(struct with-cont-mark (key val body))
       (for-each (lambda (f) (build-graph! lhs f))
                 (list key val body))]
      [(struct beg0 (seq))
       (for-each (lambda (f) (build-graph! lhs f))
                 seq)]
      [(struct varref (tl dummy))
       (build-graph! lhs tl)
       (build-graph! lhs dummy)]
      [(and f (struct assign (id rhs undef-ok?)))
       (build-graph! lhs id)
       (build-graph! lhs rhs)]
      [(struct apply-values (proc args-expr))
       (build-graph! lhs proc)
       (build-graph! lhs args-expr)]
      [(and f (struct primval (id)))
       (void)]
      [(and f (struct localref (unbox? pos clear? other-clears? type)))
       (void)]
      [(and v (not (? form?)))
       (void)]))
  (define-values (first-build-graph!** build-graph!**) 
    (build-form-memo build-graph!* #:void? #t))
  (define (build-graph! lhs form) (first-build-graph!** form lhs))
  build-graph!)

(define (graph-dfs g start-node)
  (define visited? (make-hasheq))
  (define (visit-tl n tls stxs)
    (if (hash-has-key? visited? n)
        (values tls stxs)
        (match (dict-ref g n)
          [(struct refs (n-tls n-stxs))
           (hash-set! visited? n #t)
           (define-values (new-tls1 new-stxs1)
             (for/fold ([new-tls tls]
                        [new-stxs stxs])
                       ([tl (in-list n-tls)])
               (visit-tl tl new-tls new-stxs)))
           (define new-stxs2
             (for/fold ([new-stxs new-stxs1])
                       ([stx (in-list n-stxs)])
               (define this-stx (visit-stx stx))
               (if this-stx
                   (list* this-stx new-stxs)
                   new-stxs)))
           (values (list* n new-tls1)
                   new-stxs2)])))
  (define stx-visited? (make-hasheq))
  (define (visit-stx n)
    (if (hash-has-key? stx-visited? n)
        #f
        (begin (hash-set! stx-visited? n #t)
               n)))
  (visit-tl start-node empty empty))

; index<=? : number? (listof number?) -> (or/c number? false/c)
; returns the index of n in l and assumes that l is sorted by <=
(define (index<=? n l)
  (match l
    [(list) #f]
    [(list-rest f l)
     (cond
       [(= n f)
        0]
       [(< n f)
        #f]
       [else
        (let ([rec (index<=? n l)])
          (if rec (add1 rec) rec))])]))

(define (identity x) x)
(define (gc-toplevels-form keep? update-tl update-ts new-ts-midpt)
  (define (inner-update form)
    (match form
      [(struct def-values (ids rhs))
       (if (ormap (compose keep? toplevel-pos) ids)
           (make-def-values (map update ids)
                            (update rhs))
           #f)]
      [(? def-syntaxes?)
       (error 'gc-tls "Doesn't handle syntax")]
      [(? seq-for-syntax?)
       (error 'gc-tls "Doesn't handle syntax")]
      [(struct req (reqs dummy))
       (make-req reqs (update dummy))]
      [(? mod?)
       (error 'gc-tls "Doesn't handle modules")]
      [(struct seq (forms))
       (make-seq (filter identity (map update forms)))]
      [(struct splice (forms))
       (make-splice (filter identity (map update forms)))]
      [(and l (struct lam (name flags num-params param-types rest? closure-map closure-types tl-map max-let-depth body)))
       (struct-copy lam l
                    [toplevel-map #f] ; consevrative
                    [body (update body)])]
      [(and c (struct closure (code gen-id)))
       (struct-copy closure c
                    [code (update code)])]
      [(and cl (struct case-lam (name clauses)))
       (struct-copy case-lam cl
                    [clauses (map update clauses)])]
      [(struct let-one (rhs body type unused?))
       (make-let-one (update rhs) (update body) type unused?)]
      [(and f (struct let-void (count boxes? body)))
       (struct-copy let-void f
                    [body (update body)])]
      [(and f (struct install-value (_ _ _ rhs body)))
       (struct-copy install-value f
                    [rhs (update rhs)]
                    [body (update body)])]
      [(struct let-rec (procs body))
       (make-let-rec (map update procs) (update body))]
      [(and f (struct boxenv (_ body)))
       (struct-copy boxenv f [body (update body)])]
      [(and f (struct toplevel (_ pos _ _)))
       (struct-copy toplevel f
                    [pos (update-tl pos)])]
      [(and f (struct topsyntax (_ pos _)))
       (struct-copy topsyntax f
                    [pos (update-ts pos)]
                    [midpt new-ts-midpt])]
      [(struct application (rator rands))
       (make-application 
        (update rator)
        (map update rands))]
      [(struct branch (test then else))
       (make-branch 
        (update test)
        (update then)
        (update else))]
      [(struct with-cont-mark (key val body))
       (make-with-cont-mark 
        (update key)
        (update val)
        (update body))]
      [(struct beg0 (seq))
       (make-beg0 (map update seq))]
      [(struct varref (tl dummy))
       (make-varref (update tl) (update dummy))]
      [(and f (struct assign (id rhs undef-ok?)))
       (struct-copy assign f
                    [id (update id)]
                    [rhs (update rhs)])]
      [(struct apply-values (proc args-expr))
       (make-apply-values
        (update proc)
        (update args-expr))]
      [(and f (struct primval (id)))
       f]
      [(and f (struct localref (unbox? pos clear? other-clears? type)))
       f]
      [(and v (not (? form?)))
       v]
      ))
  (define-values (first-update update)
    (build-form-memo inner-update))
  first-update)

(provide/contract
 [gc-toplevels (compilation-top? . -> . compilation-top?)])
