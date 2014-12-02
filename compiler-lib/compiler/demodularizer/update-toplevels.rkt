#lang racket/base

(require racket/match
         racket/contract
         compiler/zo-structs
         "util.rkt")

(define (update-toplevels toplevel-updater topsyntax-updater topsyntax-new-midpt)
  (define (inner-update form)
    (match form
      [(struct def-values (ids rhs))
       (make-def-values (map update ids)
                        (update rhs))]
      [(? def-syntaxes?)
       (error 'increment "Doesn't handle syntax")]
      [(? seq-for-syntax?)
       (error 'increment "Doesn't handle syntax")]
      [(struct inline-variant (direct inline))
       (update direct)]
      [(struct req (reqs dummy))
       (make-req reqs (update dummy))]
      [(? mod?)
       (error 'increment "Doesn't handle modules")]
      [(struct seq (forms))
       (make-seq (map update forms))]
      [(struct splice (forms))
       (make-splice (map update forms))]
      [(and l (struct lam (name flags num-params param-types rest? closure-map closure-types tl-map max-let-depth body)))
       (struct-copy lam l
                    [toplevel-map #f] ; conservative
                    [body (update body)])]
      [(and c (struct closure (code gen-id)))
       (struct-copy closure c
                    [code (update code)])]
      [(and cl (struct case-lam (name clauses)))
       (define new-clauses
         (map update clauses))
       (struct-copy case-lam cl
                    [clauses new-clauses])]
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
                    [pos (toplevel-updater pos)])]
      [(and f (struct topsyntax (_ pos _)))
       (struct-copy topsyntax f
                    [pos (topsyntax-updater pos)]
                    [midpt topsyntax-new-midpt])]
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
      [(and f (not (? form?)))
       f]
      ))
  (define-values (first-update update)
    (build-form-memo inner-update))
  first-update)

(provide/contract
 [update-toplevels 
  ((exact-nonnegative-integer? . -> . exact-nonnegative-integer?)
   (exact-nonnegative-integer? . -> . exact-nonnegative-integer?)
   exact-nonnegative-integer?
   . -> .
   (form? . -> . form?))])
