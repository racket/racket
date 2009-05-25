#lang scheme/base
(require "../utils/utils.ss")

(require (rep type-rep)  
	 (env type-name-env)
	 (utils tc-utils)
         (types utils)
         scheme/match
         scheme/contract
         mzlib/trace)

(provide resolve-name resolve-app needs-resolving? resolve)
(p/c [resolve-once (Type/c . -> . (or/c Type/c #f))])

(define (resolve-name t)
  (match t
    [(Name: n) (let ([t (lookup-type-name n)])
                 (if (Type? t) t #f))]
    [_ (int-err "resolve-name: not a name ~a" t)]))

(define (resolve-app rator rands stx)
  (parameterize ([current-orig-stx stx])
    (match rator
      [(Poly-unsafe: n _)
       (unless (= n (length rands))
         (tc-error "wrong number of arguments to polymorphic type: expected ~a and got ~a"
                   n (length rands)))
       (instantiate-poly rator rands)]
      [(Name: _) (let ([r (resolve-name rator)])
                   (and r (resolve-app r rands stx)))]
      [(Mu: _ _) (resolve-app (unfold rator) rands)]
      [(App: r r* s) (resolve-app (resolve-app r r* s) rands)]
      [_ (tc-error "cannot apply a non-polymorphic type: ~a" rator)])))

(define (needs-resolving? t)
  (or (Mu? t) (App? t) (Name? t)))

(define (resolve-once t)
  (match t
    [(Mu: _ _) (unfold t)]
    [(App: r r* s) (resolve-app r r* s)]
    [(Name: _) (resolve-name t)]))

(define (resolve t)
  (if (needs-resolving? t) (resolve-once t) t))

;(trace resolve-app)
