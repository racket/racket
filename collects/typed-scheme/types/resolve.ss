#lang scheme/base
(require "../utils/utils.ss")

(require (rep type-rep)  
	 (env type-name-env)
	 (utils tc-utils)
         (types utils)
         scheme/match
         mzlib/trace)

(provide resolve-name resolve-app needs-resolving? resolve-once resolve)

(define (resolve-name t)
  (match t
    [(Name: n) (lookup-type-name n)]
    [_ (int-err "resolve-name: not a name ~a" t)]))

(define (resolve-app rator rands stx)
  (parameterize ([current-orig-stx stx])
    (match rator
      [(Poly: _ _)
       (instantiate-poly rator rands)]
      [(Name: _) (resolve-app (resolve-name rator) rands stx)]
      [(Mu: _ _) (resolve-app (unfold rator) rands)]
      [(App: r r* s) (resolve-app (resolve-app r r* s) rands)]
      [_ (tc-error "resolve-app: not a proper operator ~a" rator)])))

(define (needs-resolving? t)
  (or (Mu? t) (App? t) (Name? t)))

(define (resolve-once t)
  (match t
    [(Mu: _ _) (unfold t)]
    [(App: r r* s) (resolve-app r r* s)]
    [(Name: _) (resolve-name t)]))

(define (resolve t)
  (if (needs-resolving? t) (resolve-once t) t))
