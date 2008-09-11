#lang scheme/base

(require "type-rep.ss"  "type-name-env.ss" "tc-utils.ss"
         "type-utils.ss"
         mzlib/plt-match
         mzlib/trace)

(provide resolve-name resolve-app needs-resolving? resolve-once)

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

#|

(define (resolve-tc-result tcr)
  (match tcr
    [(tc-result: t e1s e2s)
     (ret (resolve-type t) (map resolve-effect e1s) (map resolve-effect e2s))]))

(define (resolve-effect* e)
  (effect-case resolve-type resolve-effect e))



(define (resolve-type* t)
  (define (int t)
    (type-case resolve-type t
               [#:Name stx (lookup-type-name stx)]
               [#:Poly #:matcher Poly: names body (make-Poly names (resolve-type body))]
               [#:Mu #:matcher Mu: name body (make-Mu name (resolve-type body))]
               [#:App rator rands stx
                      (let ([rator (resolve-type rator)]
                            [rands (map resolve-type rands)])
                        (unless (Poly? rator)
                          (tc-error/stx stx "Cannot apply non-polymorphic type: ~a, arguments were: ~a" rator rands))
                        (instantiate-poly rator rands))]))             
  (let loop ([t (int t)])
    (if (or (Name? t) (App? t))
        (loop (resolve-type t))
        t)))

(define table (make-hash-table))

(define (resolve-type t)
  (hash-table-get table t
                  (lambda () (let ([v (resolve-type* t)])
                               (hash-table-put! table t v)
                               v))))

(define (resolve-effect t)
  (hash-table-get table t
                  (lambda () (let ([v (resolve-effect* t)])
                               (hash-table-put! table t v)
                               v))))

;(trace resolve-type)

|#
