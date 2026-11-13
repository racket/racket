#lang racket/base

;; Attaching a module to a namespace while it is current tells the
;; module name resolver about the namespace's registry; the resolver
;; must not keep that registry reachable
(define-namespace-anchor a)
(define src (namespace-anchor->empty-namespace a))

(define wb
  (let ([ns (make-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-attach-module src 'racket/base))
    (make-weak-box (namespace-module-registry ns))))

(collect-garbage)

(and (not (weak-box-value wb))
     'ok-42)
