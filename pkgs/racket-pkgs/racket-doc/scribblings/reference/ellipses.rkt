(module ellipses scheme/base
  (require scribble/manual
           "ellipses-defn.rkt"
           (for-label "ellipses-defn.rkt"))

  (provide ellipses-defn
           ellipses-id)

  (define-syntax ellipses-defn
    (syntax-rules ()
      [(_ . body)
       (defidform (... ...) . body)]))

  (define ellipses-id
    (racket ...)))
