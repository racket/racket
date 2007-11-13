(module ellipses scheme/base
  (require scribble/manual
           "ellipses-defn.ss"
           (for-label "ellipses-defn.ss"))

  (provide ellipses-defn
           ellipses-id)

  (define-syntax ellipses-defn
    (syntax-rules ()
      [(_ . body)
       (defidform (... ...) . body)]))

  (define ellipses-id
    (scheme ...)))
