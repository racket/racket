(module ellipses (lib "lang.ss" "big")
  (require (lib "manual.ss" "scribble")
           "ellipses-defn.ss")
  (require-for-label "ellipses-defn.ss")

  (provide ellipses-defn
           ellipses-id)

  (define-syntax ellipses-defn
    (syntax-rules ()
      [(_ . body)
       (defidform (... ...) . body)]))

  (define ellipses-id
    (scheme ...)))
