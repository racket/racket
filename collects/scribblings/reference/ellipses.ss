(module ellipses (lib "lang.ss" "big")
  (require (lib "manual.ss" "scribble"))

  (provide ellipses-defn
           ellipses-id)

  (define-syntax ... #'no)

  (define-syntax ellipses-defn
    (syntax-rules ()
      [(_ . body)
       (defidform (... ...) . body)]))

  (define ellipses-id
    (scheme ...)))
