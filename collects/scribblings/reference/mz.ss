(module mz mzscheme
  (require (lib "struct.ss" "scribble")
           (lib "manual.ss" "scribble")
           (lib "eval.ss" "scribble")
           (lib "decode.ss" "scribble")
           (lib "kw.ss"))

  (provide (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "eval.ss" "scribble")))

  (define (*exnraise s)
    (make-element #f (list s " exception is raised")))
  (define-syntax exnraise
    (syntax-rules ()
      [(_ s) (*exnraise (scheme s))]))
  (define-syntax Exn
    (syntax-rules ()
      [(_ s) (scheme s)]))
  (provide exnraise Exn)

  (provide Guide guideintro)

  (define Guide
    (italic (link "../guide/index.html" "A Guide to PLT Scheme")))

  (define/kw (guideintro tag #:body s)
    (apply margin-note
           (decode-content (append (list "For an introduction to ")
                                   s
                                   (list ", see "
                                         (secref tag)
                                         " in "
                                         Guide
                                         "."))))))
