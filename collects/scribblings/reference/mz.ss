(module mz mzscheme
  (require (lib "struct.ss" "scribble")
           (lib "manual.ss" "scribble")
           (lib "eval.ss" "scribble")
           (lib "decode.ss" "scribble")
           (lib "kw.ss")
           (lib "contract.ss")
           "../icons.ss")

  (provide (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "eval.ss" "scribble"))
           (all-from (lib "contract.ss")))

  (define AllUnix "Unix and Mac OS X")
  (provide AllUnix)

  (define (*exnraise s)
    (make-element #f (list s " exception is raised")))
  (define-syntax exnraise
    (syntax-rules ()
      [(_ s) (*exnraise (scheme s))]))
  (define-syntax Exn
    (syntax-rules ()
      [(_ s) (scheme s)]))
  (provide exnraise Exn)

  (provide refalso moreref Guide guideintro)

  (define/kw (refalso tag #:body s)
    (apply margin-note
           (decode-content (append (list magnify (secref tag) " also provides information on ")
                                   s
                                   (list ".")))))

  (define/kw (moreref tag #:body s)
    (apply margin-note
           (decode-content (append (list magnify (secref tag) " provides more information on ")
                                   s
                                   (list ".")))))

  (define Guide
    (italic (link "../guide/index.html" "A Guide to PLT Scheme")))

  (define/kw (guideintro tag #:body s)
    (apply margin-note
           (decode-content (append (list finger (secref tag) " in " Guide " introduces ")
                                   s
                                   (list "."))))))

