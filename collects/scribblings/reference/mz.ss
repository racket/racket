(module mz (lib "lang.ss" "big")
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

  (require-for-label (lib "lang.ss" "big")
                     "to-do.ss")
  (provide-for-label (all-from (lib "lang.ss" "big"))
                     (all-from "to-do.ss"))

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

  (provide refalso moreref Guide guideintro guidesecref)

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

  (define (guidesecref s)
    (secref #:doc '(lib "guide.scrbl" "scribblings" "guide") s))

  (define/kw (guideintro tag #:body s)
    (apply margin-note
           (decode-content (append (list finger (guidesecref tag) " in " Guide " introduces ")
                                   s
                                   (list ".")))))

  (define Guide
    (italic (guidesecref "top"))))

