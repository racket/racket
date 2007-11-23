(module mz scheme/base
  (require scribble/struct
           scribble/manual
           scribble/eval
           scribble/decode
           scheme/contract
           "../icons.ss")

  (provide (all-from-out scribble/manual)
           (all-from-out scribble/eval)
           (all-from-out scheme/contract))

  (require (for-label scheme
                      "to-do.ss"))
  (provide (for-label (all-from-out scheme)
                      (all-from-out "to-do.ss")))

  (define AllUnix "Unix and Mac OS X")
  (provide AllUnix)

  (provide note-lib)
  (define-syntax-rule (note-lib lib)
    (t "The bindings documented in this section are provided by the "
       (schememodname lib)
       " and "
       (schememodname scheme)
       " libraries, but not " (schememodname scheme/base)
       "."))

  (provide note-lib-only)
  (define-syntax-rule (note-lib-only lib)
    (t "The bindings documented in this section are provided by the "
       (schememodname lib)
       " library, not " (schememodname scheme/base)
       " or " (schememodname scheme)
       "."))

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

  (define (refalso tag . s)
    (apply margin-note
           (decode-content (append (list magnify (secref tag) " also provides information on ")
                                   s
                                   (list ".")))))

  (define (moreref tag . s)
    (apply margin-note
           (decode-content (append (list magnify (secref tag) " provides more information on ")
                                   s
                                   (list ".")))))
  
  (define (guidesecref s)
    (secref #:doc '(lib "scribblings/guide/guide.scrbl") s))

  (define (guideintro tag . s)
    (apply margin-note
           (decode-content (append (list finger (guidesecref tag) " in " Guide " introduces ")
                                   s
                                   (list ".")))))

  (define Guide
    (italic (guidesecref "top"))))

