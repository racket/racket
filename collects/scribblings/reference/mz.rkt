(module mz racket/base
  (require scribble/struct
           scribble/manual
           scribble/eval
           scribble/decode
           racket/contract
           "../icons.ss")
  
  (provide (all-from-out scribble/manual)
           (all-from-out scribble/eval)
           (all-from-out racket/contract))
  
  (require (for-label racket))
  (provide (for-label (all-from-out racket)))
  
  (provide mz-examples)
  (define mz-eval (make-base-eval))
  (define-syntax mz-examples
    (syntax-rules ()
      [(_ #:eval . rest)
       (examples #:eval . rest)]
      [(_ . rest)
       (examples #:eval mz-eval . rest)]))
  
  (define AllUnix "Unix and Mac OS X")
  (provide AllUnix)
  
  (provide note-lib)
  (define-syntax note-lib
    (syntax-rules ()
      [(_ lib #:use-sources (src ...) . more)
       (begin
         (declare-exporting lib racket #:use-sources (src ...))
         (defmodule*/no-declare (lib)
           (t (make-collect-element
               #f null
               (lambda (ci)
                 (collect-put! ci `(scheme-extra-lib ,'lib) (schememodname lib))))
              "The bindings documented in this section are provided by the "
              (schememodname lib)
              " and "
              (schememodname racket)
              " libraries, but not " (schememodname racket/base)
              "."
              . more)))]
      [(_ lib . more)
       (note-lib lib #:use-sources () . more)]))
  
  (provide note-init-lib)
  (define-syntax note-init-lib
    (syntax-rules ()
      [(_ lib #:use-sources (src ...) . more)
       (begin
         (declare-exporting lib racket/init #:use-sources (src ...))
         (defmodule*/no-declare (lib)
           (t "The bindings documented in this section are provided by the "
              (schememodname lib)
              " and "
              (schememodname racket/init)
              " libraries, which means that they are available when "
              " the Racket executable is started with no command-line arguments."
              " They are not provided by " (schememodname racket/base)
              " or " (schememodname racket) "."
              . more)))]
      [(_ lib . more)
       (note-init-lib lib #:use-sources () . more)]))
  
  (provide note-lib-only)
  (define-syntax note-lib-only
    (syntax-rules ()
      [(_ lib #:use-sources (src ...) . more)
       (defmodule lib #:use-sources (src ...)
         (t "The bindings documented in this section are provided by the "
            (schememodname lib)
            " library, not " (schememodname racket/base)
            " or " (schememodname racket)
            "."
            . more))]
      [(_ lib . more)
       (note-lib-only lib #:use-sources () . more)]))
  
  (define (*exnraise s)
    (make-element #f (list s " exception is raised")))
  (define-syntax exnraise
    (syntax-rules ()
      [(_ s) (*exnraise (scheme s))]))
  (define-syntax Exn
    (syntax-rules ()
      [(_ s) (scheme s)]))
  (provide exnraise Exn)
  
  (provide margin-note/ref
           refalso moreref Guide guideintro guidealso guidesecref
           HonuManual)
  
  (define (margin-note/ref . s)
    (apply margin-note
           (decode-content (cons magnify s))))
  
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

  (define (guidealso tag)
    (apply margin-note
           (decode-content (append (list finger "See also " (guidesecref tag) " in " Guide ".")))))
  
  (define Guide
    (other-manual '(lib "scribblings/guide/guide.scrbl")))
  
  (define HonuManual
    (other-manual '(lib "scribblings/honu/honu.scrbl")))
  
  (provide speed)
  (define-syntax speed
    (syntax-rules ()
      [(_ id what)
       (t "An " (scheme id) " application can provide better performance for "
          (elem what)
          " iteration when it appears directly in a " (scheme for) " clause.")])))
