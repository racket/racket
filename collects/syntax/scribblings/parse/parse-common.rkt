#lang racket/base
(require scribble/manual
         scribble/eval
         racket/sandbox)

(provide ellipses
         the-eval
         myexamples
         myinteraction)

(define ellipses (racket ...))

(define (fixup exn)
  (let ([src (ormap values (exn:fail:syntax-exprs exn))])
    (if src
        (make-exn:fail:syntax
         (format "~a at: ~s" (exn-message exn) (syntax->datum src))
         (exn-continuation-marks exn)
         (exn:fail:syntax-exprs exn))
        exn)))
(define the-eval
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-propagate-breaks #f]
                    [sandbox-eval-handlers
                     (list #f
                           (lambda (thunk)
                             (with-handlers ([exn:fail:syntax?
                                              (lambda (e) (raise (fixup e)))])
                               (thunk))))])
       (make-evaluator 'racket/base
                       #:requires (let ([mods '(syntax/parse
                                                syntax/parse/debug
                                                syntax/parse/experimental/splicing
                                                syntax/parse/experimental/contract
                                                syntax/parse/experimental/reflect
                                                syntax/parse/experimental/specialize
                                                syntax/parse/experimental/template
                                                syntax/parse/experimental/eh)])
                                    `((for-syntax racket/base ,@mods)
                                      ,@mods)))))))
(the-eval '(error-print-source-location #f))

(define-syntax-rule (myexamples e ...)
  (examples #:eval the-eval e ...))

(define-syntax-rule (myinteraction e ...)
  (interaction #:eval the-eval e ...))

;; ----

(define Spattern "single-term pattern")
(define Lpattern "list pattern")
(define Hpattern "head pattern")
(define EHpattern "ellipsis-head pattern")
(define Apattern "action pattern")

(define Spatterns "single-term patterns")
(define Lpatterns "list patterns")
(define Hpatterns "head patterns")
(define EHpatterns "ellipsis-head patterns")
(define Apatterns "action patterns")

(provide Spattern
         Lpattern
         Hpattern
         EHpattern
         Apattern
         Spatterns
         Lpatterns
         Hpatterns
         EHpatterns
         Apatterns)

;; ----

(define-syntax-rule (defhere id) (defidentifier #'id #:form? #t))

(define-syntax ref
  (syntax-rules ()
    [(ref id suffix ...)
     (elemref (list 'pattern-link (list 'id 'suffix ...))
              (racketkeywordfont (symbol->string 'id))
              (superscript (symbol->string 'suffix)) ...
              #:underline? #f)]))
(define-syntax def
  (syntax-rules ()
    [(def id suffix ...)
     (elemtag (list 'pattern-link (list 'id 'suffix ...))
              (racket id)
              #|(superscript (symbol->string 'suffix)) ...|# )]))

(provide defhere
         ref
         def)

;; ----

(require (for-label racket/base
                    racket/contract
                    (except-in syntax/parse ...+)
                    syntax/parse/debug
                    syntax/parse/experimental/contract
                    syntax/parse/experimental/splicing
                    syntax/parse/experimental/reflect
                    syntax/parse/experimental/provide
                    syntax/parse/experimental/specialize
                    syntax/parse/experimental/template
                    syntax/parse/experimental/eh
                    "parse-dummy-bindings.rkt"))
(provide (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)
                    (all-from-out syntax/parse)
                    (all-from-out syntax/parse/debug)
                    (all-from-out syntax/parse/experimental/contract)
                    (all-from-out syntax/parse/experimental/splicing)
                    (all-from-out syntax/parse/experimental/reflect)
                    (all-from-out syntax/parse/experimental/provide)
                    (all-from-out syntax/parse/experimental/specialize)
                    (all-from-out syntax/parse/experimental/template)
                    (all-from-out syntax/parse/experimental/eh)
                    (all-from-out "parse-dummy-bindings.rkt")))
