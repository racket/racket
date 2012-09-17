#lang racket/base

(require racket/class
         racket/match
         (for-syntax racket/base)
         (for-syntax syntax/parse
                     syntax/parse/experimental/template))

(provide match? as object)

(define-syntax-rule (match? e p ...)
  (match e [p #t] ... [_ #f]))

(define-match-expander as
  (syntax-rules ()
    [(as ([x e] ...) p ...) (and (app (lambda (y) e) x) ... p ...)]))

;; Added by asumu
;; Comprehensive `define` procedure form for match
(provide define/match)

(begin-for-syntax
  (define-syntax-class function-header
    (pattern ((~or header:function-header name:id) . args:args)
             #:attr params
             (template ((?@ . (?? header.params ()))
                        . args.params))))

  (define-syntax-class args
    (pattern (arg:arg ...)
             #:attr params #'(arg.name ...))
    (pattern (arg:arg ... . rest:id)
             #:attr params #'(arg.name ... rest)))

  (define-splicing-syntax-class arg
    #:attributes (name)
    (pattern name:id)
    (pattern [name:id default])
    (pattern (~seq kw:keyword name:id))
    (pattern (~seq kw:keyword [name:id default]))))

(define-syntax (define/match stx)
  (syntax-parse stx
    [(_ ?header:function-header ?clause ...)
     (template
      (define ?header
        (match* (?? ?header.params)
          ?clause ...)))]))

;; Added by asumu
;; Match expander for objects from racket/class
(define-match-expander object
  (λ (stx)
    (define-syntax-class field
      #:attributes (name pat)
      (pattern
        ((~datum field)
         name
         (~optional pat #:defaults ([pat #'name])))))

    (syntax-parse stx
      [(object f:field ...)
       #'(and (? object?)
              (and (? (λ (o) (field-bound? f.name o)))
                   (app (λ (o) (get-field f.name o))
                        f.pat))
              ...)]
      [(object class f:field ...)
       #'(and (? (λ (o) (is-a? o class)))
              (object f ...))])))
