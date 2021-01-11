#lang racket/base

(require "../../parse.rkt"
         racket/dict)

(provide function-header formal formals)

(define-syntax-class function-header
  #:attributes (name params args)
  (pattern ((~or header:function-header name*:id) . args:formals)
           #:attr params #'((~@ . (~? header.params ())) . args.params)
           #:attr name   #'(~? header.name name*)))

(define-syntax-class formals
  #:attributes (params)
  (pattern (arg:formal ...)
           #:attr params #'(arg.name ...)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
                       "duplicate argument name"
           #:fail-when (check-duplicates (attribute arg.kw) #:key syntax-e)
                       "duplicate keyword for argument"
           #:fail-when (invalid-option-placement
                        (attribute arg.kw) (attribute arg.name) (attribute arg.default))
                       "default-value expression missing")
  (pattern (arg:formal ... . rest:id)
           #:attr params #'(arg.name ... rest)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
                       "duplicate argument name"
           #:fail-when (check-duplicates (attribute arg.kw) #:key syntax-e)
                       "duplicate keyword for argument"
           #:fail-when (invalid-option-placement
                        (attribute arg.kw) (attribute arg.name) (attribute arg.default))
                       "default-value expression missing"))

(define-splicing-syntax-class formal
  #:attributes (name kw default)
  (pattern name:id
           #:attr kw #f
           #:attr default #f)
  (pattern [name:id default]
           #:attr kw #f)
  (pattern (~seq kw:keyword name:id)
           #:attr default #f)
  (pattern (~seq kw:keyword [name:id default])))

;; invalid-option-placement : (Listof Id) (Listof Syntax/#f) -> Id/#f
;; Checks for mandatory argument after optional argument; if found, returns
;; identifier of mandatory argument.
(define (invalid-option-placement kws names defaults)
  ;; find-mandatory : (Listof Id) (Listof Syntax/#f) -> Id/#f
  ;; Finds first name w/o corresponding default.
  (define (find-mandatory kws names defaults)
    (for/first ([kw (in-list kws)]
                [name (in-list names)]
                [default (in-list defaults)]
                #:when (and (not kw) (not default)))
      name))
  ;; Skip through mandatory args until first optional found, then search
  ;; for another mandatory.
  (let loop ([kws kws] [names names] [defaults defaults])
    (cond [(or (null? names) (null? defaults))
           #f]
          [(eq? (car defaults) #f) ;; mandatory
           (loop (cdr kws) (cdr names) (cdr defaults))]
          [else ;; found optional
           (find-mandatory (cdr kws) (cdr names) (cdr defaults))])))
