#lang racket/base

(require syntax/parse/pre
         racket/list)

(provide function-header formal formals formals-no-rest)

(define-syntax-class function-header
  #:attributes (name params args)
  (pattern ((~or header:function-header name*:id) . args:formals)
           #:attr params #'((~@ . (~? header.params ())) . args.params)
           #:attr name   #'(~? header.name name*)))

(define-splicing-syntax-class formals-no-rest
  #:attributes (params)
  (pattern (~seq arg:formal ...)
           #:attr params #'(arg.name ...)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
                       "duplicate argument name"
           #:fail-when (check-duplicates (attribute arg.kw)
                                         (lambda (x y)
                                           (and x y (equal? (syntax-e x) (syntax-e y)))))
                       "duplicate keyword for argument"
           #:fail-when (invalid-option-placement
                        (attribute arg.kw) (attribute arg.name) (attribute arg.default))
                       "default-value expression missing"))

(define-syntax-class formals
  #:attributes (params)
  (pattern (~or* (args:formals-no-rest)
                 (args:formals-no-rest . rest-id:id))
           #:attr params #'((~@ . args.params) (~? rest-id))
           #:fail-when (and (attribute rest-id)
                            (member #'rest-id (syntax->list #'args.params) bound-identifier=?)
                            #'rest-id)
                       "duplicate argument identifier"))

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

;; invalid-option-placement : (Listof Keyword) (Listof Id) (Listof Syntax/#f) -> Id/#f
;; Checks for mandatory argument after optional argument; if found, returns
;; identifier of mandatory argument.
(define (invalid-option-placement kws names defaults)
  ;; find-mandatory : (Listof Keyword) (Listof Id) (Listof Syntax/#f) -> Id/#f
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
          [(or (car kws) ;; keyword
               (eq? (car defaults) #f)) ;; mandatory arg
           (loop (cdr kws) (cdr names) (cdr defaults))]
          [else ;; found optional
           (find-mandatory (cdr kws) (cdr names) (cdr defaults))])))
