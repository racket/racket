#lang racket/base

(require "../../parse.rkt"
         "../experimental/template.rkt"
         racket/dict)

(provide function-header formal formals)

(define-syntax-class function-header
  (pattern ((~or header:function-header name:id) . args:formals)
           #:attr params
           (template ((?@ . (?? header.params ()))
                      . args.params))))

(define-syntax-class formals
  #:attributes (params)
  (pattern (arg:formal ...)
           #:attr params #'(arg.name ...)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
                       "duplicate argument name"
           #:fail-when (check-duplicate (attribute arg.kw)
                                        #:same? (λ (x y)
                                                  (and x y (equal? (syntax-e x)
                                                                   (syntax-e y)))))
                       "duplicate keyword for argument"
           #:fail-when (invalid-option-placement
                        (attribute arg.name) (attribute arg.default))
                       "default-value expression missing")
  (pattern (arg:formal ... . rest:id)
           #:attr params #'(arg.name ... rest)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
                       "duplicate argument name"
           #:fail-when (check-duplicate (attribute arg.kw)
                                        #:same? (λ (x y)
                                                  (and x y (equal? (syntax-e x)
                                                                   (syntax-e y)))))
                       "duplicate keyword for argument"
           #:fail-when (invalid-option-placement
                        (attribute arg.name) (attribute arg.default))
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
(define (invalid-option-placement names defaults)
  ;; find-mandatory : (Listof Id) (Listof Syntax/#f) -> Id/#f
  ;; Finds first name w/o corresponding default.
  (define (find-mandatory names defaults)
    (for/first ([name (in-list names)]
                [default (in-list defaults)]
                #:when (not default))
      name))
  ;; Skip through mandatory args until first optional found, then search
  ;; for another mandatory.
  (let loop ([names names] [defaults defaults])
    (cond [(or (null? names) (null? defaults))
           #f]
          [(eq? (car defaults) #f) ;; mandatory
           (loop (cdr names) (cdr defaults))]
          [else ;; found optional
           (find-mandatory (cdr names) (cdr defaults))])))

;; Copied from unstable/list
;; check-duplicate : (listof X)
;;                   #:key (X -> K)
;;                   #:same? (or/c (K K -> bool) dict?)
;;                -> X or #f
(define (check-duplicate items
                        #:key [key values]
                        #:same? [same? equal?])
  (cond [(procedure? same?)
         (cond [(eq? same? equal?)
                (check-duplicate/t items key (make-hash) #t)]
               [(eq? same? eq?)
                (check-duplicate/t items key (make-hasheq) #t)]
               [(eq? same? eqv?)
                (check-duplicate/t items key (make-hasheqv) #t)]
               [else
                (check-duplicate/list items key same?)])]
        [(dict? same?)
         (let ([dict same?])
           (if (dict-mutable? dict)
               (check-duplicate/t items key dict #t)
               (check-duplicate/t items key dict #f)))]))
(define (check-duplicate/t items key table mutating?)
  (let loop ([items items] [table table])
    (and (pair? items)
         (let ([key-item (key (car items))])
           (if (dict-ref table key-item #f)
               (car items)
               (loop (cdr items) (if mutating?
                                     (begin (dict-set! table key-item #t) table)
                                     (dict-set table key-item #t))))))))
(define (check-duplicate/list items key same?)
  (let loop ([items items] [sofar null])
    (and (pair? items)
         (let ([key-item (key (car items))])
           (if (for/or ([prev (in-list sofar)])
                 (same? key-item prev))
               (car items)
               (loop (cdr items) (cons key-item sofar)))))))
