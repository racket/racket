#lang racket/base

(require "../../parse.rkt"
         "../experimental/template.rkt"
         racket/dict)

(provide function-header arg args)

(define-syntax-class function-header
  (pattern ((~or header:function-header name:id) . args:args)
           #:attr params
           (template ((?@ . (?? header.params ()))
                      . args.params))))

(define-syntax-class args
  #:attributes (params)
  (pattern (arg:arg ...)
           #:attr params #'(arg.name ...)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
           "duplicate argument name"
           #:fail-when (check-duplicate (syntax->list #'(arg.kw ...))
                                        #:key (λ (x)
                                                (syntax->datum x))
                                        #:same? (λ (x y)
                                                  (and x y (equal? x y))))
           "duplicate keyword for argument"
           #:fail-when (invalid-option-placement
                        (syntax->list #'((arg.name arg.default) ...)))
           "default-value expression missing")
  (pattern (arg:arg ... . rest:id)
           #:attr params #'(arg.name ... rest)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
           "duplicate argument name"
           #:fail-when (check-duplicate (syntax->list #'(arg.kw ...))
                                        #:key (λ (x)
                                                (syntax->datum x))
                                        #:same? (λ (x y)
                                                  (and x y (equal? x y))))
           "duplicate keyword for argument"
           #:fail-when (invalid-option-placement
                        (syntax->list #'((arg.name arg.default) ...)))
           "default-value expression missing"))

(define-splicing-syntax-class arg
  #:attributes (name kw default)
  (pattern name:id
           #:attr kw #'#f
           #:attr default #'#f)
  (pattern [name:id default]
           #:attr kw #'#f)
  (pattern (~seq kw:keyword name:id)
           #:attr default #'#f)
  (pattern (~seq kw:keyword [name:id default])))

(define (invalid-option-placement optional-list)
  (define iop
    (for/fold ([status      'required])
              ([i optional-list]
               #:break (syntax? status))
      (define i* (syntax->list i))
      ;(match* (status (syntax->datum (cadr i*)))
      (cond [(eq? status 'required)
             (cond [(syntax->datum (cadr i*)) 'optional]
                   [else                      'required])]
            [else
             (cond [(syntax->datum (cadr i*)) 'optional]
                   [else                      (car i*)])])))
  (if (syntax? iop) iop #f))

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
