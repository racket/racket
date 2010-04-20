#lang racket/base

(require (for-syntax racket/base
                     racket/stxparam))
(require racket/stxparam)

(provide current-class-event-handler
         define-traced
         trace-begin
         trace
         initialize-call-event
         finalize-call-event
         new-event
         inspect-event
         set-event
         get-event
         )

(define current-class-event-handler
  (make-parameter void))

;; ----------------------------------------------------------------------
;; Definitions for traced vs untraced functions
;; ----------------------------------------------------------------------

(define-syntax-parameter trace? #f)

(define-syntax trace
  (make-set!-transformer
   (lambda (stx)
     (raise-syntax-error
      #f
      "trace used outside a trace-begin"
      stx))))

(define-syntax (trace-begin stx)
  (syntax-case stx (trace)
    [(form (trace expr) ...)
     (raise-syntax-error
      #f
      "traced block has no non-trace code"
      stx)]
    [(form expr0 (trace expr) ...)
     (if (syntax-parameter-value (syntax trace?))
         (syntax/loc stx (begin0 expr0 expr ...))
         (syntax expr0))]
    [(form (trace expr) rest ...)
     (if (syntax-parameter-value (syntax trace?))
         (syntax/loc stx (begin expr (form rest ...)))
         (syntax/loc stx (form rest ...)))]
    [(form expr rest ...)
     (syntax/loc stx (begin expr (form rest ...)))]))

(define-syntax (define-traced stx)
  (syntax-case stx ()
    [(form (name . args) . body)
     (syntax/loc stx (form name (lambda args . body)))]
    [(form name body ...)
     (with-syntax ([name-traced
                    (datum->syntax
                     (syntax name)
                     (string->symbol
                      (string-append
                       (symbol->string (syntax-e (syntax name)))
                       "-traced"))
                     (syntax name))])
       (syntax/loc stx
         (begin
           (define name
             (syntax-parameterize ([trace? #f])
                                  body ...))
           (define name-traced
             (syntax-parameterize ([trace? #t])
                                  body ...)))))]))

(define current-class-event-stack
  (make-parameter null))

(define (initialize-event event . args)
  (current-class-event-stack
   (cons
    (apply (current-class-event-handler) event args)
    (current-class-event-stack))))

(define (finalize-event event . args)
  (let* ([stack (current-class-event-stack)]
         [head (car stack)]
         [tail (cdr stack)])
    (when (procedure? head) (apply head args))
    (current-class-event-stack tail)))

(define (new-event class obj fields)
  ((current-class-event-handler) 'new class obj fields))

(define (initialize-call-event obj method args)
  (initialize-event 'call obj method args))

(define (finalize-call-event . returned)
  (apply finalize-event 'call returned)
  (apply values returned))

(define (inspect-event obj)
  ((current-class-event-handler) 'inspect obj))

(define (set-event obj field value)
  ((current-class-event-handler) 'set obj field value))

(define (get-event obj field)
  ((current-class-event-handler) 'get obj field))

