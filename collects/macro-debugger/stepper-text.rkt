#lang racket/base
(require racket/pretty
         racket/promise
         "model/trace.rkt"
         "model/reductions.rkt"
         "model/reductions-config.rkt"
         "model/steps.rkt"
         "syntax-browser/partition.rkt"
         "syntax-browser/pretty-helper.rkt"
         "view/debug-format.rkt")
(provide expand/step-text
         stepper-text)

(define (expand/step-text stx [show #f]
                          #:internal-error-file [error-file #f])
  (let ([s (stepper-text stx (->show-function show) #:internal-error-file error-file)])
    (s 'all)))

(define (stepper-text stx [show #f]
                      #:internal-error-file [error-file #f])
  (internal-stepper stx (->show-function show) error-file))

;; internal procedures

(define (internal-stepper stx show? error-file)
  (define steps (get-steps stx show? error-file))
  (define used-steps null)
  (define partition (new-bound-partition))
  (define dispatch
    (case-lambda
     [() (dispatch 'next)]
     [(sym)
      (case sym
        ((next)
         (if (pair? steps)
             (begin (show-step (car steps) partition)
                    (set! used-steps (cons (car steps) used-steps))
                    (set! steps (cdr steps)))
             #f))
        ((prev)
         (if (pair? used-steps)
             (begin (show-step (car used-steps) partition)
                    (set! steps (cons (car used-steps) steps))
                    (set! used-steps (cdr used-steps)))
             #f))
        ((all)
         (when (pair? steps)
           (dispatch 'next)
           (dispatch 'all))))]))
  dispatch)

(define (get-steps stx show? error-file)
  (let-values ([(_result events derivp) (trace* stx)])
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (when error-file
                         (write-debug-file error-file exn events))
                       (raise exn))])
      (define deriv (force derivp))
      (define steps
        (parameterize ((macro-policy show?))
          (reductions deriv)))
      (define (ok? x)
        (or (rewrite-step? x) (misstep? x)))
      (filter ok? steps))))

(define (show-step step partition)
  (cond [(step? step)
         (display (step-type->string (protostep-type step)))
         (newline)
         (show-term (step-term1 step) partition)
         (display "  ==>")
         (newline)
         (show-term (step-term2 step) partition)
         (newline)]
        [(misstep? step)
         (display (exn-message (misstep-exn step)))
         (newline)
         (show-term (misstep-term1 step) partition)]))

(define (show-term stx partition)
  (define-values (datum flat=>stx stx=>flat)
    (table stx partition 0 'always #t))
  (define identifier-list 
    (filter identifier? (hash-map stx=>flat (lambda (k v) k))))
  (define (pp-size-hook obj display-like? port)
    (cond [(syntax-dummy? obj)
           (let ((ostring (open-output-string)))
             ((if display-like? display write)
              (syntax-dummy-val obj)
              ostring)
             (string-length (get-output-string ostring)))]
          [else #f]))
  (define (pp-print-hook obj display-like? port)
    (cond [(syntax-dummy? obj)
           ((if display-like? display write) (syntax-dummy-val obj) port)]
          [else 
           (error 'pretty-print-hook "unexpected special value: ~e" obj)]))
  (define (pp-better-style-table)
    (pretty-print-extend-style-table (pretty-print-current-style-table)
                                     (map car extended-style-list)
                                     (map cdr extended-style-list)))
  (parameterize 
   ([pretty-print-size-hook pp-size-hook]
    [pretty-print-print-hook pp-print-hook]
    [pretty-print-current-style-table (pp-better-style-table)])
   (pretty-print/defaults datum)))

(define (->show-function show)
  (cond [(procedure? show)
         show]
        [(list? show)
         (lambda (id)
           (ormap (lambda (x) (free-identifier=? x id))
                  show))]
        [(eq? show #f)
         (lambda (id) #t)]
        [else
         (error 'expand/trace-text
                "expected procedure or list of identifiers for macros to show; got: ~e"
                show)]))

(define extended-style-list
  '((define-values          . define)
    (define-syntaxes        . define-syntax)))
