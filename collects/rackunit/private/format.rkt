#lang racket/base
(require racket/match
         "base.rkt"
         "check-info.rkt")

(provide display-check-info-name-value
         display-check-info
         display-check-info-stack
         display-test-name
         display-exn

         display-delimiter
         display-failure
         display-error

         display-test-failure/error
         strip-redundant-params)

;; name-width : integer
;;
;; Number of characters we reserve for the check-info name column
(define name-width 12)

(define (display-delimiter)
  (display "--------------------") (newline))

(define (display-failure)
  (display "FAILURE"))

(define (display-error)
  (display "ERROR"))

(define (string-pad-right s n)
  (define m (string-length s))
  (cond
   [(= m n) s]
   [(m . < . n)
    (string-append s (make-string (- n m) #\space))]
   [else
    (substring s n)]))

(define (display-check-info-name-value name value [value-printer write])
  (display (string-pad-right
            (string-append (symbol->string name) ": ")
            name-width))
  (value-printer value)
  (newline))

(define display-check-info
  (match-lambda [(struct check-info (name value))
                 (display-check-info-name-value name value)]))

;; display-check-info-stack : (listof check-info) -> void
(define (display-check-info-stack check-info-stack)
  (for-each
   display-check-info
   (strip-redundant-params check-info-stack))
  (newline))

;; display-test-name : (U string #f) -> void
(define (display-test-name name)
  (if name
      (begin
        (display name) (newline))
      (begin
        (display "Unnamed test ")(newline))))

;; display-exn : any -> void
;;
;; Outputs a printed representation of the exception to
;; the current-output-port
;; If given non-exn value, says so.
(define (display-exn v)
  (parameterize ((current-error-port (current-output-port)))
    (if (exn? v)
        ((error-display-handler) (exn-message v) v)
        (printf "A value other than an exception was raised: ~e\n" v))
    (newline)))

;; ----

;; strip-redundant-parms : (list-of check-info) -> (list-of check-info)
;;
;; Strip any check-params? is there is an
;; actual/expected check-info in the same stack frame.  A
;; stack frame is delimited by occurrence of a check-name?
(define (strip-redundant-params stack)
  (define (binary-check-this-frame? stack)
    (let loop ([stack stack])
      (cond
        [(null? stack) #f]
        [(check-name? (car stack)) #f]
        [(check-actual? (car stack)) #t]
        [else (loop (cdr stack))])))
  (let loop ([stack stack])
    (cond
      [(null? stack) null]
      [(check-params? (car stack))
       (if (binary-check-this-frame? stack)
           (loop (cdr stack))
           (cons (car stack) (loop (cdr stack))))]
      [else (cons (car stack) (loop (cdr stack)))])))

;; ----

;; display-test-failure/error : any string/#f -> void
(define (display-test-failure/error e [name #f])
  (parameterize ((current-output-port (current-error-port)))
    (display-delimiter)
    (when name (display-test-name name))
    (cond [(exn:test:check? e)
           (display-failure) (newline)
           (display-check-info-stack (exn:test:check-stack e))
           (when #t
             (parameterize ((error-print-context-length 0))
               ((error-display-handler) (exn-message e) e)))]
          [else
           (display-error) (newline)
           (display-exn e)])
    (display-delimiter)))
