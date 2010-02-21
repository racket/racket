#lang scheme/base

(require scheme/match
         srfi/13
         "check-info.ss")

(provide display-check-info-name-value
         display-check-info
         display-check-info-stack
         display-test-name
         display-exn

         display-delimiter
         display-failure
         display-error)

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
   check-info-stack)
  (newline))

;; display-test-name : (U string #f) -> void
(define (display-test-name name)
  (if name
      (begin
        (display name) (newline))
      (begin
        (display "Unnamed test ")(newline))))

;; display-exn : exn -> void
;;
;; Outputs a printed representation of the exception to
;; the current-output-port
(define (display-exn exn)
  (let ([op (open-output-string)])
    (parameterize ([current-error-port op])
      ((error-display-handler)
       (exn-message exn)
       exn))
    (display (get-output-string op))
    (newline)))

