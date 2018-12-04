#lang racket/base

;; Check that the contract error messages for the `c*r` accessors
;;  describe a value that the accessor accepts
;;
;; e.g. `cadr` accepts a `(cons/c any/c pair?)`, so the error message should
;;  not ask for a `(cons/c pair? any/c)`

(require racket/contract racket/port racket/match)

(module+ test
  (require rackunit)

  (for* ((num-letters (in-range 1 5))
         (accessor-num (in-range 0 (expt 2 num-letters))))
    (define ad* (fixnum->accessor-char* accessor-num num-letters))
    (define accessor (make-c*r ad*))
    (define sexp (accessor->expected-sexp accessor))
    (define val (sexp->value sexp))
    (define ctc (sexp->contract sexp))
    (if (contract-first-order-passes? ctc val)
      (check-not-exn (lambda () (accessor val))
                     (format "~a claims it expects a '~a but fails on ~a"
                             accessor sexp val))
      (error 'bad-value "(~a ~a) = #false" ctc val))))

;; -----------------------------------------------------------------------------
;; --- helper functions

(define (string->value str)
  (with-input-from-string str read))

(define (accessor->expected-sexp f)
  (define evil-value 0)
  (define err-str
    (with-handlers ((exn:fail:contract? exn-message))
      (f evil-value)
      (raise-user-error 'accessor->expected-sexp "application failed to raise contract error (~a ~a)" f evil-value)))
  (string->value (cadr (regexp-match "expected: (.*)$" err-str))))

(module+ test
  (check-equal? (accessor->expected-sexp car) 'pair?)
  (check-equal? (accessor->expected-sexp caddr) '(cons/c any/c (cons/c any/c pair?))))

(define (sexp->value sexp)
  (match sexp
   ['any/c 'any/c]
   ['pair? (cons 'any/c 'any/c)]
   [(list 'cons/c a b) (cons (sexp->value a) (sexp->value b))]))

(module+ test
  (check-equal? (sexp->value 'any/c) 'any/c)
  (check-equal? (sexp->value 'pair?) (cons 'any/c 'any/c))
  (check-equal? (sexp->value '(cons/c any/c pair?)) (cons 'any/c (cons 'any/c 'any/c))))

(define (sexp->contract sexp)
  (match sexp
   ['any/c any/c]
   ['pair? pair?]
   [(list 'cons/c a b) (cons/c (sexp->contract a) (sexp->contract b))]))

(module+ test
  (check-eq? (sexp->contract 'any/c) any/c)
  (check-eq? (sexp->contract 'pair?) pair?)
  (check-pred contract? (sexp->contract '(cons/c any/c pair?))))

(define (fixnum->accessor-char* n k)
  (unless (fixnum? n)
    (raise-argument-error 'fixnum->accessor-char* "fixnum?" n))
  (reverse
    (for/list ((i (in-range k)))
      (if (bitwise-bit-set? n i) #\a #\d))))

(module+ test
  (check-equal? (fixnum->accessor-char* 4 3) '(#\a #\d #\d))
  (check-equal? (fixnum->accessor-char* 1 1) '(#\a))
  (check-equal? (fixnum->accessor-char* 10 3) '(#\d #\a #\d)))

(define make-c*r
  (let ((ns (make-base-namespace)))
    (lambda (cr*)
      (unless (< 0 (length cr*) 5)
        (raise-argument-error 'make-c*r "1 to 4 characters" cr*))
      (eval (string->symbol (format "c~ar" (list->string cr*))) ns))))

(module+ test
  (check-eq? (make-c*r '(#\a #\a #\a)) caaar))

