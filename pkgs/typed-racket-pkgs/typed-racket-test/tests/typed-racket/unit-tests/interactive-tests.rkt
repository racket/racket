#lang racket/base

(require
  racket/base
  racket/port
  racket/promise
  rackunit
  (for-syntax
    racket/base
    racket/format
    syntax/parse))

(provide interactive-tests)

(define promised-ns
  (delay
    (define ns (make-base-namespace))
    (eval '(require typed/racket) ns)
    ns))

(define-syntax (test-form-exn stx)
  (syntax-parse stx
    [(_ regexp:expr form:expr)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'form))
         (check-exn
           regexp
           (lambda ()
             (eval `(#%top-interaction .
                     ,(syntax->datum #'form)) (force promised-ns))))))]))

(define-syntax (test-form stx)
  (syntax-parse stx
    [(_ regexp:expr form:expr)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'form))
         (check-regexp-match
           regexp
           (with-output-to-string
             (lambda ()
               (eval `(#%top-interaction .
                       ,(syntax->datum #'form)) (force promised-ns)))))))]))

;; Add 'only at the toplevel tests'
(define (interactive-tests)
  (test-suite "Interactive tests"

    (test-form #rx""
      (module test racket))
    (test-form #rx""
      (define module displayln))
    (test-form #rx"racket"
      (module 'racket))

    (test-form #rx"1"
      (:type 1))
    (test-form (regexp-quote "(U Positive-Byte Zero)")
      (:type Byte))
    (test-form (regexp-quote "(U 0 1 Byte-Larger-Than-One")
      (:type #:verbose Byte))
    (test-form-exn #rx":type.*applied to arguments"
      :type)
    (test-form-exn #rx":type.*only valid at the top-level"
      (list (:type)))
    (test-form-exn #rx"exactly one argument"
      (:type))
    (test-form-exn #rx"exactly one argument"
      (:type 1 2))
    (test-form-exn #rx"exactly one argument"
      (:type #:verbose))

    (test-form #rx"Positive-Index"
      (:print-type (+ 1 1)))
    (test-form (regexp-quote "(values One One)")
      (:print-type (values 1 1)))
    (test-form-exn #rx":print-type.*applied to arguments"
      :print-type)
    (test-form-exn #rx":print-type.*only valid at the top-level"
      (list (:print-type)))
    (test-form-exn #rx"exactly one argument"
      (:print-type))
    (test-form-exn #rx"exactly one argument"
      (:print-type 1 2))

    (test-form (regexp-quote "(4 Zero -> Zero)")
      (:query-type/args * 4 0))
    (test-form-exn #rx":query-type/args.*applied to arguments"
      :query-type/args)
    (test-form-exn #rx":query-type/args.*only valid at the top-level"
      (list (:query-type/args)))
    (test-form-exn #rx"at least one argument"
      (:query-type/args))

    (test-form (regexp-quote "(case-> (One -> One) (-> One))")
      (:query-type/result * 1))
    (test-form #rx"not in the given function's range.\n"
      (:query-type/result + String))
    (test-form-exn #rx":query-type/result.*applied to arguments"
      :query-type/result)
    (test-form-exn #rx":query-type/result.*only valid at the top-level"
      (list (:query-type/result)))
    (test-form-exn #rx"exactly two arguments"
      (:query-type/result))
    (test-form-exn #rx"exactly two arguments"
      (:query-type/result 1 2 3))))
