#lang racket/base
(require racket/format
         racket/string
         setup/getinfo
         "get-info.rkt"
         "print.rkt")

(provide pkg-doctor)

;; The checks mirror the metadata that the package catalog surfaces on
;; a package's page: a missing or empty entry there shows up as a
;; visible "todo" for the package.

(define (check-present get key)
  (define v (get key (λ () #f)))
  (cond
    [(not v) 'missing]
    [(and (string? v) (string=? "" (string-trim v))) 'missing]
    [(and (list? v) (null? v)) 'missing]
    [else 'ok]))

(define (pkg-doctor dir)
  (printf "Checking package in ~a\n" dir)
  (define ns (make-metadata-namespace))
  (define get (get-pkg-info dir ns))
  (unless get
    (pkg-error "no usable info.rkt found"))
  (define results
    (for/list ([key (in-list '(description author license scribblings))])
      (cons key (check-present get key))))
  (define problems
    (for/list ([key+status (in-list results)]
               #:when (eq? (cdr key+status) 'missing))
      (car key+status)))
  (for ([key+status (in-list results)])
    (printf "  ~a  ~a\n"
            (if (eq? (cdr key+status) 'ok)
                "ok     "
                "missing")
            (car key+status)))
  (cond
    [(null? problems)
     (printf "No problems found.\n")]
    [else
     (printf "~a missing metadata field(s) found.\n" (length problems))
     (pkg-error
      (~a "add the missing field(s) to the package's info.rkt: "
          (string-join (map symbol->string problems) ", ")))]))
