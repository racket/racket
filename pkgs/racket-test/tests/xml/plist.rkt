#lang racket/base

(module+ test
  (require
   (only-in rackunit
            test-case
            check-pred
            check-false
            check-equal?)
   (only-in racket/port
            with-output-to-string
            call-with-input-string)
   (only-in xml/plist
            plist-value?
            write-plist
            read-plist))

  (define (plist->string pl)
    (with-output-to-string
      (lambda ()
        (write-plist pl (current-output-port)))))

  (define (string->plist s)
    (call-with-input-string
     s
     (lambda (port)
       (read-plist port))))

  (define (check-inverse v0)
    (check-pred plist-value? v0)
    (define s
      (plist->string v0))
    (define v1
      (string->plist s))
    (check-equal? v1 v0))

  (check-false (plist-value? '(dict (assoc-pair "" (array)))))

  (check-inverse "blub")
  (check-inverse '(true))
  (check-inverse '(false))
  (check-inverse '(dict))
  (check-inverse '(dict (assoc-pair "a" (array))))
  (check-inverse '(dict (assoc-pair "a" "bla") (assoc-pair "b" "blub"))))

