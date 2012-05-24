#lang racket/base

(require racket/generic racket/port)

(define-generics printable
  (gen-print printable [port])
  (gen-port-print port printable)
  (gen-print* printable [port] #:width width #:height [height]))

(define-struct num (v)
  #:methods gen:printable
  [(define/generic super-print gen-print)
   (define (gen-print n [port (current-output-port)])
     (fprintf port "Num: ~a" (num-v n)))
   (define (gen-port-print port n)
     (super-print n port))
   (define (gen-print* n [port (current-output-port)]
                       #:width w #:height [h 0])
     (fprintf port "Num (~ax~a): ~a" w h (num-v n)))])

(define-struct bool (v)
  #:methods gen:printable
  [(define/generic super-print gen-print)
   (define (gen-print b [port (current-output-port)])
     (fprintf port "Bool: ~a"
              (if (bool-v b) "Yes" "No")))
   (define (gen-port-print port b)
     (super-print b port))
   (define (gen-print* b [port (current-output-port)]
                       #:width w #:height [h 0])
     (fprintf port "Bool (~ax~a): ~a" w h
              (if (bool-v b) "Yes" "No")))])

(module+ test
  (require rackunit)

  (define x (make-num 10))
  (check-equal? (with-output-to-string (lambda () (gen-print x)))
                "Num: 10")
  (check-equal? (with-output-to-string
                  (lambda () (gen-port-print (current-output-port)
                                             x)))
                "Num: 10")
  (check-equal? (with-output-to-string
                  (lambda () (gen-print* x #:width 100 #:height 90)))
                "Num (100x90): 10")

  (define y (make-bool #t))
  (check-equal? (with-output-to-string (lambda () (gen-print y)))
                "Bool: Yes")
  (check-equal? (with-output-to-string
                  (lambda () (gen-port-print (current-output-port)
                                             y)))
                "Bool: Yes")
  (check-equal? (with-output-to-string
                  (lambda () (gen-print* y #:width 100 #:height 90)))
                "Bool (100x90): Yes"))
