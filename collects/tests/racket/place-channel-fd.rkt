#lang racket/base

(require racket/place
         racket/port
         racket/runtime-path
         racket/list
         racket/tcp
         racket/match
         rackunit
         (for-syntax racket/base))

(module+ test
  (main))

(define (main)
  (with-output-to-file "test2" #:exists 'replace (lambda () (write "Get it?\n")))

  (define p 
    (place ch
           (define b (place-channel-get ch))
           (fprintf b "Got it\n")
           (check-equal? #t #t "standard output")

           (define b2 (place-channel-get ch))
           (fprintf b2 "Bye\n")
           (flush-output b2)
           (close-output-port b2)
           (check-equal? #t #t "output-file")
           (place-channel-put ch 1)

           (define b3 (place-channel-get ch))
           (define msg (read b3))
           (check-equal? msg "Get it?\n" "input-file")
           (fprintf (current-output-port) msg)
           (close-input-port b3)

           (define b4 (car (place-channel-get ch)))
           (fprintf b4 "BYE\n")
           (flush-output b4)
           (close-output-port b4)
           (check-equal? #t #t "output-file from large message")
           (place-channel-put ch 1)

           (define b5 (car (place-channel-get ch)))
           (define msg2 (read b5))
           (check-equal? msg2 "Get it?\n" "input-file from large message")
           (fprintf (current-output-port) msg2)
           (close-input-port b5)

           ))
  (place-channel-put p (current-output-port))

  (define o (open-output-file "test1" #:exists 'replace))
  (for ([n (in-range 10000)]) (place-message-allowed? o)) ; make sure checking doesn't dup
  (write-string "Hello\n" o)
  (flush-output o)
  (place-channel-put p o)
  (place-channel-get p)

  (define i (open-input-file "test2"))
  (for ([n (in-range 10000)]) (place-message-allowed? i)) ; make sure checking doesn't dup
  (place-channel-put p i)
  (close-input-port i)

  (write-string "Hello\n" o)
  (close-output-port o)
  (with-input-from-file "test1"
    (lambda ()
      (check-equal? (port->string) "Hello\nBye\nHello\n" "output file contents match")))

  (define o2 (open-output-file "test1" #:exists 'replace))
  (define l (make-list 1024 1))
  (write-string "HELLO\n" o2)
  (flush-output o2)
  (place-channel-put p (cons o2 l))
  (place-channel-get p)

  (write-string "HELLO\n" o2)
  (close-output-port o2)
  (with-input-from-file "test1"
    (lambda ()
      (check-equal? (port->string) "HELLO\nBYE\nHELLO\n" "output file contents match")))

  (define i2 (open-input-file "test2"))
  (place-channel-put p (cons i2 l))
  (close-input-port i2)

  (place-wait p)

  (define i3 (open-input-file "test2"))
  (check-equal? #t #t "cleanup of unreceived port message")
  (place-channel-put p i3)


  (define port-ch (make-channel))
  
  (thread 
    (lambda () 
      (define p (place ch
                            (match (place-channel-get ch)
                              [(list in out)
                                (define x (read in))
                                (printf "IN PLACE ~a\n" x)
                                (write (string-append "From Place " x) out)
                                (flush-output out)])))
      (define s (tcp-listen 0))
      (define-values (h1 p1 h2 p2) (tcp-addresses s #t))
      (printf "~a ~a ~a ~a\n" h1 p1 h2 p2)
      (channel-put port-ch p1)
      (define-values (in out) (tcp-accept s))
      (place-channel-put p (list in out))
      (place-wait p)))

  (define-values (in out) (tcp-connect "localhost" (channel-get port-ch)))
  (write "Hello There" out)
  (flush-output out)
  (displayln (read in))
  )
