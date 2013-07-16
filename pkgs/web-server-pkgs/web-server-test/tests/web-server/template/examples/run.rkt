#lang racket
(require web-server/templates
         rackunit)

(include-template "static.html")

(define (basic-template title clients client-surname client-firstname client-email)
  (include-template "basic.html"))

(basic-template "Title"
                (list (list "First 1" "Second 1" "Third 1")
                      (list "First 2" "Second 2" "Third 2")
                      (list "First 3" "Second 3" "Third 3")
                      (list "First 4" "Second 4" "Third 4"))
                first second third)

(local ()
  (define-struct client (surname firstname email))
  (basic-template "Title"
                  (list (make-client "First 1" "Second 1" "Third 1")
                        (make-client "First 2" "Second 2" "Third 2")
                        (make-client "First 3" "Second 3" "Third 3")
                        (make-client "First 4" "Second 4" "Third 4"))
                  client-surname client-firstname client-email))

(define (if-template #:monkeys monkeys
                     #:monkey-limit monkey-limit
                     #:monkey-minimum monkey-minimum)
  (include-template "if.html"))

(if-template #:monkeys 5
             #:monkey-limit 10
             #:monkey-minimum 2)
(if-template #:monkeys 11
             #:monkey-limit 10
             #:monkey-minimum 2)
(if-template #:monkeys 1
             #:monkey-limit 10
             #:monkey-minimum 2)

(check-equal? (include-template #:command-char #\$ "diff.html")
              "This is the number: 42\nThis is not the number: @(+ 2 40)")
