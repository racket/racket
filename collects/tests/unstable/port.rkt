#lang racket

(require unstable/port
         test-engine/scheme-tests)

(check-expect (port? null-output-port)
              #t)

(check-expect (with-output-to-string
                (lambda ()
                  (parameterize ([current-output-port null-output-port])
                    (display "can't see me"))))
              "")

(check-expect (with-output-to-string
                (lambda ()
                  (display "now you see me")
                  (parameterize ([current-output-port null-output-port])
                    (display "now you don't"))
                  (display "you can see me again")))
              "now you see meyou can see me again")

(test)
