#lang racket/base
(require net/smtp tests/eli-tester)

(provide tests)

(define SERVER-REPLIES
  (string-append "220 ready\r\n"
                 "250 ok\r\n"   ; EHLO
                 "250 ok\r\n"   ; MAIL FROM
                 "250 ok\r\n"   ; RCPT TO
                 "354 go\r\n"   ; DATA
                 "250 ok\r\n"   ; .
                 "221 bye\r\n"))

;; returns everything the client sent, or the error message
(define (send sender recipients)
  (define out (open-output-bytes))
  (with-handlers ([exn:fail? exn-message])
    (smtp-send-message* (open-input-string SERVER-REPLIES) out
                        sender recipients
                        "Subject: hi\r\n\r\n" '("hello")
                        #f #f #f)
    (get-output-bytes out)))

(define (tests)
  (test
   (send "a@example.com" '("b@example.com"))
   => (bytes-append #"EHLO localhost\r\n"
                    #"MAIL FROM:<a@example.com>\r\n"
                    #"RCPT TO:<b@example.com>\r\n"
                    #"DATA\r\n"
                    #"Subject: hi\r\n\r\n"
                    #"hello\r\n"
                    #".\r\n"
                    #"QUIT\r\n")
   ;; a CR or LF in an address must not be able to inject commands
   (regexp-match? #rx"carriage return or linefeed"
                  (send "a@example.com" '("b@example.com>\r\nRCPT TO:<c@example.com")))
   (regexp-match? #rx"carriage return or linefeed"
                  (send "a@example.com>\nRCPT TO:<c@example.com" '("b@example.com")))))

(module+ main (tests))
(module+ test (require (submod ".." main))) ; for raco test & drdr
