#lang racket/base
(require racket/format
         net/head
         net/smtp
         net/sendmail
         openssl
         racket/tcp)

(provide send-email)

(define (send-email to-email get-opt
                    stamp
                    start-seconds end-seconds
                    failures)
  (let ([server (get-opt '#:smtp-server #f)]
        [from-email (or (get-opt '#:email-from #f)
                        (car to-email))]
        [subject (~a "[build] "
                     (if (null? failures)
                         "success"
                         "FAILURE")
                     " " stamp)]
        [message (append
                  (if (null? failures)
                      '("All builds succeeded.")
                      (cons
                       "The following builds failed:"
                       (for/list ([i (in-list failures)])
                         (~a " " i))))
                  (list
                   ""
                   (let ([e (- end-seconds start-seconds)]
                         [~d (lambda (n)
                               (~a n #:width 2 #:pad-string "0" #:align 'right))])
                     (~a "Elapsed time: "
                         (~d (quotient e (* 60 60)))
                         ":"
                         (~d (modulo (quotient e (* 60)) 60))
                         ":"
                         (~d (modulo e (* 60 60)))))
                   ""
                   (~a "Stamp: " stamp)))])
    (cond
     [server
      (let* ([smtp-connect (get-opt '#:smtp-connect 'plain)]
             [port-no (get-opt '#:smtp-port 
                               (case smtp-connect
                                 [(plain) 25]
                                 [(ssl) 465]
                                 [(tls) 587]))])
        (smtp-send-message server
                           #:port-no port-no
                           #:tcp-connect (if (eq? 'ssl smtp-connect)
                                             ssl-connect
                                             tcp-connect)
                           #:tls-encode (and (eq? 'tls smtp-connect)
                                             ports->ssl-ports)
                           #:auth-user (get-opt '#:smtp-user #f)
                           #:auth-passwd (get-opt '#:smtp-password #f)
                           from-email
                           to-email
                           (standard-message-header from-email
                                                    to-email
                                                    null
                                                    null
                                                    subject)
                           message))]
     [else
      (send-mail-message from-email
                         subject	 	 	 	 
                         to-email
                         null
                         null
                         message)])))
