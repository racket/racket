#lang racket/base
(require racket/promise)

(provide send-mail-message/port send-mail-message)

(require net/unihead)

(define sendmail-search-path
  '("/usr/sbin" "/sbin" "/usr/local/sbin" "/usr/lib"))

(define sendmail-program-file
  (delay
    (let ([exe (case (system-type)
                 [(windows) "sendmail.exe"]
                 [else "sendmail"])])
      (or (for/or ([path (in-list sendmail-search-path)])
            (define p (build-path path exe))
            (and (file-exists? p)
                 (memq 'execute (file-or-directory-permissions p))
                 p))
          (raise (make-exn:fail:unsupported
                  (format "unable to find a sendmail executable in ~s"
                          sendmail-search-path)
                  (current-continuation-marks)))))))

;; Main implementation, returns a port
(define (send-mail-core who sender subject TOs CCs BCCs headers)
  (define qTOs  (map encode-for-header TOs))
  (define qCCs  (map encode-for-header CCs))
  (define qBCCs (map encode-for-header BCCs))
  (define all-recipients (append qTOs qCCs qBCCs))
  (when (null? all-recipients)
    (error who "no mail recipients were specified"))
  (define-values [p pout pin perr]
    ;; use -i, so "." lines are not a problem
    (apply subprocess #f #f #f (force sendmail-program-file) "-i" all-recipients))
  (close-input-port pout)
  (close-input-port perr)
  (port-count-lines! pin)
  (fprintf pin "X-Mailer: Racket (racket-lang.org)\n")
  (when sender (fprintf pin "From: ~a\n" (encode-for-header sender)))
  (for ([header (in-list '("To" "CC"))]
        [recipients (in-list (list qTOs qCCs))]
        #:unless (null? recipients))
    (fprintf pin "~a: ~a" header (car recipients))
    (for ([recipient (in-list (cdr recipients))])
      (define col (let-values ([(line col pos) (port-next-location pin)]) col))
      (fprintf pin ",~a~a"
               (if ((+ col 2 (string-length recipient)) . > . 78)
                 "\n    " " ")
               recipient))
    (newline pin))
  (fprintf pin "Subject: ~a\n" (encode-for-header subject))
  (for ([h (in-list headers)]) (fprintf pin "~a\n" h))
  (newline pin)
  pin)

;; send-mail-message/port:
;;  String String (Listof String) (Listof String) (Listof String) String ...
;;  -> Output-Port

;; -- sender can be anything, though spoofing is not recommended.
;; The recipients must all be valid email addresses, they're passed to
;; sendmail as arguments -- and seems that it handles various name+email
;; formats correctly.  Note that everything is expected to follow RFC
;; conventions.  If any other headers are specified, they are expected
;; to be completely formatted already.  Clients are urged to use
;; close-output-port on the port returned by this procedure as soon as
;; the necessary text has been written, so that the sendmail process can
;; complete.
(define (send-mail-message/port sender subject TOs CCs BCCs . headers)
  (send-mail-core 'send-mail-message/port sender subject TOs CCs BCCs headers))

;; send-mail-message :
;; string x string x list (string) x list (string) x list (string) x
;;   list (string) [x list (string)] -> ()
(define (send-mail-message sender subject TOs CCs BCCs text . headers)
  (define pin
    (send-mail-core 'send-mail-message sender subject TOs CCs BCCs headers))
  (for ([t (in-list text)]) (fprintf pin "~a\n" t))
  (close-output-port pin))
