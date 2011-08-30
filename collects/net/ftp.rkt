#lang racket/base

(require racket/date racket/file racket/port racket/tcp)

(provide ftp-connection?
         ftp-cd
         ftp-establish-connection ftp-establish-connection*
         ftp-close-connection
         ftp-directory-list
         ftp-download-file
         ftp-make-file-seconds)

;; opqaue record to represent an FTP connection:
(define-struct ftp-connection (in out))

(define re:multi-response-start #rx#"^[0-9][0-9][0-9]-")
(define re:response-end #rx#"^[0-9][0-9][0-9] ")

(define (check-expected-result line expected)
  (when expected
    (unless (ormap (lambda (expected)
                     (bytes=? expected (subbytes line 0 3)))
                   (if (bytes? expected)
                       (list expected)
                       expected))
      (error 'ftp "expected result code ~a, got ~a" expected line))))

;; ftp-check-response : input-port output-port bytes-or-byteslist-or-#f (bytes any -> any) any -> any
;;
;;  Checks a standard-format response, checking for the given
;;  expected 3-digit result code if expected is not #f.
;;
;;  While checking, the function sends response lines to
;;  diagnostic-accum. This function -accum functions can return a
;;  value that accumulates over multiple calls to the function, and
;;  accum-start is used as the initial value. Use `void' and
;;  `(void)' to ignore the response info.
;;
;; If an unexpected result is found, an exception is raised, and the
;; stream is left in an undefined state.
(define (ftp-check-response tcpin tcpout expected diagnostic-accum accum-start)
  (flush-output tcpout)
  (let ([line (read-bytes-line tcpin 'any)])
    (cond
     [(eof-object? line)
      (error 'ftp "unexpected EOF")]
     [(regexp-match re:multi-response-start line)
      (check-expected-result line expected)
      (let ([re:done (regexp (format "^~a " (subbytes line 0 3)))])
        (let loop ([accum (diagnostic-accum line accum-start)])
          (let ([line (read-bytes-line tcpin 'any)])
            (cond [(eof-object? line)
                   (error 'ftp "unexpected EOF")]
                  [(regexp-match re:done line)
                   (diagnostic-accum line accum)]
                  [else
                   (loop (diagnostic-accum line accum))]))))]
     [(regexp-match re:response-end line)
      (check-expected-result line expected)
      (diagnostic-accum line accum-start)]
     [else
      (error 'ftp "unexpected result: ~e" line)])))

(define (get-month month-bytes)
  (cond [(assoc month-bytes
                '((#"Jan" 1) (#"Feb" 2) (#"Mar" 3) (#"Apr" 4) (#"May" 5)
                  (#"Jun" 6) (#"Jul" 7) (#"Aug" 8) (#"Sep" 9) (#"Oct" 10)
                  (#"Nov" 11) (#"Dec" 12)))
         => cadr]
        [else (error 'get-month "bad month: ~s" month-bytes)]))

(define (bytes->number bytes)
  (string->number (bytes->string/latin-1 bytes)))

(define re:date #rx#"(...) *(.*) (..):(..)|(...) *([0-9]*) +(....)")

(define (ftp-make-file-seconds ftp-date-str)
  (define date-list (regexp-match re:date (string->bytes/utf-8 ftp-date-str)))
  (if (not (list-ref date-list 4))
      (find-seconds 0 0 0
                    (bytes->number (list-ref date-list 6))
                    (get-month (list-ref date-list 5))
                    (bytes->number (list-ref date-list 7)))
      (let* ([cur-secs (current-seconds)]
             [cur-date (seconds->date cur-secs)]
             [cur-year (date-year cur-date)]
             [tzofs    (date-time-zone-offset cur-date)]
             [minute (bytes->number (list-ref date-list 4))]
             [hour   (bytes->number (list-ref date-list 3))]
             [day    (bytes->number (list-ref date-list 2))]
             [month  (get-month (list-ref date-list 1))]
             [guess  (+ (find-seconds 0 minute hour day month cur-year) tzofs)])
        (if (guess . <= . cur-secs)
          guess
          (+ (find-seconds 0 minute hour day month (sub1 cur-year)) tzofs)))))

(define re:passive #rx#"\\((.*),(.*),(.*),(.*),(.*),(.*)\\)")

(define (establish-data-connection tcp-ports)
  (fprintf (ftp-connection-out tcp-ports) "PASV\r\n")
  (let ([response (ftp-check-response
                   (ftp-connection-in tcp-ports)
                   (ftp-connection-out tcp-ports)
                   #"227"
                   (lambda (s ignore) s) ; should be the only response
                   (void))])
    (let* ([reg-list (regexp-match re:passive response)]
           [pn1 (and reg-list
                     (bytes->number (list-ref reg-list 5)))]
           [pn2 (bytes->number (list-ref reg-list 6))])
      (unless (and reg-list pn1 pn2)
        (error 'ftp "can't understand PASV response: ~e" response))
      (let-values ([(tcp-data tcp-data-out)
                    (tcp-connect (format "~a.~a.~a.~a"
                                         (list-ref reg-list 1)
                                         (list-ref reg-list 2)
                                         (list-ref reg-list 3)
                                         (list-ref reg-list 4))
                                 (+ (* 256 pn1) pn2))])
        (fprintf (ftp-connection-out tcp-ports) "TYPE I\r\n")
        (ftp-check-response (ftp-connection-in tcp-ports)
                            (ftp-connection-out tcp-ports)
                            #"200" void (void))
        (tcp-abandon-port tcp-data-out)
        tcp-data))))

;; Used where version 0.1a printed responses:
(define (print-msg s ignore)
  ;; (printf "~a\n" s)
  (void))

(define (ftp-establish-connection* in out username password)
  (ftp-check-response in out #"220" print-msg (void))
  (fprintf out "USER ~a\r\n" username)
  (let ([no-password? (ftp-check-response
                       in out (list #"331" #"230")
                       (lambda (line 230?)
                         (or 230? (regexp-match #rx#"^230" line)))
                       #f)])
    (unless no-password?
      (fprintf out "PASS ~a\r\n" password)
      (ftp-check-response in out #"230" void (void))))
  (make-ftp-connection in out))

(define (ftp-establish-connection server-address server-port username password)
  (let-values ([(tcpin tcpout) (tcp-connect server-address server-port)])
    (ftp-establish-connection* tcpin tcpout username password)))

(define (ftp-close-connection tcp-ports)
  (fprintf (ftp-connection-out tcp-ports) "QUIT\r\n")
  (ftp-check-response (ftp-connection-in tcp-ports)
                      (ftp-connection-out tcp-ports)
                      #"221" void (void))
  (close-input-port (ftp-connection-in tcp-ports))
  (close-output-port (ftp-connection-out tcp-ports)))

(define (ftp-cd ftp-ports new-dir)
  (fprintf (ftp-connection-out ftp-ports) "CWD ~a\r\n" new-dir)
  (ftp-check-response (ftp-connection-in ftp-ports)
                      (ftp-connection-out ftp-ports)
                      #"250" void (void)))

(define re:dir-line
  (regexp (string-append
           "^(.)(.*) ((?i:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)"
           " .* [0-9][0-9]:?[0-9][0-9]) (.*)$")))

(define (ftp-directory-list tcp-ports [path #f])
  (define tcp-data (establish-data-connection tcp-ports))
  (if path
    (fprintf (ftp-connection-out tcp-ports) "LIST ~a\r\n" path)
    (fprintf (ftp-connection-out tcp-ports) "LIST\r\n"))
  (ftp-check-response (ftp-connection-in tcp-ports)
                      (ftp-connection-out tcp-ports)
                      (list #"150" #"125") void (void))
  (define lines (port->lines tcp-data))
  (close-input-port tcp-data)
  (ftp-check-response (ftp-connection-in tcp-ports)
                      (ftp-connection-out tcp-ports)
                      #"226" print-msg (void))
  (for*/list ([l (in-list lines)]
              [m (in-value (cond [(regexp-match re:dir-line l) => cdr]
                                 [else #f]))]
              #:when m)
    (define size (cond [(and (equal? "-" (car m))
                             (regexp-match #rx"([0-9]+) *$" (cadr m)))
                        => cadr]
                       [else #f]))
    (define r `(,(car m) ,@(cddr m)))
    (if size `(,@r ,size) r)))

(define (ftp-download-file tcp-ports folder filename)
  ;; Save the file under the name tmp.file, rename it once download is
  ;; complete this assures we don't over write any existing file without
  ;; having a good file down
  (let* ([tmpfile (make-temporary-file
                   (string-append
                    (regexp-replace
                     #rx"~"
                     (path->string (build-path folder "ftptmp"))
                     "~~")
                    "~a"))]
         [new-file (open-output-file tmpfile #:exists 'replace)]
         [tcp-data (establish-data-connection tcp-ports)])
    (fprintf (ftp-connection-out tcp-ports) "RETR ~a\r\n" filename)
    (ftp-check-response (ftp-connection-in tcp-ports)
                        (ftp-connection-out tcp-ports)
                        (list #"125" #"150") print-msg (void))
    (copy-port tcp-data new-file)
    (close-output-port new-file)
    (close-input-port tcp-data)
    (ftp-check-response (ftp-connection-in tcp-ports)
                        (ftp-connection-out tcp-ports)
                        #"226" print-msg (void))
    (rename-file-or-directory tmpfile (build-path folder filename) #t)))
