(module ftp-unit (lib "a-unit.ss")
  ;; Version 0.2
  ;;   Version 0.1a
  ;;   Micah Flatt
  ;;   06-06-2002
  (require (lib "date.ss") (lib "file.ss") (lib "port.ss") "ftp-sig.ss")
  (import)
  (export ftp^)

  ;; opqaue record to represent an FTP connection:
  (define-struct tcp-connection (in out))

  (define tzoffset (date-time-zone-offset (seconds->date (current-seconds))))

  (define re:multi-response-start #rx#"^[0-9][0-9][0-9]-")
  (define re:response-end #rx#"^[0-9][0-9][0-9] ")

  (define (check-expected-result line expected)
    (when expected
      (unless (ormap (lambda (expected)
                       (bytes=? expected (subbytes line 0 3)))
                     (if (bytes? expected)
                       (list expected)
                       expected))
        (error 'ftp "exected result code ~a, got ~a" expected line))))

  ;; ftp-check-response : input-port output-port bytes-or-byteslist-or-#f (bytes any -> any) any -> any
  ;;
  ;;  Checks a standard-format response, checking for the given
  ;;  expected 3-digit result code if expected is not #f.
  ;;
  ;;  While checking, the function sends reponse lines to
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
    (let ([date-list (regexp-match re:date (string->bytes/locale ftp-date-str))])
      (if (not (list-ref date-list 4))
        (find-seconds 0
                      0
                      2
                      (bytes->number (list-ref date-list 6))
                      (get-month (list-ref date-list 5))
                      (bytes->number (list-ref date-list 7)))
        (+ (find-seconds 0
                         (bytes->number (list-ref date-list 4))
                         (bytes->number (list-ref date-list 3))
                         (bytes->number (list-ref date-list 2))
                         (get-month (list-ref date-list 1))
                         2002)
           tzoffset))))

  (define re:passive #rx#"\\((.*),(.*),(.*),(.*),(.*),(.*)\\)")

  (define (establish-data-connection tcp-ports)
    (fprintf (tcp-connection-out tcp-ports) "PASV\n")
    (let ([response (ftp-check-response
                     (tcp-connection-in tcp-ports)
                     (tcp-connection-out tcp-ports)
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
          (fprintf (tcp-connection-out tcp-ports) "TYPE I\n")
          (ftp-check-response (tcp-connection-in tcp-ports)
                              (tcp-connection-out tcp-ports)
                              #"200" void (void))
          (close-output-port tcp-data-out)
          tcp-data))))

  ;; Used where version 0.1a printed responses:
  (define (print-msg s ignore)
    ;; (printf "~a\n" s)
    (void))

  (define (ftp-establish-connection* in out username password)
    (ftp-check-response in out #"220" print-msg (void))
    (display (bytes-append #"USER " (string->bytes/locale username) #"\n") out)
    (let ([no-password? (ftp-check-response
                         in out (list #"331" #"230")
                         (lambda (line 230?)
                           (or 230? (regexp-match #rx#"^230" line)))
                         #f)])
      (unless no-password?
        (display (bytes-append #"PASS " (string->bytes/locale password) #"\n")
                 out)
        (ftp-check-response in out #"230" void (void))))
    (make-tcp-connection in out))

  (define (ftp-establish-connection server-address server-port username password)
    (let-values ([(tcpin tcpout) (tcp-connect server-address server-port)])
      (ftp-establish-connection* tcpin tcpout username password)))

  (define (ftp-close-connection tcp-ports)
    (fprintf (tcp-connection-out tcp-ports) "QUIT\n")
    (ftp-check-response (tcp-connection-in tcp-ports)
                        (tcp-connection-out tcp-ports)
                        #"221" void (void))
    (close-input-port (tcp-connection-in tcp-ports))
    (close-output-port (tcp-connection-out tcp-ports)))

  (define (filter-tcp-data tcp-data-port regular-exp)
    (let loop ()
      (let ([theline (read-bytes-line tcp-data-port 'any)])
        (cond [(or (eof-object? theline) (< (bytes-length theline) 3))
               null]
              [(regexp-match regular-exp theline)
               => (lambda (m) (cons (cdr m) (loop)))]
              [else
               ;; ignore unrecognized lines?
               (loop)]))))

  (define (ftp-cd ftp-ports new-dir)
    (display (bytes-append #"CWD " (string->bytes/locale new-dir) #"\n")
             (tcp-connection-out ftp-ports))
    (ftp-check-response (tcp-connection-in ftp-ports)
                        (tcp-connection-out ftp-ports)
                        #"250" void (void)))

  (define re:dir-line
    #rx#"^(.)......... .* ([A-Z].* .* [0-9][0-9]:?[0-9][0-9]) (.*)$")

  (define (ftp-directory-list tcp-ports)
    (let ([tcp-data (establish-data-connection tcp-ports)])
      (fprintf (tcp-connection-out tcp-ports) "LIST\n")
      (ftp-check-response (tcp-connection-in tcp-ports)
                          (tcp-connection-out tcp-ports)
                          #"150" void (void))
      (let ([dir-list (filter-tcp-data tcp-data re:dir-line)])
        (close-input-port tcp-data)
        (ftp-check-response (tcp-connection-in tcp-ports)
                            (tcp-connection-out tcp-ports)
                            #"226" print-msg (void))
        (map (lambda (l) (map bytes->string/locale l)) dir-list))))

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
           [new-file (open-output-file tmpfile 'replace)]
           [tcpstring (bytes-append #"RETR "
                                    (string->bytes/locale filename)
                                    #"\n")]
           [tcp-data (establish-data-connection tcp-ports)])
      (display tcpstring (tcp-connection-out tcp-ports))
      (ftp-check-response (tcp-connection-in tcp-ports)
                          (tcp-connection-out tcp-ports)
                          #"150" print-msg (void))
      (copy-port tcp-data new-file)
      (close-output-port new-file)
      (close-input-port tcp-data)
      (ftp-check-response (tcp-connection-in tcp-ports)
                          (tcp-connection-out tcp-ports)
                          #"226" print-msg (void))
      (rename-file-or-directory tmpfile (build-path folder filename) #t)))

  ;; (printf "FTP Client Installed...\n")
  )
