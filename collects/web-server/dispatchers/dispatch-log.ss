(module dispatch-log mzscheme
  (require (lib "url.ss" "net")
           (lib "date.ss")
           (lib "kw.ss")
           (lib "async-channel.ss")
           (lib "plt-match.ss")
           (lib "contract.ss"))
  (require "dispatch.ss"
           "../request-structs.ss"
           "../private/servlet-helpers.ss")  
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide ; XXX contract kw
   make)  
  
  (define interface-version 'v1)
  (define/kw (make #:key 
                   [log-format 'parenthesized-default]
                   [log-path #f])
    (if log-path
        (case log-format
          [(parenthesized-default extended)
           (let ([log-message (gen-log-message log-format log-path)])
             (lambda (conn req)
               (log-message req)
               (next-dispatcher)))]
          [else
           (lambda (conn req)
             (next-dispatcher))])
        (lambda (conn req)
          (next-dispatcher))))
  
  ; gen-log-message : sym str -> str str sym url str -> str
  ; XXX: check apache log configuration formats
  ; other server's include the original request line,
  ; including the major and minor HTTP version numbers
  ; to produce a string that is displayed into the log file
  ; This is a kill-safe wait-less abstraction
  (define (gen-log-message log-format log-path)
    (define log-ch (make-async-channel))
    (define log-thread
      (thread/suspend-to-kill
       (lambda ()
         (let loop ([log-p #f])
           (sync
            (handle-evt 
             log-ch
             (match-lambda
               [(list req)
                (loop
                 (with-handlers ([exn? (lambda (e)
                                         ((error-display-handler) "dispatch-log.ss: Error writing log entry" e)
                                         (with-handlers ([exn? (lambda (e) #f)])
                                           (close-output-port log-p))
                                         #f)])
                   (define the-log-p
                     (if (not (and log-p (file-exists? log-path)))
                         (begin
                           (unless (eq? log-p #f)
                             (close-output-port log-p))
                           (let ([new-log-p (open-output-file log-path 'append)])
                             (file-stream-buffer-mode new-log-p 'line)
                             new-log-p))
                         log-p))
                   (display
                    (format "~s~n"
                            (case log-format
                              [(parenthesized-default)
                               (list 'from (request-client-ip req)
                                     'to (request-host-ip req)
                                     'for (url->string (request-uri req)) 'at
                                     (date->string (seconds->date (current-seconds)) #t))]
                              [(extended)
                               `((client-ip ,(request-client-ip req))
                                 (host-ip ,(request-host-ip req))
                                 (referer ,(let ([R (headers-assq* #"Referer" (request-headers/raw req))])
                                             (if R
                                                 (header-value R)
                                                 #f)))                                              
                                 (uri ,(url->string (request-uri req)))
                                 (time ,(current-seconds)))]))
                    the-log-p)
                   the-log-p))])))))))
    (lambda args
      (thread-resume log-thread (current-custodian))
      (async-channel-put log-ch args)
      (void))))