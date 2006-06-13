(module dispatch-log mzscheme
  (require (lib "url.ss" "net")
           (lib "date.ss")
           (lib "kw.ss")
           (lib "async-channel.ss")
           (lib "plt-match.ss"))
  (require "dispatch.ss"
           "../servlet-helpers.ss")  
  (provide interface-version
           make)
  
  (define interface-version 'v1)
  (define/kw (make #:key 
                   [log-format 'parenthesized-default]
                   [log-path #f])
    (if log-path
        (case log-format
          [(parenthesized-default)
           (let ([log-message (gen-log-message log-format log-path)])
             (lambda (conn req)
               (let ([host (get-host (request-uri req) (request-headers/raw req))])
                 (log-message (request-host-ip req)
                              (request-client-ip req)
                              (request-method req)
                              (request-uri req)
                              host)
                 (next-dispatcher))))]
          [else
           (lambda (conn req) (next-dispatcher))])
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
           (with-handlers ([exn? (lambda (e) (loop #f))])
             (if (not (and log-p (file-exists? log-path)))
                 (begin
                   (unless (eq? log-p #f)
                     (close-output-port log-p))
                   (let ([new-log-p (open-output-file log-path 'append)])
                     (file-stream-buffer-mode new-log-p 'line)
                     (loop new-log-p)))
                 (sync
                  (handle-evt 
                   log-ch
                   (match-lambda
                     [(list host-ip client-ip method uri host)
                      (display
                       (format "~s~n"
                               (list 'from client-ip 'to host-ip 'for (url->string uri) 'at
                                     (date->string (seconds->date (current-seconds)) #t)))
                       log-p)
                      (loop log-p)])))))))))
    (lambda args
      (thread-resume log-thread (current-custodian))
      (async-channel-put log-ch args)
      (void))))