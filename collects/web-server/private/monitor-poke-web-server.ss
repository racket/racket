(module monitor-poke-web-server mzscheme
  (require (lib "contract.ss")
           (lib "match.ss"))
  
  ;; this file contains functions to check whether a given host is responding to HTTP requests,
  ;; specifically to a "HEAD ~a HTTP/1.0" request, where ~a is supplied by the caller.
  
  ;; the original code was Paul Graunke's, refactored & encontracted by John Clements
  
  
  (define (poke-result? result)
    (match result
      [`(fail ,(? string? server-name) ,(? number? server-port) ,(? string? msg)) #t]
      [`(timeout ,(? string? server-name) ,(? number? server-port) ,(? number? timeout-seconds)) #t]
      [`(exn ,(? string? server-name) ,(? number? server-port) ,(? exn? exn)) #t]
      [`(ok) #t]
      [else #f]))
  
  (provide/contract [poke-web-server (channel? ; result-channel
                                      string? ; server-name 
                                      number? ; server-port
                                      number? ; timeout-seconds
                                      . -> . 
                                      void?)]
                    [poke-web-server/path (string? ; path
                                           channel? ; result-channel
                                           string? ; server-name
                                           number? ; server-port
                                           number? ; timeout-seconds
                                           . -> .
                                           void?)]
                    [result->message (poke-result? . -> . string?)])
  
  (define OK-REGEXP (regexp "^HTTP/[0-9]*.[0-9]* 200"))
  
  
  ; result->message : given a poke-result?, produce a reasonable error message.
  (define (result->message result)
    (match result
      [`(fail ,server-name ,server-port ,line) 
       (string-append
        (format "The web server ~a:~a did not respond\n" server-name server-port)
        "to a head request for its home page with an 'okay' result.\n"
        (format "Received: ~a\n" line))]
      [`(timeout ,server-name ,server-port ,timeout-seconds)
       (string-append
        (format "Attempting to send a head request to ~a:~a\n" server-name server-port)
        (format "timed out after ~a seconds.\n" timeout-seconds))]
      [`(exn ,server-name ,server-port ,exn)
       (string-append
        (format "Attempting to send a head request to ~a:~a\n" server-name server-port)
        "resulted in the following exception:\n"
        "\n"
        (format "~a\n" (if (exn? exn)
                           (exn-message exn)
                           exn)))]
      [`(ok) "no error"]))
  
  (define (poke-web-server result-channel server-name server-port timeout-seconds)
    (poke-web-server/path "/" result-channel server-name server-port timeout-seconds))

  (define (poke-web-server/path path result-channel server-name server-port timeout-seconds)
    (let* ([cust (make-custodian)]
           [blow-up-handler (lambda (exn)
                              (channel-put result-channel `(exn ,server-name ,server-port ,exn))
                              (custodian-shutdown-all cust))])
      (parameterize ([current-custodian cust])
        (thread (lambda ()
                  (with-handlers ([void blow-up-handler])
                    (sleep timeout-seconds))
                  (channel-put result-channel `(timeout ,server-name ,server-port ,timeout-seconds))
                  (custodian-shutdown-all cust)))
        (thread
         (lambda ()
           (with-handlers ([void blow-up-handler])
             (let-values ([(in out) (tcp-connect server-name server-port)])
               (fprintf out "HEAD ~a HTTP/1.0\r\n" path)
               (fprintf out "Host: ~a\r\n\r\n" server-name) ; what the jiminy cricket does this line do;?
               (flush-output out) ;; now required for all TCP ports
               (let ([line (read-line in)])
                 (if (regexp-match OK-REGEXP line)
                     (channel-put result-channel '(ok))
                     (channel-put result-channel `(fail ,server-name ,server-port ,line)))
                 (custodian-shutdown-all cust))))))
        (void)))))