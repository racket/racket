
(module sendurl mzscheme
  (require (lib "process.ss"))

  (provide send-url)
  
  (define (send-url str)
    (case (system-type)
      [(macos macosx)
       (send-event "MACS" "GURL" "GURL" str)] ;; actually, I think GURL means something slightly different...
      [(unix)
       (cond
         [(find-executable-path "opera" #f)
          =>
          (lambda (browser-path) 
            ;; opera may not return -- always open asyncronously
            ;; opera starts a new browser automatically, if it can't find one
            (process* browser-path "-remote" (format "openURL(~a)" str))
            (void))]
         [(find-executable-path "netscape" #f)
          =>
          (lambda (browser-path)
            ;; netscape's -remote returns with an error code, if no
            ;; netscape is around. start a new netscape in that case.
            (or (system* browser-path "-remote" (format "openURL(~a)" str))
                (let-values ([(out in id err status) (apply values (process* browser-path str))])
                  (close-output-port in)
                  (close-input-port out)
                  (close-input-port err))))]
         [else
          (error 'open-url "Couldn't find Netscape or Opera to open url: ~e" str)])]
      [else (error 'send-url "don't know how to open url on platform: ~s" (system-type))])))
