
(module sendurl mzscheme
  (require (lib "process.ss"))

  (provide send-url)
  
  (define (send-url str)
    (case (system-type)
      [(macos macosx)
       (send-event "MACS" "GURL" "GURL" str)] ;; actually, I think GURL means something slightly different...
      [(unix)
       (let ([netscape-path (find-executable-path "netscape" #f)])
         (unless netscape-path
           (error 'open-url "Couldn't find Netscape."))
         
         (or (system* netscape-path "-remote" (format "openURL(~a)" str))
             (let-values ([(out in id err status) (apply values (process* netscape-path str))])
               (close-output-port in)
               (close-input-port out)
               (close-input-port err))))]
      [else (error 'send-url "don't know how to open url on platform: ~s" (system-type))])))
