(module open-url mzscheme
  (provide open-url)
  
  (define (open-url str)
    (case (system-type)
      [(macos)
       (send-event "MACS" "GURL" "GURL" str)]
      [(unix)
       (define netscape-path (find-executable-path "netscape" #f))
       
       (unless netscape-path
         (error 'open-url "Couldn't find Netscape."))
       
       (or (system* netscape-path "-remote" (format "openURL(~a)" url))
           (let-values ([(out in id err status) (apply values (process* netscape-path url))])
             (close-output-port in)
             (close-input-port out)
             (close-input-port err)))]
      [else (error 'open-url "don't know how to open url on platform: ~s" (system-type))])))
