
(module sendurl mzscheme
  (require (lib "process.ss"))

  (provide send-url)
  
  (define (send-url str)
    (case (system-type)
      [(macos macosx)
       (send-event "MACS" "GURL" "GURL" str)] ;; actually, I think GURL means something slightly different...
      [(windows)
       ;; Try to get a MrEd function...
       (let ([get-res (with-handlers ([not-break-exn? (lambda (x) #f)])
		        (dynamic-require '(lib "mred.ss" "mred") 'get-resource))])
         (if get-res
             (let ([b (box "")])
               (unless (get-res "HKEY_CLASSES_ROOT" "htmlfile\\shell\\open\\command" b)
                 (error 'send-url "couldn't find URL opener in the registry"))
	       (let-values ([(out in id err status) (apply 
						     values
						     (process (format "~a ~a" (unbox b) str)))])
	         (close-output-port in)
		 (close-input-port out)
		 (close-input-port err)))
	     (error 'send-url "don't know how to open URL in Windows without MrEd")))]
      [(unix)
       (cond
         [(find-executable-path "opera" #f)
          =>
          (lambda (browser-path) 
            ;; opera may not return -- always open asyncronously
            ;; opera starts a new browser automatically, if it can't find one
            (let-values ([(out in id err status) (apply 
                                                  values
                                                  (process* browser-path "-remote" (format "openURL(~a)" str)))])
              (close-output-port in)
              (close-input-port out)
              (close-input-port err)))]
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
          (error 'open-url "Couldn't find Netscape or Opera to open URL: ~e" str)])]
      [else (error 'send-url "don't know how to open URL on platform: ~s" (system-type))])))
