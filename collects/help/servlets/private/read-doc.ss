(module read-doc mzscheme

  (require (lib "etc.ss"))
  (require (lib "getinfo.ss" "setup"))

  (require "util.ss")
  (require "read-lines.ss")
  (require "headelts.ss")

  (provide read-doc)

  ; extracts help desk message
  (define (get-message coll)
    (with-handlers ; collection may not exist
     ((void (lambda _ #f)))
     ((get-info (list coll)) 
      'help-desk-message
      (lambda () #f))))

  (define no-offset-format "file=~a&caption=~a")
  (define offset-format (string-append no-offset-format "&offset=~a#temp"))

  (define (build-page file caption coll offset)
    (let ([msg (get-message coll)])
      (if msg
	  `(HTML
            (HEAD (TITLE "PLT Help Desk") 
                  ,hd-css)
            (BODY
             ,(format-collection-message msg)
             (HR)
             ,(read-lines file caption offset)))
	  `(HTML
	    (HEAD (TITLE "PLT Help Desk") 
		  ,hd-css)
	    ,(read-lines file caption offset)))))

  (define read-doc 
    (opt-lambda (file caption coll [offset #f])
      (build-page file caption coll offset))))
