(module misc mzscheme
  (require (lib "servlet.ss" "web-server"))
  (require "../private/headelts.ss"
           "../private/util.ss")
  
  ; (listof string string) -> xexpr
  (define (make-link-line url/txt)
    (let ([url (car url/txt)]
          [txt (cadr url/txt)])
      `(LI () (B () (A ((HREF ,(string-append
                                "/servlets/scheme/misc/"
				url))) ,txt)))))
  
  (define links
    '(("standalone.ss" 
       "How to build a stand-alone executable")
      ("graphics.ss" 
       "How to write graphics programs")
      ("script.ss"
       "How to write Unix shell scripts")
      ("batch.ss" 
       "How to write Windows batch files")
      ("cgi.ss" 
       "How to write CGI scripts")
      ("activex.ss" 
       "How to use ActiveX components")
      ("database.ss"
       "How to connect to databases")
      ("system.ss" 
       "How to call low-level system routines")))
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "How to do things in Scheme"))  
      (BODY 
       (H1 "How to do things in Scheme") 
       (UL 
        ,@(map make-link-line links))
       (P)
       
       "If you did't find what you're looking for in the "
       "list above, try")
      " "
      (A ((HREF "/servlets/howtouse.ss#search")) "searching")
      " "
      "in Help Desk. "
      
      "Also, check "
      (a ((href "http://www.htus.org/")) (i "How to Use Scheme"))
      ".")))