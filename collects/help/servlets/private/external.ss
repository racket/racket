(module external mzscheme

  (require (lib "servlet.ss" "web-server")
           (lib "defmacro.ss"))

  (require "headelts.ss")

  (provide external-box
	   check-external)

  (define external-box (box #f))
  
  (define (check-external show url)
    (when (unbox external-box)
      (show 
       `(HTML
	 (HEAD ,hd-css
               ,@hd-links
	       (TITLE "Servlet unavailable"))
	 (BODY
	  (H3
	   (FONT ((COLOR "red"))
		 "Servlet unavailable"))
	  (P)
	  "Because the PLT Help Desk server is "
	  "accepting external connections, the "
	  "requested Help Desk servlet"
	  (BLOCKQUOTE (TT ,url))
	  "is not available."))))))









