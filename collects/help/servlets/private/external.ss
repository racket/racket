(module external mzscheme
  (require (lib "servlet.ss" "web-server") (lib "defmacro.ss") "headelts.ss")
  (provide external-box check-external)
  (define external-box (box #f))
  (define (check-external show url)
    (when (unbox external-box)
      (show 
       `(html (head ,hd-css ,@hd-links (title "Servlet unavailable"))
              (body (h3 (font ([color "red"]) "Servlet unavailable"))
                    (p)
                    "Because the PLT Help Desk server is accepting external"
                    " connections, the requested Help Desk servlet"
                    (blockquote (tt ,url))
                    "is not available."))))))
