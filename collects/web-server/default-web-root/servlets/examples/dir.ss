(module dir mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (send/back
     `(html (head (title "Current Directory Page"))
            (body
             (h1 "Current Directory Page")
             (p "The current directory is: " (em ,(path->string (current-directory)))))))))