(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "date.ss"))

(unit/sig () (import servlet^)

  (send/back
   `(html (head (title "Current Directory Page"))
          (body
           (h1 "Current Directory Page")
           (p "The current directory is: " (em ,(current-directory)))))))

