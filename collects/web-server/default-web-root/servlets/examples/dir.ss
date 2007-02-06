(require (lib "unit.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "date.ss"))

(unit (import servlet^)
      (export)

  (send/back
   `(html (head (title "Current Directory Page"))
          (body
           (h1 "Current Directory Page")
           (p "The current directory is: " (em ,(path->string (current-directory))))))))

