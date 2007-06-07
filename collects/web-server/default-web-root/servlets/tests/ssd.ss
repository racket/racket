(module ssd mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout 120)
  (define (start ir)
    (printf "X~n")
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html (head)
              (body
               (ul
                ,@(map (lambda (i)
                         `(li (a ([href ,(embed/url
                                          (lambda (r)
                                            `(html (head) (body ,i))))])
                                 ,(number->string i))))
                       `(1 2 3 4 5 6 7 8 9 0)))))))))
