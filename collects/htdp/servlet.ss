; Author: Paul Graunke
#cs(module servlet mzscheme
     (require web-server/servlet-env
              web-server/servlet
              htdp/error
              xml
              scheme/contract
              mzlib/etc)
     (provide (all-from web-server/servlet-env)
              (all-from web-server/servlet))
     (provide/contract
      [build-suspender 
       (((listof xexpr/c)
         (listof xexpr/c))
        ((listof (list/c symbol? string?))
         (listof (list/c symbol? string?)))
        . ->* . 
        (string?
         . -> .
         xexpr/c))])
     
     ; build-suspender : (listof html) (listof html) [(listof (cons sym str))] [(listof (cons sym str))] -> str -> response
     (define build-suspender
       (opt-lambda (title content [body-attributes '([bgcolor "white"])] [head-attributes null])
         (lambda (k-url)
           `(html (head ,head-attributes
                        (meta ([http-equiv "Pragma"] [content "no-cache"])) ; don't cache in netscape
                        (meta ([http-equiv "Expires"] [content "-1"])) ; don't cache in IE
                        ; one site said to use -1, another said to use 0.
                        (title . ,title))
                  (body ,body-attributes
                        (form ([action ,k-url] [method "post"])
                              ,@content)))))))
