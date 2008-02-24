; Author: Paul Graunke
#cs(module servlet mzscheme
     (require web-server/servlet-env
              htdp/error
              (lib "xml.ss" "xml")
              mzlib/etc)
     (provide (all-from web-server/servlet-env)
              (rename wrapped-build-suspender build-suspender))
     
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
                              ,@content))))))
     
     (define wrapped-build-suspender
       (case-lambda
         [(title content)
          (check-suspender2 title content)
          (build-suspender title content)]
         [(title content body-attributes)
          (check-suspender3 title content body-attributes)
          (build-suspender title content body-attributes)]
         [(title content body-attributes head-attributes)
          (check-suspender4 title content body-attributes head-attributes)
          (build-suspender title content body-attributes head-attributes)]))
     
     ; : tst tst -> void
     (define (check-suspender2 title content)
       (check-arg 'build-suspender (listof? xexpr? title) "(listof xexpr[HTML])" "1st" title)
       (check-arg 'build-suspender (listof? xexpr? content) "(listof xexpr[HTML])" "2nd" content))
     
     ; : tst tst tst -> void
     (define (check-suspender3 title content body-attributes)
       (check-suspender2 title content)
       (check-arg 'build-suspender (listof? attribute-pair? body-attributes)
                  "(listof (cons sym str))" "3rd" body-attributes))
     
     ; : tst tst tst tst -> void
     (define (check-suspender4 title content body-attributes head-attributes)
       (check-suspender3 title content body-attributes)
       (check-arg 'build-suspender (listof? attribute-pair? head-attributes)
                  "(listof (cons sym str))" "4th" head-attributes))
     
     ; : tst -> bool
     (define (attribute-pair? b)
       (and (pair? b)
            (symbol? (car b))
            (string? (cdr b)))))
