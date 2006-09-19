; Author: Paul Graunke
#cs(module servlet mzscheme
     (require (lib "servlet-env.ss" "web-server" "tools")
              (lib "error.ss" "htdp")
              (lib "xml.ss" "xml")
              (lib "list.ss")
              (lib "prim.ss" "lang")
              (lib "unitsig.ss"))
     (provide (all-from-except (lib "servlet-env.ss" "web-server" "tools") build-suspender)
              (rename wrapped-build-suspender build-suspender))
     
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
