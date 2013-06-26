#lang racket/base
(require rackunit
         racket/class
         (prefix-in srfi: srfi/19)
         db/base
         "../config.rkt")

(provide query:test)

(define vehicles-result
  (rows-result
   '(((name . "type")) ((name . "maker")) ((name . "model")))
   `(#("car"  "honda"   "civic")
     #("car"  "ford"    "focus")
     #("car"  "ford"    "pinto")
     #("bike" "giant"   "boulder")
     #("bike" "schwinn" ,sql-null))))

(define query:test
  (test-suite "Query utilities"
    (test-suite "group-rows"
      (test-case "single grouping"
        (check-equal?
         (rows-result-rows (group-rows vehicles-result #:group '#("type")))
         `(#("car" (#("honda" "civic")
                    #("ford" "focus")
                    #("ford" "pinto")))
           #("bike" (#("giant" "boulder")
                     #("schwinn" ,sql-null))))))
      (test-case "multiple groupings"
        (check-equal?
         (rows-result-rows
          (group-rows vehicles-result #:group '(#("type") #("maker"))))
         `(#("car" (#("honda" (#("civic")))
                    #("ford"  (#("focus") #("pinto")))))
           #("bike" (#("giant" (#("boulder")))
                     #("schwinn" ()))))))
      (test-case "multiple groupings, preserve null rows"
        (check-equal?
         (rows-result-rows
          (group-rows vehicles-result
                      #:group '(#("type") #("maker"))
                      #:group-mode '(preserve-null-rows)))
         `(#("car" (#("honda" (#("civic")))
                    #("ford"  (#("focus") #("pinto")))))
           #("bike" (#("giant" (#("boulder")))
                     #("schwinn" (#(,sql-null))))))))
      (test-case "multiple groupings, list"
        (check-equal?
         (rows-result-rows
          (group-rows vehicles-result
                      #:group '(#("type") #("maker"))
                      #:group-mode '(list)))
         `(#("car" (#("honda" ("civic"))
                    #("ford"  ("focus" "pinto"))))
           #("bike" (#("giant" ("boulder"))
                     #("schwinn" ())))))))))
