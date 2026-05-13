#lang racket/base

(require
 rackunit
 "../main.rkt"
) ; end require

(define name "Alice")

(define hello-interpolation
  (interpolation name #'name #f "")
) ; end define hello-interpolation

(define hello-template
  (template (list "hello " "")
            (list hello-interpolation)
  ) ; end template
) ; end define hello-template

(check-true (template? hello-template))
(check-equal? (template-parts hello-template) (list "hello " hello-interpolation ""))
(check-equal? (template-strings hello-template) (list "hello " ""))
(check-equal? (map interpolation-value (template-interpolations hello-template)) (list "Alice"))
(check-equal? (render-template hello-template) "hello Alice")
(check-equal? (render-fstring hello-template) "hello Alice")

(define sum-template
  (template (list "sum = " "")
            (list (interpolation 3 #'(+ 1 2) #f ""))
  ) ; end template
) ; end define sum-template

(check-equal? (render-template sum-template) "sum = 3")
(check-equal? (render-template sum-template
                               #:value->string number->string
              ) ; end render-template
              "sum = 3"
) ; end check-equal?

(define id 42)
(define status "active")

(define sql-template
  (template (list "WHERE id = " " AND status = " "")
            (list (interpolation id #'id #f "")
                  (interpolation status #'status #f "")
            ) ; end list
  ) ; end template
) ; end define sql-template

(define-values (sql params)
  (template->sql sql-template)
) ; end define-values

(check-equal? sql "WHERE id = ? AND status = ?")
(check-equal? params (list 42 "active"))

(check-exn
 exn:fail?
 (lambda ()
   (template->sql sum-template)
 ) ; end lambda
) ; end check-exn

(define html-template
  (template (list "<p>" "</p>")
            (list (interpolation "<script>&\"" #'user-input #f ""))
  ) ; end template
) ; end define html-template

(check-equal? (html-render html-template)
              "<p>&lt;script&gt;&amp;&quot;</p>"
) ; end check-equal?

(check-exn
 exn:fail:contract?
  (lambda ()
    (render-template
     (template (list "too " "many " "strings")
               '()
     ) ; end template
    ) ; end render-template
  ) ; end lambda
) ; end check-exn
