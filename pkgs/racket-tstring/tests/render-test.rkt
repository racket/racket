#lang racket/base

(require
 rackunit
 "../main.rkt"
) ; end require

(define name "Alice")

(define hello-template
  (template (list "hello " "")
            (list (interpolation name #'name 'identifier #'name))
  ) ; end template
) ; end define hello-template

(check-true (template? hello-template))
(check-equal? (template-strings hello-template) (list "hello " ""))
(check-equal? (map interpolation-value (template-interpolations hello-template))
              (list "Alice")
) ; end check-equal?
(check-equal? (render-template hello-template) "hello Alice")
(check-equal? (render-fstring hello-template) "hello Alice")

(define sum-template
  (template (list "sum = " "")
            (list (interpolation 3 #'(+ 1 2) 'expression #'(+ 1 2)))
  ) ; end template
) ; end define sum-template

(check-equal? (render-template sum-template) "sum = 3")
(check-equal? (render-template sum-template #:value->string number->string) "sum = 3")

(define id 42)
(define status "active")

(define sql-template
  (template (list "WHERE id = " " AND status = " "")
            (list (interpolation id #'id 'identifier #'id)
                  (interpolation status #'status 'identifier #'status)
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
            (list (interpolation "<script>&\"" #'user-input 'identifier #'user-input))
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
              (list (interpolation 1 #'x 'identifier #'x))
    ) ; end template
   ) ; end render-template
 ) ; end lambda
) ; end check-exn
