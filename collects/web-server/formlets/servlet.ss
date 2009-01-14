#lang scheme
(require web-server/servlet
         xml
         "lib.ss")

(provide/contract
 [send/formlet (((formlet/c any/c))
                (#:wrap (xexpr? . -> . response?))                 
                . ->* . any/c)])

(define (send/formlet f
                      #:wrap 
                      [wrapper
                       (lambda (form-xexpr)
                         `(html (head (title "Form Entry"))
                                (body ,form-xexpr)))])
  (formlet-process 
   f
   (send/suspend
    (lambda (k-url)
      (wrapper
       `(form ([action ,k-url])
              ,@(formlet-display f)))))))

(provide/contract
 [embed-formlet (embed/url/c (formlet/c any/c) . -> . xexpr?)])

(define (embed-formlet embed/url f)
  `(form ([action ,(embed/url
                    (lambda (r)
                      (formlet-process f r)))])
         ,@(formlet-display f)))
