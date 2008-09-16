#lang scheme
(require web-server/servlet
         xml
         "lib.ss")

(provide/contract
 [send/formlet ((formlet/c any/c) . -> . any/c)])

(define (send/formlet f)
  (formlet-process 
   f
   (send/suspend
    (lambda (k-url)
      `(form ([action ,k-url])
             ,@(formlet-display f))))))

(provide/contract
 [embed-formlet (embed/url/c (formlet/c any/c) . -> . xexpr?)])

(define (embed-formlet embed/url f)
  `(form ([action ,(embed/url
                    (lambda (r)
                      (formlet-process f r)))])
         ,@(formlet-display f)))