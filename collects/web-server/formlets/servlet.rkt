#lang scheme
(require web-server/servlet
         web-server/private/xexpr
         "lib.ss")

(provide/contract
 [send/formlet ((formlet*/c)
                (#:wrap (pretty-xexpr/c . -> . response/c))                 
                . ->* . any)])

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
 [embed-formlet (embed/url/c formlet*/c . -> . pretty-xexpr/c)])

(define (embed-formlet embed/url f)
  `(form ([action ,(embed/url
                    (lambda (r)
                      (formlet-process f r)))])
         ,@(formlet-display f)))
