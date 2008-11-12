#lang scheme/base
(require scheme/contract)
(require "../private/util.ss"
         web-server/http)

(define (with-errors-to-browser send/finish-or-back thunk)
  (with-handlers ([exn? (lambda (exn)
                          (send/finish-or-back
                           `(html (head (title "Servlet Error"))
                                  (body ([bgcolor "white"])
                                        (p "The following error occured: "
                                           (pre ,(exn->string exn)))))))])
    (thunk)))

(provide/contract
 [with-errors-to-browser
  ((response? . -> . request?)
   (-> any)
   . -> .
   any)])
