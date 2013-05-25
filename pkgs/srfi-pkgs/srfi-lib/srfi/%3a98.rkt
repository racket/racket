#lang scheme/base
(require srfi/98
         scheme/mpair)

(provide get-environment-variable
         (rename-out (mpair:get-environment-variables
                      get-environment-variables)))

(define (mpair:get-environment-variables)
  (list->mlist (map (lambda (pair)
                      (mcons (car pair) (cdr pair)))
                    (get-environment-variables))))
