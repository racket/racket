#lang scheme/unit

  (require "sig.ss"
           (lib "mred-sig.ss" "mred")
           mzlib/string
           mzlib/list)
  (import)
  (export (rename framework:version^
                  [-version version]))
  
  (define specs null)
  
  (define (-version)
    (foldr (lambda (entry sofar)
             (let ([sep (first entry)]
                   [num (second entry)])
               (string-append sofar sep num)))
           (version)
           specs))
  
  (define (add-spec sep num)
    (set! specs (cons (list (expr->string sep) (format "~a" num))
                      specs)))
