(module version (lib "a-unit.ss")
  (require "sig.ss"
           (lib "mred-sig.ss" "mred")
           (lib "string.ss")
           (lib "list.ss"))
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
                      specs))))
