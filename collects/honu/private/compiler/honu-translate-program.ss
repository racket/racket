(module honu-translate-program mzscheme

  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss"))
  
  (require "../../ast.ss")
  (require "../../tenv.ss")
  (require "honu-translate-type-defn.ss")
  (require "honu-translate-function.ss")
  (require "honu-translate-class.ss")
  (require "honu-translate-subclass.ss")
  (require "honu-translate-utils.ss")
  
  (provide/contract [honu-translate-program
                     (tenv?
                      honu-program?
                      . -> .
;                      (listof (syntax/c any/c))])
                      list?)])
  (define (honu-translate-program tenv pgm)
    (map (lambda (d)
           (honu-translate-defn tenv pgm d))
         (filter (lambda (d)
                   (not (honu-mixin? d)))
                 (honu-program-defns pgm))))
             
  (define (honu-translate-defn tenv pgm defn)
    (cond
      [(honu-function? defn) (honu-translate-function tenv defn)]
      [(honu-type-defn? defn) (honu-translate-type-defn tenv defn)]
      [(honu-class? defn) (honu-translate-class tenv defn)]
 ;     [(honu-mixin? defn) (honu-translate-mixin pgm defn)]
      [(honu-subclass? defn) 
       (let ([mixin (find (lambda (d)
                            (and (honu-mixin? d)
                                 (tenv-key=? (honu-mixin-name d)
                                             (honu-subclass-mixin defn))))
                          (honu-program-defns pgm))])
         (honu-translate-subclass tenv mixin defn))]))
  
  )
