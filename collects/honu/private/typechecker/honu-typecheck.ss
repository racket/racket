(module honu-typecheck mzscheme
  (require (lib "struct.ss")
           (lib "contract.ss"))
  
  (require "../../ast.ss")
  (require "../../tenv.ss")
  (require "honu-type-utils.ss")
  (require "honu-convert-static.ss")
  (require "honu-typecheck-function.ss")
  (require "honu-typecheck-type-defn.ss")
  (require "honu-typecheck-class.ss")
  (require "honu-typecheck-mixin.ss")
  (require "honu-typecheck-prechecks.ss")
  (require "../../read-error-with-stx.ss")

;  (provide honu-typecheck-program)
  (provide/contract [honu-typecheck-program
                     (tenv?
                      honu-program?
                      . -> .
                      honu-program?)])
  (define (honu-typecheck-program tenv pgm)
    (let ((pgm (honu-convert-static tenv pgm)))
      (check-uses-of-this pgm)
      (let-values (((new-defns)
                    (map (lambda (d)
                           (honu-typecheck-defn tenv d))
                         (honu-program-defns pgm))))
        (let ((new-pgm (copy-struct honu-program pgm
                         (honu-program-defns new-defns))))
          new-pgm))))
  
  (define (honu-typecheck-defn tenv defn)
    (cond
      [(honu-function? defn)
       (honu-typecheck-function tenv defn)]
      [(honu-type-defn? defn)
       (honu-typecheck-type-defn tenv defn)]
      [(honu-subclass? defn)
       ;; we don't need to check this anymore, because it's checked in add-defns-to-tenv
       ;       (honu-typecheck-subclass pgm defn)]
       defn]
      [(honu-mixin? defn)
       (honu-typecheck-mixin tenv defn)]
      [(honu-class? defn)
       (honu-typecheck-class tenv defn)]
      [else
       (raise-read-error-with-stx
        "Unknown type of top-level definition."
        (honu-ast-src-stx defn))]))
  )
