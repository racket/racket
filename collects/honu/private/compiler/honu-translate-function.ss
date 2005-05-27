(module honu-translate-function mzscheme
  (require (lib "plt-match.ss"))
  
  (require "../../ast.ss")
  (require "honu-translate-utils.ss")
  (require "honu-translate-expression.ss")
  
  (provide honu-translate-function)
  (define (honu-translate-function pgm defn)
    (match defn
      [(struct honu-function (stx name _ arg-names _ body))
       (at stx `(define ,(cons (at-ctxt name) (map at-ctxt arg-names))
                  ,(honu-translate-expression pgm defn body)))]))
  )
