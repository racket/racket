(module honu-translate-class mzscheme

  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss"))
  
  (require "../../ast.ss")
  (require "../../tenv.ss")
  (require "honu-translate-utils.ss")
  (require "honu-translate-class-utils.ss")
  (require "honu-translate-expression.ss")
  
  (provide honu-translate-class)
  (define (honu-translate-class pgm cls)
    (match cls
      [(struct honu-class (stx name type final? init-names init-types impls defns exports))
       (at stx `(define ,(honu-translate-class-name name)
                  (parameterize ([current-inspector (make-inspector (current-inspector))])
                    (define ,(honu-translate-class-name name)
                      (class* object% ,(filter-map honu-translate-type-name impls)
                        ,@(honu-translate-init-slots (honu-class-init-names cls))
                        ,@(honu-translate-slotdefns pgm cls (honu-class-defns cls))
                        ,@(honu-translate-exports pgm cls '() (honu-class-exports cls))
                        (super-new)))
                    ,(honu-translate-class-name name))))]))
  )
