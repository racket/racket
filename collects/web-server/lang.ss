(module lang mzscheme
  (require-for-syntax (lib "etc.ss")
                      (lib "list.ss")
                      "lang/labels.ss"
                      "lang/util.ss"
                      "lang/elim-letrec.ss"
                      "lang/anormal.ss"
                      "lang/elim-callcc.ss"
                      "lang/defun.ss")
  (require "lang/lang-api.ss")
  (provide (rename lang-module-begin #%module-begin))
  (provide (all-from "lang/lang-api.ss"))
  
  (define-syntax lang-module-begin 
    (make-lang-module-begin 
     make-labeling
     (make-module-case/new-defs
      (make-define-case/new-defs
       (compose #;(lambda (stx) (values stx empty))
                defun
                elim-callcc
                (make-anormal-term elim-letrec-term)))))))