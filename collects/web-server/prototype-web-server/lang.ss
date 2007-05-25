(module lang mzscheme
  (require-for-syntax (lib "etc.ss")
                      (lib "list.ss")
                      "labels.ss"
                      "lang/util.ss"
                      "lang/elim-letrec.ss"
                      "lang/anormal.ss"
                      "lang/elim-callcc.ss"
                      "lang/defun.ss")
  (require "abort-resume.ss"
           (only "persistent-web-interaction.ss"
                 send/suspend/hidden
                 send/suspend/url
                 send/suspend/dispatch
                 extract-proc/url embed-proc/url
                 redirect/get
                 start-servlet))
  (provide (rename lang-module-begin #%module-begin))
  (provide (all-from "abort-resume.ss")
           (all-from-except mzscheme #%module-begin)
           send/suspend/hidden
           send/suspend/url
           send/suspend/dispatch
           extract-proc/url embed-proc/url
           redirect/get
           start-servlet)
  
  (define-syntax lang-module-begin 
    (make-lang-module-begin 
     make-labeling
     (make-module-case/new-defs
      (make-define-case/new-defs
       (compose #;(lambda (stx) (values stx empty))
                defun
                elim-callcc
                (make-anormal-term elim-letrec-term)))))))