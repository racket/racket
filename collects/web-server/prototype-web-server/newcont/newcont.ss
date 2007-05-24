(module newcont mzscheme
  (require-for-syntax (lib "etc.ss")
                      (lib "labels.ss" "prototype-web-server")
                      "util.ss"
                      "elim-letrec.ss"
                      "anormal.ss"
                      "elim-callcc.ss"
                      "defun.ss")
  (require (lib "abort-resume.ss" "prototype-web-server"))
  (require (only (lib "persistent-web-interaction.ss" "prototype-web-server")
                 send/suspend/hidden
                 send/suspend/url
                 send/suspend/dispatch
                 extract-proc/url embed-proc/url
                 redirect/get
                 start-servlet))
  (provide (rename lang-module-begin #%module-begin))
  (provide (all-from (lib "abort-resume.ss" "prototype-web-server"))
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