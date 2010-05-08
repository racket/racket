#|

The EoPL language can almost be specified via info.ss fields, but
on-execute needs to install the EoPL exception handler as its 
last action. (The module body can't do that, because a `with-handlers'
wraps the load of the module.)

|#

#lang mzscheme

(require mzlib/unit
         mzlib/class
         drscheme/tool
         string-constants)

(provide tool@)

(define tool@
  (unit
   (import drscheme:tool^)
   (export drscheme:tool-exports^)
   (define language-base%
     (class* object% (drscheme:language:simple-module-based-language<%>)
       (define/public (get-language-numbers)
         '(-500 -400))
       (define/public (get-language-position)
         (list (string-constant teaching-languages)
               "Essentials of Programming Languages (3rd ed.)"))
       (define/public (get-module)
         '(lib "eopl/eopl.ss"))
       (define/public (get-one-line-summary)
         "Based on the Friedman, Wand, and Haynes text")
       (define/public (get-language-url)
         "http://www.eopl3.com/")
       (define/public (get-reader)
         (lambda (src port)
           (let ([v (read-syntax src port)])
             (if (eof-object? v)
               v
               (namespace-syntax-introduce v)))))
       (super-instantiate ())))

   (define language%
     (class (drscheme:language:module-based-language->language-mixin
             (drscheme:language:simple-module-based-language->module-based-language-mixin
              language-base%))
       (define/override (use-namespace-require/copy?) #t)
       (define/override (on-execute settings run-in-user-thread)
         (super on-execute settings run-in-user-thread)
         (print-mpair-curly-braces #f)
         (run-in-user-thread
          (lambda ()
            (print-as-expression #f)
            ((namespace-variable-value 'install-eopl-exception-handler)))))
       (super-instantiate ())))

   (define (phase1) (void))
   (define (phase2)
     (drscheme:language-configuration:add-language 
      (make-object ((drscheme:language:get-default-mixin) language%))))))
