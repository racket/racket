#lang racket/base
(require "test-suite-utils.rkt")

  (load-framework-automatically #f)

  (define (test/load file exp)
    (test
     (string->symbol file)
     void?
     `(let ([mred-name ((current-module-name-resolver) 'mred #f #f)]
            [orig-namespace (current-namespace)])
        (parameterize ([current-namespace (make-base-namespace)])
          (namespace-attach-module orig-namespace mred-name)
          (eval '(require (lib ,file "framework")))
          (with-handlers ([(lambda (x) #t)
                           (lambda (x)
                             (if (exn? x)
                                 (exn-message x)
                                 (format "~s" x)))])
            (eval ',exp)
            (void))))))

 
  (test/load "gui-utils.rkt" 'gui-utils:next-untitled-name)
  (test/load "test.rkt" 'test:run-interval)
  (test/load "splash.rkt" 'start-splash)
  (test/load "framework-sig.rkt" '(begin (eval '(require mzlib/unit))
                                        (eval '(define-signature dummy-signature^ ((open framework^))))))
  (test/load "framework-unit.rkt" 'framework@)
  (test/load "framework.rkt" '(list test:button-push
                                    gui-utils:next-untitled-name
                                    frame:basic-mixin))
  
  ;; ensures that all of the names in the signature are provided
  ;; by (require framework)
  (test/load
   "framework.rkt"
   ;; these extra evals let me submit multiple, independent top-level
   ;; expressions in the newly created namespace.
   '(begin (eval '(require mzlib/unit))
           (eval '(require (for-syntax scheme/base)))
           (eval '(require (for-syntax scheme/unit-exptime)))
           (eval '(define-syntax (signature->symbols stx)
                    (syntax-case stx ()
                      [(_ sig)
                       (let-values ([(_1 eles _2 _3) (signature-members #'sig #'whatever)])
                         (with-syntax ([eles eles])
                           #''eles))])))
           (eval '(require framework/framework-sig))
           (eval '(for-each eval (signature->symbols framework^)))))
