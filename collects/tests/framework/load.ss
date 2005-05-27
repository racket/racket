(module load mzscheme
  (require "test-suite-utils.ss")

  (load-framework-automatically #f)

  (define (test/load file exp)
    (test
     (string->symbol file)
     void?
     `(let ([mred-name 
             ((current-module-name-resolver) '(lib "mred.ss" "mred") #f #f)]
            [orig-namespace (current-namespace)])
        (parameterize ([current-namespace (make-namespace)])
          (namespace-attach-module
           orig-namespace           mred-name)
          (eval '(require (lib ,file "framework")))
          (with-handlers ([(lambda (x) #t)
                           (lambda (x)
                             (if (exn? x)
                                 (exn-message x)
                                 (format "~s" x)))])
            (eval ',exp)
            (void))))))

 
  (test/load "gui-utils.ss" 'gui-utils:next-untitled-name)
  (test/load "test.ss" 'test:run-interval)
  (test/load "splash.ss" 'start-splash)
  (test/load "framework-sig.ss" '(begin (eval '(require (lib "unitsig.ss")))
                                        (eval '(define-signature dummy-signature^ framework^))))
  (test/load "framework-unit.ss" 'framework@)
  (test/load "framework.ss" '(list test:button-push
				   gui-utils:next-untitled-name
				   frame:basic-mixin))
  
  ;; ensures that all of the names in the signature are provided
  ;; by (require (lib "framework.ss" "framework"))
  (test/load 
   "framework.ss"
   ;; these extra evals let me submit multiple, independent top-level
   ;; expressions in the newly created namespace.
   '(begin (eval '(require (lib "unitsig.ss")))
           (eval '(require (lib "framework-sig.ss" "framework")))
           (eval '(letrec ([prepend-symbol
                            (lambda (s1)
                              (lambda (s2)
                                (string->symbol
                                 (string-append
                                  (symbol->string s1)
                                  ":"
                                  (symbol->string s2)))))]
                           ;; exp-sig = (union (vectorof exp-sig)
                           ;;                  (cons sym exp-sig) 
                           ;;                  symbol)
                           [flatten ;; : exp-sig -> (listof symbol)
                            (lambda (l)
                              (cond
                                [(vector? l) 
                                 (apply append (map flatten (vector->list l)))]
                                [(pair? l)
                                 (map (prepend-symbol (car l)) (flatten (cdr l)))]
                                [(symbol? l) (list l)]
                                [else (error 'flatten "unk: ~e" l)]))]
                        [names (flatten (signature->symbols framework^))])
                    (for-each eval names))))))
