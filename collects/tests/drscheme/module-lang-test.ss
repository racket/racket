(module module-lang-test mzscheme
  (require "drscheme-test-util.ss"
           mzlib/class
           mzlib/file
           mzlib/etc
           mred
           framework
           (prefix fw: framework))
  
  (provide run-test)
  
  (define-struct test (definitions   ;; string
                       interactions  ;; (union #f string)
                       result))      ;; string

  (define this-dir (collection-path "tests" "drscheme"))

  (define tests
    (list
     
     (make-test "" 
                #f
                (regexp "module-language: the definitions window must contain a module"))
     (make-test "1" 
                #f
                (regexp "module-language: only module expressions are allowed"))
     (make-test "(module m mzscheme) 1" 
                #f 
                (regexp "module-language: there can only be one expression in the definitions window"))
     (make-test "#lang mzscheme\n(define x 1)" "x" "1")
     (make-test "(module m mzscheme (provide x) (define x 1))" "x" "1")
     (make-test "(module m mzscheme (define x 1))" "x" "1")
     (make-test "(module m mzscheme (define x 1) (define y 1) (provide y))" "x" "1")
     (make-test "(module m mzscheme (define x 1) (define y 2) (provide y))" "y" "2")
     (make-test "(module m mzscheme (require (lib \"list.ss\")))" 
                "foldl" 
                (regexp "foldl"))
     
     (make-test "(module m mzscheme (require (rename (lib \"list.ss\") local-foldl foldl)))" 
                "local-foldl"
                (regexp "foldl>"))
     
     (make-test "(module m mzscheme (require (all-except (lib \"list.ss\") foldl)))" 
                "first"
                (regexp "first>"))
     (make-test "(module m mzscheme (require (all-except (lib \"list.ss\") foldl)))" 
                "foldl"
                ". . reference to an identifier before its definition: foldl")
     
     (make-test "(module m mzscheme (require (prefix mz: mzscheme)))" "mz:+" #rx"procedure:+")
     
     (make-test "(module n mzscheme (provide (all-from-except mzscheme +)))"
                "+"
                #rx"procedure:+")
     
     (make-test "(module m mzscheme (require (prefix x: (lib \"list.ss\")) (lib \"list.ss\")))" 
                "foldl"
                (regexp "foldl>"))
     (make-test "(module m mzscheme (require (prefix x: (lib \"list.ss\")) (lib \"list.ss\")))" 
                "x:foldl"
                (regexp "foldl>"))
     
     (make-test (format "~s"
                        `(module m (file ,(path->string (build-path this-dir "module-lang-test-tmp.ss")))
                           x))
                "x"
                "1")
     
     ;; + shouldn't be bound in the REPL because it isn't bound
     ;; in the module.
     (make-test (format "~s"
                        `(module m (file ,(path->string (build-path this-dir "module-lang-test-tmp.ss")))
                           x))
                "+"
                ". . reference to an identifier before its definition: +")
     
     (make-test (format "~s" '(module m mzscheme (provide lambda)))
                "(lambda (x) x)"
                #rx"<procedure")
     
     (make-test (format "~s" '(module m mzscheme (define-syntax (m x) (syntax 1)) (provide m)))
                "(m)"
                "1")
     (make-test (format "~s" '(module m mzscheme (define-syntax s (syntax 1)) (provide s)))
                "s"
                ". s: illegal use of syntax in: s")
     
     (make-test (format "~s" '(module m mzscheme (define-syntax (x stx) #'(define a 10)) x x))
                "a"
                ". . reference to an identifier before its definition: a")
     (make-test (format "~s" '(module m mzscheme (define-syntax (x stx) #'(define-syntax (a stx) #'10)) x x))
                "a"
                ". . reference to an identifier before its definition: a")
     (make-test (format "~s" '(module m mzscheme (define-syntax (x stx) #'(define a 10)) x x (define a 77)))
                "a"
                "77")
     (make-test (format "~s" '(module m mzscheme (define-syntax (x stx) #'(define-syntax (a stx) #'10)) x x (define a 78)))
                "a"
                "78")
     
     (make-test
      (format "~s" `(module m mzscheme
                      (require-for-syntax (file ,(path->string (build-path this-dir "module-lang-test-tmp2.ss"))))
                      (provide s)
                      (define-syntax (s stx) e)))
      (format "~s ~s" '(require m) 's)
      #rx"module-lang-test-tmp2.ss:1:[67][90]: compile: bad syntax; literal data is not allowed, because no #%datum syntax transformer is bound in: 1$")
    
     (make-test (format "~s"
                        '(module tmp mzscheme
                           (provide (rename app #%app)
                                    (rename -current-namespace current-namespace)
                                    (rename -module->namespace module->namespace))
                           (define x 2)
                           (define -current-namespace error)
                           (define -module->namespace error)
                           (define-syntax app
                             (syntax-rules ()
                               ((app . x) '(app . x))))))
                "x"
                "2")
     
     (make-test
      "#lang scheme\n(eval 'cons)"
      #f
      ". compile: bad syntax; reference to top-level identifier is not allowed, because no #%top syntax transformer is bound in: cons")
     
     (make-test
      (format "~s" `(module m (file ,(path->string (build-path this-dir "module-lang-test-tmp.ss"))) 1 2 3))
      "1" ;; just make sure no errors.
      "1")))
  
  ;; set up for tests that need external files
  (call-with-output-file (build-path this-dir "module-lang-test-tmp.ss")
    (lambda (port)
      (write `(module module-lang-test-tmp mzscheme
                (provide (all-from-except mzscheme +)
                         x)
                (define x 1))
             port))
    'truncate
    'text)
  
  (call-with-output-file (build-path this-dir "module-lang-test-tmp2.ss")
    (lambda (port)
      (write `(module module-lang-test-tmp2 mzscheme
                (provide e)
                (define e #'1))
             port))
    'truncate
    'text)
  
  (call-with-output-file (build-path this-dir "module-lang-test-tmp3.ss")
    (lambda (port)
      (write `(module module-lang-test-tmp3 mzscheme
                (define-syntax (bug-datum stx)
                  (syntax-case stx ()
                    [(dat . thing)
                     (number? (syntax-e (syntax thing)))
                     (syntax/loc stx (#%datum . thing))]))
                
                (provide #%module-begin [rename bug-datum #%datum]))
             
             port))
    'truncate
    'text)

  (define drs (wait-for-drscheme-frame))
  (define interactions-text (send drs get-interactions-text))
  
  (define (single-test test)
    (let/ec k
      (clear-definitions drs)
      (type-in-definitions drs (test-definitions test))
      (do-execute drs)
        
      (let ([ints (test-interactions test)])
        
        (when ints
          (let ([after-execute-output
                 (send interactions-text
                       get-text
                       (send interactions-text paragraph-start-position 2)
                       (send interactions-text paragraph-end-position 2))])
            (unless (string=? "> " after-execute-output)
              (printf "FAILED: ~a\n        ~a\n        expected no output after execution, got: ~s\n"
                      (test-definitions test)
                      (or (test-interactions test) 'no-interactions)
                      after-execute-output)
              (k (void)))
            (type-in-interactions drs ints)
            (fw:test:keystroke #\return)
            (wait-for-computation drs)))
        
        (let* ([para-to-check (- (send interactions-text position-paragraph
                                       (send interactions-text last-position))
                                 1)]
               [after-int-start
                (send interactions-text paragraph-start-position para-to-check)]
               [after-int-end
                (send interactions-text paragraph-end-position para-to-check)]
               [after-int-output (send interactions-text
                                       get-text
                                       after-int-start
                                       after-int-end)]
               [passed?
                (cond
                  [(string? (test-result test))
                   (string=? after-int-output (test-result test))]
                  [(regexp? (test-result test))
                   (regexp-match (test-result test) after-int-output)])])
          (unless passed?
            (printf "FAILED: ~a\n        ~a\n  expected: ~s\n       got: ~s\n"
                    (test-definitions test)
                    (or (test-interactions test) 'no-interactions)
                    (test-result test)
                    after-int-output))))))
  
  (define (run-test)
    (set-language-level! '("Module") #t)
    (for-each single-test tests)))
