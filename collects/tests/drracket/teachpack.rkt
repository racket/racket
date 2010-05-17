#lang scheme/base

(require "drracket-test-util.rkt"
         scheme/class
         scheme/path
         scheme/gui/base
         (prefix-in fw: framework))

(provide run-test)

(define drs-frame 'not-yet-drs-frame)
(define interactions-text 'not-yet-interactions-text)

(define good-teachpack-name "teachpack-tmp~a")

(define (test-good-teachpack tp-exps dr-exp expected)
  (clear-definitions drs-frame)
  (type-in-definitions drs-frame dr-exp)
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  
  (let ([tp-names
         (let ([teachpack-path (normal-case-path
                                (normalize-path
                                 (collection-path "tests" "drracket")))])
           (let loop ([tp-exps tp-exps]
                      [n 0])
             (cond
               [(null? tp-exps) null]
               [else
                (let ([tp-name (build-path teachpack-path 
                                           (string-append
                                            (format good-teachpack-name n)
                                            ".ss"))])
                  (call-with-output-file tp-name
                    (lambda (port) (write (car tp-exps) port))
                    'truncate)
                  (use-get/put-dialog
                   (lambda ()
                     (fw:test:menu-select "Language" "Add Teachpack..."))
                   tp-name)
                  (cons tp-name (loop (cdr tp-exps) (+ n 1))))])))])
    
    (do-execute drs-frame)
    
    (let ([got (fetch-output drs-frame)]
          [full-expectation 
           (string-append
            (apply string-append (map (lambda (x) (format "Teachpack: ~a.~n" x)) tp-names))
            expected
            "\nThis psorgram should be tested.")])
      (unless (equal? got 
                      full-expectation)
        (printf 
         "FAILED:       tp: ~s~n             exp: ~s~n        expected: ~s~n             got: ~s~n"
         tp-exps
         dr-exp
         full-expectation
         got)))))

;; there are no more errors when the teachpack is loaded (for now...)
(define (test-bad/load-teachpack tp-exp expected-error)
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  (let ([tp-name (normal-case-path
                  (normalize-path
                   (build-path
                    (collection-path "tests" "drracket")
                    "teachpack-tmp.ss")))])
    (call-with-output-file tp-name
      (lambda (port) (display tp-exp port))
      'truncate)
    (use-get/put-dialog
     (lambda ()
       (fw:test:menu-select "Language" "Add Teachpack..."))
     tp-name)
    (let ([dialog
           (with-handlers ([(lambda (x) #t)
                            (lambda (x) #f)])
             (wait-for-new-frame drs-frame))])
      (cond
        [dialog
         (let ([got (send dialog get-message)])
           (unless (string=? got expected-error)
             (printf "FAILED:       tp: ~s~n        expected: ~s~n             got: ~s~n"
                     tp-exp expected-error got))
           (fw:test:button-push "Ok")
           (wait-for-new-frame dialog))]
        [else
         (printf "FAILED: no error message appeared~n              tp: ~s~n        expected: ~s~n"
                 tp-exp expected-error)]))))

(define (test-bad/execute-teachpack tp-exp expected)
  (fw:test:menu-select "Language" "Clear All Teachpacks")
  (let ([tp-name (normal-case-path
                  (normalize-path
                   (build-path
                    (collection-path "tests" "drracket")
                    "teachpack-tmp.ss")))])
    (call-with-output-file tp-name
      (lambda (port) (display tp-exp port))
      'truncate)
    (use-get/put-dialog
     (lambda ()
       (fw:test:menu-select "Language" "Add Teachpack..."))
     tp-name)
    (do-execute drs-frame #f)
    (let ([dialog
           (with-handlers ([exn:fail? (lambda (x) #f)])
             (let ([wait-for-error-pred
                    (lambda ()
                      (let ([active
                             (or
                              (get-top-level-focus-window)
                              (and (send interactions-text get-user-eventspace)
                                   (parameterize ([current-eventspace
                                                   (send interactions-text get-user-eventspace)])
                                     (get-top-level-focus-window))))])
                        (if (and active (not (eq? active drs-frame)))
                            active
                            #f)))])
               (poll-until wait-for-error-pred)))])
      (cond
        [dialog
         (let ([got (send dialog get-message)]
               [expected-error
                (string-append (format "Invalid Teachpack: ~a~n" tp-name)
                               expected)])
           (unless (string=? got expected-error)
             (printf "FAILED:       tp: ~s~n        expected: ~s~n             got: ~s~n"
                     tp-exp expected-error got))
           (fw:test:button-push "Ok")
           (wait-for-new-frame dialog))]
        [else
         (printf "FAILED: no error message appeared~n              tp: ~s~n        expected: ~s~n"
                 tp-exp error)]))))

(define (generic-tests)
  (test-good-teachpack
   (list
    `(module ,(string->symbol (format good-teachpack-name 0)) mzscheme))
   "1"
   "1")
  
  (test-good-teachpack
   (list `(module ,(string->symbol (format good-teachpack-name 0)) mzscheme
            (provide not-a-primitive)
            (define not-a-primitive 1)))
   "not-a-primitive"
   "1")
  
  (test-good-teachpack
   (list `(module ,(string->symbol (format good-teachpack-name 0)) mzscheme
            (provide not-a-primitive1)
            (define not-a-primitive1 1))
         `(module ,(string->symbol (format good-teachpack-name 1)) mzscheme
            (provide not-a-primitive2)
            (define not-a-primitive2 2)))
   "(+ not-a-primitive1 not-a-primitive2)"
   "3"))

(define (good-tests)
  (set-language-level! '("How to Design Programs" "Beginning Student"))
  (do-execute drs-frame)
  (generic-tests))

(define (bad-tests)
  (set-language-level! '("How to Design Programs" "Beginning Student"))
  
  (test-bad/execute-teachpack
   "undefined-id"
   "reference to undefined identifier: undefined-id")
  
  (test-bad/execute-teachpack
   `(module teachpack-tmp mzscheme (car))
   "car: expects argument of type <pair>; given 1"))

(define (get-string-from-file fn)
  (call-with-input-file fn
    (lambda (port)
      (apply string-append
             (let loop ()
               (let ([l (read-line port)])
                 (if (eof-object? l)
                     null
                     (list* l " " (loop)))))))
    'text))

;; doesn't test graphing.ss teachpack
(define (test-built-in-teachpacks)
  (clear-definitions drs-frame)
  (type-in-definitions drs-frame "1")
  (let* ([test-teachpack
          (lambda (dir)
            (lambda (teachpack)
              (when (or (equal? #"ss" (filename-extension teachpack))
                        (equal? #"scm" (filename-extension teachpack)))
                (unless (equal? "graphing.ss" (path->string teachpack))
                  (printf "  testing ~a~n" teachpack)
                  (fw:test:menu-select "Language" "Clear All Teachpacks")
                  (fw:test:menu-select "Language" "Add Teachpack...")
                  (wait-for-new-frame drs-frame)
                  (let* ([tp-dialog (get-top-level-focus-window)]
                         [choice (find-leftmost-choice tp-dialog)])
                    (fw:test:set-list-box! choice (path->string teachpack))
                    (fw:test:button-push "OK")
                    (wait-for-new-frame tp-dialog))
                  (do-execute drs-frame)
                  
                  (let ([got (fetch-output drs-frame)]
                        [expected (format "Teachpack: ~a.\n1"
                                          (path->string teachpack))])
                    (unless (equal? got expected)
                      (printf "FAILED built in teachpack test: ~a~n" (path->string teachpack))
                      (printf "       got: ~s~n  expected: ~s~n" got expected)))))))]
         [test-teachpacks
          (lambda (paths)
            (for-each (lambda (dir)
                        (for-each (test-teachpack dir) 
                                  (directory-list dir)))
                      paths))]
         [teachpack-dir (normalize-path (collection-path "teachpack"))])
    (set-language-level! '("How to Design Programs" "Advanced Student"))
    (do-execute drs-frame)
    (test-teachpacks (list (build-path teachpack-dir "2htdp")
                           (build-path teachpack-dir "htdp")))))

(define (find-leftmost-choice frame)
  (let loop ([p frame])
    (cond
      [(is-a? p list-box%) p]
      [(is-a? p area-container<%>)
       (ormap loop (send p get-children))]
      [else #f])))

(define (run-test)
  (set! drs-frame (wait-for-drscheme-frame))
  (set! interactions-text (send drs-frame get-interactions-text))
  ;(good-tests)
  ;(bad-tests)
  (test-built-in-teachpacks))

(fire-up-drscheme-and-run-tests run-test)
