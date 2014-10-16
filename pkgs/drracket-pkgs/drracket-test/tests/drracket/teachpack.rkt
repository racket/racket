#lang racket/base

(require "private/drracket-test-util.rkt"
         racket/class
         racket/path
         racket/gui/base
         framework
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
         (let loop ([tp-exps tp-exps]
                    [n 0])
           (cond
             [(null? tp-exps) null]
             [else
              (let ([tp-name 
                     (normal-case-path
                      (normalize-path
                       (collection-file-path
                        (string-append
                         (format good-teachpack-name n)
                         ".ss")
                        "tests" "drracket")))])
                (call-with-output-file tp-name
                  (lambda (port) (write (car tp-exps) port))
                  'truncate)
                (use-get/put-dialog
                 (lambda ()
                   (fw:test:menu-select "Language" "Add Teachpack..."))
                 tp-name)
                (cons tp-name (loop (cdr tp-exps) (+ n 1))))]))])
    
    (do-execute drs-frame)
    
    (let ([got (fetch-output drs-frame)]
          [full-expectation 
           (string-append
            (apply string-append (map (lambda (x) (format "Teachpack: ~a.\n" x)) tp-names))
            expected
            "\nThis psorgram should be tested.")])
      (unless (equal? got 
                      full-expectation)
        (printf 
         "FAILED:       tp: ~s\n             exp: ~s\n        expected: ~s\n             got: ~s\n"
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
             (printf "FAILED:       tp: ~s\n        expected: ~s\n             got: ~s\n"
                     tp-exp expected-error got))
           (fw:test:button-push "Ok")
           (wait-for-new-frame dialog))]
        [else
         (printf "FAILED: no error message appeared\n              tp: ~s\n        expected: ~s\n"
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
                              (test:get-active-top-level-window)
                              (and (send interactions-text get-user-eventspace)
                                   (parameterize ([current-eventspace
                                                   (send interactions-text get-user-eventspace)])
                                     (test:get-active-top-level-window))))])
                        (if (and active (not (eq? active drs-frame)))
                            active
                            #f)))])
               (poll-until wait-for-error-pred)))])
      (cond
        [dialog
         (let ([got (send dialog get-message)]
               [expected-error
                (format "Invalid Teachpack: ~a\n~a" tp-name expected)])
           (unless (string=? got expected-error)
             (printf "FAILED:       tp: ~s\n        expected: ~s\n             got: ~s\n"
                     tp-exp expected-error got))
           (fw:test:button-push "Ok")
           (wait-for-new-frame dialog))]
        [else
         (printf "FAILED: no error message appeared\n              tp: ~s\n        expected: ~s\n"
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
  (set-language-level! '("Beginning Student"))
  (do-execute drs-frame)
  (generic-tests))

(define (bad-tests)
  (set-language-level! '("Beginning Student"))
  
  (test-bad/execute-teachpack
   "undefined-id"
   "reference to undefined identifier: undefined-id")
  
  (test-bad/execute-teachpack
   `(module teachpack-tmp mzscheme (car))
   "car: expects argument of type <pair>; given: 1"))

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
  (define (test-teachpack dir teachpack)
    (when (or (equal? #"ss" (filename-extension teachpack))
              (equal? #"scm" (filename-extension teachpack))
              (equal? #"rkt" (filename-extension teachpack)))
      (unless (or (equal? "graphing.ss" (path->string teachpack))
                  (regexp-match #rx"^info[.][^.]*$" (path->string teachpack)))
        (printf "  testing ~a\n" teachpack)
        (fw:test:menu-select "Language" "Clear All Teachpacks")
        (fw:test:menu-select "Language" "Add Teachpack...")
        (wait-for-new-frame drs-frame)
        (define tp-dialog (test:get-active-top-level-window))
        (define choice 
          (let-values ([(_1 parent-dir _2) (split-path dir)])
            (find/select-relevant-choice tp-dialog 
                                         (path->string parent-dir)
                                         (path->string teachpack))))
        (fw:test:button-push "OK")
        (wait-for-new-frame tp-dialog)
        (do-execute drs-frame)
        
        (define got (fetch-output drs-frame))
        (define expected (format "Teachpack: ~a.\n1" (path->string teachpack)))
        (unless (equal? got expected)
          (printf "FAILED built in teachpack test: ~a\n" (path->string teachpack))
          (printf "       got: ~s\n  expected: ~s\n" got expected)))))
  (define (test-teachpacks an-image-tp)
    (define-values (dir name dir?) (split-path an-image-tp))
    (for ([file (in-list (directory-list dir))])
      (test-teachpack dir file)))
  (set-language-level! '("Advanced Student"))
  (do-execute drs-frame)
  (test-teachpacks (collection-file-path "image.rkt" "teachpack" "2htdp"))
  (test-teachpacks (collection-file-path "image.rkt" "teachpack" "htdp")))

(define (find/select-relevant-choice tp-dialog parent-dir tp-string)
  (define-values (lb index)
    (let loop ([p tp-dialog])
      (cond
        [(and (is-a? p list-box%)
              (list-control-get-index p parent-dir tp-string))
         =>
         (λ (i)
           (values p i))]
        [(is-a? p area-container<%>)
         (let c-loop ([children (send p get-children)])
           (cond
             [(null? children) (values #f #f)]
             [else (define-values (tb index) (loop (car children)))
                   (if tb
                       (values tb index)
                       (c-loop (cdr children)))]))]
        [else (values #f #f)])))
  (cond
    [lb (fw:test:set-list-box! lb index)]
    [else
     (error 'find/select-relevant-choice "did not find ~s in any list-box%" tp-string)]))

(define (list-control-get-index control parent-dir tp-string)
  (let loop ([i 0])
    (cond
      [(< i (send control get-number))
       (if (or (equal? (send control get-string i)
                       tp-string)
               (equal? (send control get-string i)
                       (format "~a/~a" parent-dir tp-string)))
           i
           (loop (+ i 1)))]
      [else #f])))

(define (find-leftmost-choice frame)
  (let loop ([p frame])
    (cond
      [(is-a? p list-box%) p]
      [(is-a? p area-container<%>)
       (ormap loop (send p get-children))]
      [else #f])))

(define (run-test)
  (set! drs-frame (wait-for-drracket-frame))
  (set! interactions-text (send drs-frame get-interactions-text))
  ;(good-tests)
  ;(bad-tests)
  (test-built-in-teachpacks))

(fire-up-drracket-and-run-tests run-test)

(module+ test
  (module config info
    (define timeout 150)))
