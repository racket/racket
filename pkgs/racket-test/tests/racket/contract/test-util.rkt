#lang racket/base
(require (for-syntax racket/base))

(provide test/no-error
         test/spec-passed
         test/spec-passed/result
         test/spec-failed
         test/pos-blame
         test/neg-blame
         test/well-formed
         test ctest ctest/rewrite
         test-true test-false
         
         current-contract-namespace
         make-basic-contract-namespace
         make-full-contract-namespace
         full-contract-namespace-initial-set
         
         contract-syntax-error-test
         contract-error-test
         
         contract-eval
         contract-compile
         contract-expand
         
         rewrite-to-add-opt/c
         rewrite-to-multi-wrap
         do-not-double-wrap
         contract-rewrite-tests-to-skip
         
         test-cases failures)

(define test-cases 0)
(define failures 0)

(define contract-rewrite-tests-to-skip (make-parameter '()))

(provide new-test-case new-failure)
(define (new-test-case name)
  ;(printf "running test ~a\n" name)
  (set! test-cases (+ test-cases 1)))
(define (new-failure) (set! failures (+ failures 1)))

(define (test #:test-case-name [name #f] expected fun arg1 . args)
  (new-test-case name)
  ;(printf "\n\nexpected ~s\n" arg1)
  (define result
    (cond
      [(procedure? fun) (apply fun arg1 args)]
      [else arg1]))
  (unless (equal? result expected)
    (eprintf "FAILED ~a\n       got ~s\n  expected ~s\n"
            (cond
              [name name]
              [(procedure? fun) (format "~s" `(,fun ,arg1 ,@args))]
              [else (format "~s" fun)])
            result
            expected)
    (new-failure)))

(define (test-an-error name thunk sexp predicate?)
  (new-test-case name)
  (define exn
    (with-handlers ((exn:fail? values))
      (thunk)
      'no-exn-raised))
  (unless (predicate? exn)
    (eprintf "FAILED ~s\n" name)
    (define msg (if (exn? exn)
                    (apply 
                     string-append
                     (exn-message exn)
                     (for/list ([x (in-list (continuation-mark-set->context 
                                             (exn-continuation-marks exn)))])
                       (format "\n  ~s" x)))
                    (format "~s" exn)))
    (display "  " (current-error-port))
    (display (regexp-replace* #rx"\n" msg "\n  ") (current-error-port))
    (newline (current-error-port))
    (new-failure)))

(define current-contract-namespace (make-parameter 'current-contract-namespace-not-initialized))

(define namespace-to-copy-from (make-base-namespace))

(define (make-basic-contract-namespace . addons)
  (define n (make-base-namespace))
  
  ;; set up just a single namespace to copy all of the 
  ;; library modules from; the top-level of the namespace
  ;; gets clobbered, but we can avoid reloading the contract
  ;; system code over and over (which is useful when there
  ;; are no .zo files compiled)
  
  (define modules-to-add (list* 'racket/contract/base
                                'racket/contract/private/blame
                                addons))
  (parameterize ([current-namespace namespace-to-copy-from])
    (for ([module-to-add (in-list modules-to-add)])
      (namespace-require module-to-add)))
  
  (for ([module-to-add (in-list modules-to-add)])
    (namespace-attach-module namespace-to-copy-from module-to-add n))
  
  (parameterize ([current-namespace n])
    (namespace-require 'racket/contract/base)
    (namespace-require '(only racket/contract/private/blame exn:fail:contract:blame?))
    (namespace-require '(only racket/contract/private/collapsible-common COLLAPSIBLE-LIMIT))
    (for ([addon (in-list addons)])
      (namespace-require addon)))
  n)

(define (make-full-contract-namespace . addons)
  (apply make-basic-contract-namespace 
         (append full-contract-namespace-initial-set addons)))
(define full-contract-namespace-initial-set
  '(racket/contract racket/class racket/set))
         

(define (contract-eval x #:test-case-name [test-case #f])
  (with-handlers ((exn:fail? (λ (x)
                               (when test-case
                                 (eprintf "exception raised while running test case ~a\n"
                                          test-case)
                                 (new-failure))
                               (raise x))))
  (parameterize ([current-namespace (current-contract-namespace)])
    (eval x))))

(define (contract-compile x)
  (parameterize ([current-namespace (current-contract-namespace)])
    (compile x)))

(define (contract-expand x)
  (parameterize ([current-namespace (current-contract-namespace)])
    (expand x)))

(define-syntax (ctest stx)
  (syntax-case stx ()
    [(_ a ...)
     (with-syntax ([line-name (format "~a:~a" 
                                      (let ([p (syntax-source stx)])
                                        (if (path? p)
                                            (let-values ([(base name dir)
                                                          (split-path p)])
                                              name)
                                            (format "~s" p)))
                                      (syntax-line stx))])
       (syntax (contract-eval #:test-case-name line-name
                              `(,test #:test-case-name line-name a ...))))]))

(define-syntax (ctest-no-error stx)
  (syntax-case stx ()
    [(_ name e)
     (syntax
      (ctest
       #t
       name
       (with-handlers ([exn:fail? (lambda (x) `(exn ,(exn-message x)))])
         e
         #t)))]))

(define (contract-error-test name exp exn-ok?)
  (test #t
        name
        (contract-eval #:test-case-name name
                       `(with-handlers ((exn:fail? (λ (x) (and (,exn-ok? x) #t))))
                          ,exp
                          "NO EXN RAISED"))))

(define (contract-syntax-error-test name exp [reg #rx""])
  (test #t
        name
        (contract-eval #:test-case-name name
                       `(with-handlers ((exn:fail:syntax?
                                         (lambda (x) (regexp-match? ,reg (exn-message x)))))
                          (eval ',exp)))))

;; test/spec-passed : symbol sexp -> void
;; tests a passing specification
(define (test/spec-passed name expression)
  (parameterize ([compile-enforce-module-constants #f])
    (contract-eval
     #:test-case-name name
     `(,test
       #:test-case-name ',name
       'no-exn-raised
       eval
       '(with-handlers ([exn:fail? (λ (x) (cons (exn-message x)
                                                (continuation-mark-set->context
                                                 (exn-continuation-marks x))))])
          ,expression
          'no-exn-raised)))
    (let ([new-expression (rewrite-out expression)])
      (when new-expression
        (contract-eval
         #:test-case-name (format "~a rewrite-out" name)
         `(,test
           #:test-case-name ,(format "~a rewrite-out" name)
           'no-exn-raised
           eval
           '(with-handlers ([exn:fail? exn-message])
              ,new-expression
              'no-exn-raised))))))

  (define (rewrite-test wrapper wrapper-name)
    (let/ec k
      (contract-eval
       #:test-case-name (format "~a ~a" name wrapper-name)
       `(,test #:test-case-name ,(format "~a ~a" name wrapper-name)
               'no-exn-raised
               eval
               '(with-handlers ([exn:fail? exn-message])
                  ,(wrapper expression k)
                  'no-exn-raised)))))
  (rewrite-test rewrite-to-add-opt/c   "rewrite-to-add-opt/c")
  (rewrite-test rewrite-to-multi-wrap "rewrite-to-double-wrap"))

(define (test/spec-passed/result name expression result [double-wrapped-result result])
  (parameterize ([compile-enforce-module-constants #f])
    (contract-eval #:test-case-name name `(,test #:test-case-name ',name ',result eval ',expression))
    (define (rewrite-test wrapper wrapper-name [result* result])
      (let/ec k
        (define rewrite-name (format "~a ~a" name wrapper-name))
        (contract-eval
         #:test-case-name rewrite-name
         `(,test
           #:test-case-name ,rewrite-name
           ',result*
           eval
           ',(wrapper expression k)))))
    (rewrite-test rewrite-to-add-opt/c   "rewrite-to-add-opt/c")
    (unless (eq? double-wrapped-result do-not-double-wrap)
      (rewrite-test rewrite-to-multi-wrap "rewrite-to-double-wrap" double-wrapped-result))

    (let ([new-expression (rewrite-out expression)])
      (when new-expression
        (define out-rewrite-name (format "~a rewrite-out" name))
        (contract-eval
         #:test-case-name out-rewrite-name
         `(,test
           #:test-case-name ,out-rewrite-name
           ',result
           eval
           ',new-expression))))))

;; convenient shortcuts
(define (test-true name expression)  (test/spec-passed/result name expression #t))
(define (test-false name expression) (test/spec-passed/result name expression #f))

;; rewrites `provide/contract' to use `contract-out'
(define (rewrite-out orig-exp)
  (define rewrote? #f)
  (define maybe-rewritten?
    (let loop ([exp orig-exp])
      (cond
        [(and (list? exp)
              (>= (length exp) 3)
              (eq? (car exp) 'module))
         (define modname (list-ref exp 1))
         (define lang (list-ref exp 2))
         (define bodies (list-tail exp 3))
         (define at-beginning '())
         (define at-end '())
         
         ;; remove (and save) the provide/contract & contract-out
         ;; declarations, switching their senses
         (define removed-bodies
           (apply
            append
            (for/list ([body (in-list bodies)])
              (cond
                [(and (pair? body)
                      (eq? (car body) 'provide/contract))
                 (define args (cdr body))
                 (set! rewrote? #t)
                 (set! at-beginning (cons `(provide (contract-out . ,args))
                                          at-beginning))
                 (list)]
                [(and (list? body)
                      (= 2 (length body))
                      (eq? (car body) 'provide)
                      (let ([sub-part (list-ref body 1)])
                        (and (pair? sub-part)
                             (eq? (car sub-part) 'contract-out))))
                 (define args (cdr (list-ref body 1)))
                 (set! rewrote? #t)
                 (set! at-end (cons `(provide/contract . ,args)
                                    at-end))
                 (list)]
                [else
                 (list body)]))))
         
         ;; insert the provide/contract (rewrite to contract-out) after the
         ;; first require that has 'contract' in it
         (define inserted-bodies
           (if (equal? lang 'racket)
               (append (reverse at-beginning)
                       removed-bodies)
               (apply
                append
                (for/list ([body (in-list removed-bodies)])
                  (define (good-thing? l)
                    (for/or ([x (in-list l)])
                      (and (symbol? x)
                           (regexp-match? #rx"contract" (symbol->string x)))))
                  (cond
                    [(and (pair? body)
                          (eq? (car body) 'require)
                          (good-thing? (cdr body)))
                     (cons body (reverse at-beginning))]
                    [else
                     (list body)])))))
         `(module ,modname ,lang 
            (void) ;; always insert this to work around bug in 'provide'
            ,@inserted-bodies ,@(reverse at-end))]
        [(list? exp)
         (map loop exp)]
        [else exp])))
  
  (and rewrote? maybe-rewritten?))

(define ((rewrite contract-case) exp k)
  (let loop ([exp exp])
    (cond
     [(null? exp) null]
     [(list? exp)
      (case (car exp)
        [(contract) (contract-case (cadr exp) (caddr exp) (cdddr exp) loop)]
        [(module) (k #f)]
        [else (map loop exp)])]
     [(pair? exp) (cons (loop (car exp))
                        (loop (cdr exp)))]
     [else exp])))

;; rewrites `contract' to use opt/c. If there is a module definition in there, we skip that test.
(define rewrite-to-add-opt/c
  (rewrite (lambda (ctc val parties loop)
             `(contract (opt/c ,(loop ctc)) ,(loop val) ,@(map loop parties)))))

;; rewrites `contract` to double-wrap. To test collapsible wrappers.
(define rewrite-to-multi-wrap
  (rewrite (lambda (ctc val parties loop)
             (define new-ctc (loop ctc))
             (define new-parties (map loop parties))
             `(let ([the-ctc ,new-ctc])
                (for/fold ([the-val ,(loop val)])
                          ([i (in-range (add1 COLLAPSIBLE-LIMIT))])
                  (contract the-ctc the-val ,@new-parties))))))
(define do-not-double-wrap (gensym)) ; recognized by some test forms

;; blame : (or/c 'pos 'neg string?)
;;   if blame is a string, expect to find the string (format "blaming: ~a" blame) in the exn message
(define (test/spec-failed name expression blame)
  (define (has-proper-blame? msg)
    (define reg
      (cond
        [(eq? blame 'pos) #rx"blaming: pos"]
        [(eq? blame 'neg) #rx"blaming: neg"]
        [(string? blame) (string-append "blaming: " (regexp-quote blame))]
        [else #f]))
    (and reg (regexp-match? reg msg)))
  (contract-eval
   #:test-case-name name
   `(,test-an-error
     ',name
     (lambda () ,expression)
     ',expression
     (lambda (exn)
       (and (exn:fail:contract:blame? exn)
            (,has-proper-blame? (exn-message exn))))))
  (define (rewrite-test wrapper wrapper-name short-wrapper-name)
    (unless (member short-wrapper-name (contract-rewrite-tests-to-skip))
      (let/ec k
        (let ([rewritten (wrapper expression k)])
          (contract-eval
           #:test-case-name (format "~a ~a" name wrapper-name)
           `(,test-an-error
             ',(string->symbol (format "~a+~a" name short-wrapper-name))
             (lambda () ,rewritten)
             ',rewritten
             (lambda (exn)
               (and (exn:fail:contract:blame? exn)
                    (,has-proper-blame? (exn-message exn))))))))))
  (rewrite-test rewrite-to-add-opt/c  "rewrite-to-add-opt/c"   "opt/c")
  (rewrite-test rewrite-to-multi-wrap "rewrite-to-double-wrap" "double"))

(define (test/pos-blame name expression) (test/spec-failed name expression 'pos))
(define (test/neg-blame name expression) (test/spec-failed name expression 'neg))

(define-syntax (ctest/rewrite stx)
  (syntax-case stx ()
    [(_ expected name expression)
     (with-syntax ([opt-name    (string->symbol (format "~a+opt/c" (syntax-e #'name)))]
                   [double-name (string->symbol (format "~a+double" (syntax-e #'name)))])
       #'(begin
           (contract-eval #:test-case-name 'name `(,test expected 'name expression))
           (define (rewrite-test wrapper name*)
             (let/ec k
               (contract-eval
                #:test-case-name name*
                `(,test expected 
                        ',name*
                        ,(wrapper 'expression k)))))
           (rewrite-test rewrite-to-add-opt/c   'opt-name)
           (rewrite-test rewrite-to-multi-wrap 'double-name)))]))

(define (test/well-formed stx)
  (contract-eval
   `(,test (void)
           (let ([expand/ret-void (lambda (x) (expand x) (void))]) expand/ret-void)
           ,stx)))

(define-syntax (test/no-error stx)
  (syntax-case stx ()
    [(_ arg)
     (let ()
       (define src (syntax-source stx))
       (define fn (if (path? src)
                      (let-values ([(base name dir) (split-path src)])
                        (format "~a" name))
                      (format "~s" src)))
       #`(test/no-error/proc #,fn #,(syntax-line stx) arg))]))

(define (test/no-error/proc fn line sexp)
  (contract-eval
   #:test-case-name (format "~a:~a" fn line)
   `(,test (void)
           eval
           '(begin ,sexp (void))))
  (define (rewrite-test wrapper wrapper-name)
      (let/ec k
    (define rewritten (wrapper sexp k))
    (unless (equal? rewritten sexp)
      (contract-eval
       #:test-case-name (format "~a:~a ~a" fn line wrapper-name)
       `(,test (void)
               eval
               '(begin ,rewritten (void)))))))
  (rewrite-test rewrite-to-add-opt/c   "opt/c")
  (rewrite-test rewrite-to-multi-wrap "double"))
