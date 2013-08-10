#lang at-exp racket/base
(require "private/module-lang-test-utils.rkt"
         "private/drracket-test-util.rkt")
(provide run-test)

;; set up for tests that need external files
(write-test-modules
 (module module-lang-test-tmp1 mzscheme
   (provide (all-from-except mzscheme +)
            x)
   (define x 1))
 (module module-lang-test-tmp2 mzscheme
   (provide e)
   (define e #'1))
 (module module-lang-test-tmp3 mzscheme
   (define-syntax (bug-datum stx)
     (syntax-case stx ()
       [(dat . thing)
        (number? (syntax-e (syntax thing)))
        (syntax/loc stx (#%datum . thing))]))
   (provide #%module-begin [rename bug-datum #%datum]))
 (module module-lang-test-tmp4 racket/base
   (/ 888 2)
   (provide (except-out (all-from-out racket/base) #%top-interaction)))
 (module module-lang-test-syn-error racket/base
   (lambda)))

(test @t{}
      #f
      @rx{Module Language: There must be a valid module
          Try starting your program with
          Interactions disabled}
      #t)
(test @t{1}
      #f
      @rx{Module Language: only a module expression is allowed
          Interactions disabled}
      #t)
(test @t{(module m racket) 1}
      #f
      @rx{Module Language: there can only be one expression in the definitions
          Interactions disabled}
      #t)
(test @t{.}
      #f
      @rx{Module Language: invalid module text
          read: illegal
          Interactions disabled}
      #t)
(test @t{#lang mzscheme
         (define x 1)}
      @t{x}
      "1")
(test @t{#lang m-z-scheme
         (define x 1)}
      #f
      @rx{Module Language: invalid module text
          collection not found
          Interactions disabled}
      #t)
(test @t{#lang racket
         3}
      #f
      "3")
(test @t{(module m racket (provide x) (define x 1))}
      @t{x}
      "1")
(test @t{(module m racket (define x 1))}
      @t{x}
      "1")
(test @t{(module m racket (define x 1) (define y 1) (provide y))}
      @t{x}
      "1")
(test @t{(module m racket (define x 1) (define y 2) (provide y))}
      @t{y}
      "2")
(test @t{(module m mzscheme (require mzlib/list))}
      @t{foldl}
      #rx"foldl")
(test @t{(module m mzscheme (require (rename mzlib/list local-foldl foldl)))}
      @t{local-foldl}
      #rx"foldl>")
(test @t{(module m mzscheme (require (all-except mzlib/list foldl)))}
      @t{first}
      #rx"first>")
(test @t{(module m mzscheme (require (all-except mzlib/list foldl)))}
      @t{foldl}
      #rx"[.] [.] foldl:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme (require (prefix mz: mzscheme)))}
      @t{mz:+}
      #rx"procedure:[+]")
(test @t{(module n mzscheme (provide (all-from-except mzscheme +)))}
      @t{+}
      #rx"procedure:[+]")
(test @t{(module m mzscheme
           (require (prefix x: mzlib/list) mzlib/list))}
      @t{foldl}
      #rx"foldl>")
(test @t{(module m mzscheme
           (require (prefix x: mzlib/list) mzlib/list))}
      @t{x:foldl}
      #rx"foldl>")
(test @t{(module m (file @in-here{module-lang-test-tmp1.rkt}) x)}
      @t{x}
      "1")
;; + shouldn't be bound in the REPL because it isn't bound in the module.
(test @t{(module m (file @in-here{module-lang-test-tmp1.rkt}) x)}
      @t{+}
      #rx"[.] [.] [+]:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme (provide lambda))}
      @t{(lambda (x) x)}
      #rx"<procedure")
(test @t{(module m mzscheme (define-syntax (m x) (syntax 1)) (provide m))}
      @t{(m)}
      "1")
(test @t{(module m mzscheme (define-syntax s (syntax 1)) (provide s))}
      @t{s}
      ". s: illegal use of syntax in: s")
(test @t{(module m mzscheme (define-syntax (x stx) #'(define a 10)) x x)}
      @t{a}
      #rx"[.] [.] a:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme
           (define-syntax (x stx) #'(define-syntax (a stx) #'10))
           x
           x)}
      @t{a}
      #rx"[.] [.] a:.*cannot reference an identifier before its definition")
(test @t{(module m mzscheme
           (define-syntax (x stx) #'(define a 10))
           x
           x
           (define a 77))}
      @t{a}
      "77")
(test @t{(module m mzscheme
           (define-syntax (x stx) #'(define-syntax (a stx) #'10))
           x
           x
           (define a 78))}
      @t{a}
      "78")
(test @t{(module m mzscheme
           (require-for-syntax (file @in-here{module-lang-test-tmp2.rkt}))
           (provide s)
           (define-syntax (s stx) e))}
      @t{(require 'm) s}
      @rx{[?]:.*literal data is not allowed.*no #%datum syntax transformer is bound in: 1$})
(test @t{(module tmp mzscheme
           (provide (rename app #%app)
                    (rename -current-namespace current-namespace)
                    (rename -module->namespace module->namespace))
           (define x 2)
           (define -current-namespace error)
           (define -module->namespace error)
           (define-syntax app (syntax-rules () ((app . x) '(app . x)))))}
      @t{x}
      "2")
(test @t{#lang racket
         (eval 'cons)}
      #f
      @rx{cons: unbound identifier.*no #%top syntax transformer is bound})
(test @t{(module m (file @in-here{module-lang-test-tmp1.rkt}) 1 2 3)}
      @t{1} ;; just make sure no errors.
      "1")

(test @t{#lang racket}
      @t{(begin-for-syntax (+ 1 2))}
      @t{})

(test @t{#lang racket}
      @t{(begin (struct s (x)) (struct t s (y)) (s-x (t 1 2)))}
      "1")

;; check that we have a working repl in the right language after
;; syntax errors, unless it's a bad language
(test @t{#lang racket
         (define x 1)
         (define y (/ 0))}
      @t{(+ 122 x)}
      @rx{. /: division by zero
          123}
      #t)
(test @t{#lang racket
         (define x 1)
         (define y (/ 0))}
      @t{(if x 123)}
      @rx{/: division by zero.*if: missing an "else"}
      #t)
(test @t{#lang mzscheme
         (define x 1)
         (define y (/ 0))}
      @t{(if x 123)}
      @rx{. /: division by zero
          123}
      #t)
(test @t{(module xx scheme/list
           (define x 1)
           (define y (/ 0)))}
      #f
      @rx{no #%module-begin binding in the module's language
          Interactions disabled:
          does not support a REPL \(no #%top-interaction\)}
      #t)
(test @t{(module xx (file @in-here{module-lang-test-tmp4.rkt})
           (define x 1)
           (* x 123))}
      #f
      @rx{444
          123
          Interactions disabled:
          does not support a REPL \(no #%top-interaction\)
          }
      #t)
(test @t{(module xx (file @in-here{this-file-does-not-exist})
           (define x 1)
           (* x 123))}
      #f
      @rx{cannot open input file
          Module Language: invalid language specification
          Interactions disabled}
      #t)
(test @t{#lang info}
      #f
      ;; test the complete buffer, to make sure that there is no error
      ;; would be better if this said "lang" and not "setup/infotab" but
      ;; leave it like this for now.
      "\nInteractions disabled: setup/infotab does not support a REPL (no #%top-interaction)"
      #t)

;; test racket/load behavior
(test @t{#lang racket/load
         (module m mzscheme (provide x) (define x 2))
         (require 'm)
         (printf "~s\n" x)
         (flush-output)}
      #f
      "2")
(test @t{#lang racket/load
         (module m mzscheme (provide x) (define x 2))
         (module n racket/base (require 'm) (provide y) (define y (* x x)))
         (require 'n)
         (printf "~s\n" y)
         (flush-output)}
      #f
      "4")

(test @t{#lang racket
         (define-syntax (f stx)
           (syntax-case stx ()
             [(f)
              (raise (make-exn:fail:syntax "both" (current-continuation-marks) (list #'f stx)))]))}
      @t{(f)}
      (string-append "> (f)\n"
                     ". both in:\n"
                     "  f\n"
                     "  (f)")
      #t)

(test @t{#lang racket/base}
      @t{(begin (values) 1)}
      "1")

(test @t{#lang racket/base}
      @t{    (eval '(values 1 2))}
      @t{1@"\n"2})
(test @t{#lang racket/base}
      @t{    (eval '(list 1 2))}
      @t{'(1 2)})
(test @t{#lang racket/base}
      @t{    (eval '(lambda ()))}
      @t{lambda: bad syntax in: (lambda ())})
(test @t{#lang racket/base}
      @t{(expt 3 (void))}
      @rx{expt: contract violation.*given: #<void>})
(test @t{#lang racket/base}
      @t{1 2 ( 3 4}
      @t{1@"\n"2@"\n". read: expected a `)' to close `('})
(test @t{#lang racket/base}
      "1 2 . 3 4"
      "1\n2\n. read: illegal use of `.'")
(test @t{#lang racket/base}
      "1 2 (lambda ()) 3 4"
      "1\n2\n. lambda: bad syntax in: (lambda ())")
(test @t{#lang racket/base}
      "1 2 x 3 4"
      #rx"1\n2\n[.] [.] x:.*cannot reference an identifier before its definition")
(test @t{#lang racket/base}
      "1 2 (raise 1) 3 4"
      "1\n2\nuncaught exception: 1")
(test @t{#lang racket/base}
      "1 2 (raise #f) 3 4"
      "1\n2\nuncaught exception: #f")
(test @t{#lang racket/base}
      "(current-namespace (make-empty-namespace)) if"
      #rx". #%top-interaction: unbound identifier.*no #%app syntax transformer is bound")
(test @t{#lang racket/base}
      (string-append
       "(let ([old (error-escape-handler)])\n"
       "(+ (let/ec k\n(dynamic-wind\n"
       "(lambda () (error-escape-handler (lambda () (k 5))))\n"
       "(lambda () (expt 3 #f))\n"
       "(lambda () (error-escape-handler old))))\n"
       "10))")
      #rx"[.] [.] expt: contract violation.*given: #f\n15")
(test @t{#lang racket/base}
      "(write (list (syntax x)))"
      "(.)")
(test @t{#lang racket/base}
      "(parameterize ([current-output-port (open-output-string)]) (write #'1))"
      "")
(test @t{#lang racket/base}
      "(write-special 1)"
      "1#t")
(test @t{#lang racket/gui}
      (format "~s ~s ~s"
              '(define s (make-semaphore 0))
              '(queue-callback
                (lambda ()
                  (dynamic-wind
                   void
                   (lambda () (expt 3 #f))
                   (lambda () (semaphore-post s)))))
              '(begin (yield s) (void)))
      #rx"[.] [.] expt: contract violation.*given: #f")
(test @t{#lang racket/base}
      (format "~s ~s" 
              '(define x 1) 
              '((λ (x y) y) (set! x (call/cc (lambda (x) x))) (x 3)))
      #rx". . application:.*given: 3")
(test @t{#lang racket/base}
      (format "~s ~s ~s ~s"
              '(begin (define k (call/cc (λ (x) x)))
                      (define x 'wrong))
              '(set! x 'right)
              '(k 1)
              'x)
      "'right")
(test @t{#lang racket/base}
      (format "~s"
              '(call-with-continuation-prompt
                (lambda ()
                  (eval '(begin (abort-current-continuation
                                 (default-continuation-prompt-tag)
                                 1 2 3)
                                10)))
                (default-continuation-prompt-tag)
                list))
      "'(1 2 3)")
(test @t{#lang racket/gui}
      "(vector (new snip%))"
      "(vector .)")
(test @t{#lang racket/base}
      "(begin (thread (lambda () x)) (sleep 1/10))"
      #rx"[.] [.] x:.*cannot reference an identifier before its definition")
(test @t{#lang racket/base}
      "(require texpict/utils)(disk 3)"
      ".")
(test @t{#lang racket/base}
      (string-append
       "(require mzlib/pretty)"
       "(pretty-print-print-hook (lambda x (expt 3 #f)))"
       "(list 1 2 3)")
      "'(1 2 3)")

;; test protection against user-code changing the namespace
(test @t{#lang racket/base
         (current-namespace (make-base-namespace))}
      "(+ 1 2)"
      "3")
(test @t{#lang racket/base
         (current-namespace (make-base-empty-namespace))}
      "(+ 1 2)"
      "3")
(test @t{#lang racket/base}
     @t{(parameterize ([current-directory "/does/not/exists/well/it/better/not/anwyays"])
         (load @in-here{module-lang-test-syn-error.rkt}))}
     ;; test to make sure that we don't get "exception raised by error display handler"
     #rx"module-lang-test-syn-error.rkt:[0-9]+:[0-9]+: lambda: bad syntax in: \\(lambda\\)")

(test @t{#lang racket
(module+ main (printf "main\n"))
(module+ test (printf "test\n"))
(module+ other (printf "other\n"))}
      #f
      #rx"main\ntest")


(test @t{#lang racket}
      (format "~s" '(+ 1 (+ 1 (abort-current-continuation
                               (default-continuation-prompt-tag)
                               (lambda () 
                                 (abort-current-continuation
                                  (default-continuation-prompt-tag)
                                  (λ () 0)))))))
      "0")
   
(test @t{#lang racket}
      (format "~s ~s ~s" 
              '1
              '(+ 1 (+ 1 (abort-current-continuation
                          (default-continuation-prompt-tag)
                          (lambda () 
                            (abort-current-continuation
                             (default-continuation-prompt-tag)
                             (λ () 0))))))
              '2)
      "1\n0")

(test @t{#lang racket}
      (format "~s" 
              '(begin
                 1
                 (+ 1 (+ 1 (abort-current-continuation
                            (default-continuation-prompt-tag)
                            (lambda () 
                              (abort-current-continuation
                               (default-continuation-prompt-tag)
                               (λ () 0))))))
                 2))
      "0")

(fire-up-drracket-and-run-tests run-test)
