#lang at-exp scheme/gui
(require "module-lang-test-utils.ss")
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
 (module module-lang-test-tmp4 scheme/base
   (/ 888 2)
   (provide (except-out (all-from-out scheme/base) #%top-interaction))))

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
(test @t{(module m mzscheme) 1}
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
(test @t{#lang scheme
         3}
      #f
      "3")
(test @t{(module m mzscheme (provide x) (define x 1))}
      @t{x}
      "1")
(test @t{(module m mzscheme (define x 1))}
      @t{x}
      "1")
(test @t{(module m mzscheme (define x 1) (define y 1) (provide y))}
      @t{x}
      "1")
(test @t{(module m mzscheme (define x 1) (define y 2) (provide y))}
      @t{y}
      "2")
(test @t{(module m mzscheme (require (lib "list.ss")))}
      @t{foldl}
      #rx"foldl")
(test @t{(module m mzscheme (require (rename (lib "list.ss") local-foldl foldl)))}
      @t{local-foldl}
      #rx"foldl>")
(test @t{(module m mzscheme (require (all-except (lib "list.ss") foldl)))}
      @t{first}
      #rx"first>")
(test @t{(module m mzscheme (require (all-except (lib "list.ss") foldl)))}
      @t{foldl}
      ". . reference to an identifier before its definition: foldl")
(test @t{(module m mzscheme (require (prefix mz: mzscheme)))}
      @t{mz:+}
      #rx"procedure:[+]")
(test @t{(module n mzscheme (provide (all-from-except mzscheme +)))}
      @t{+}
      #rx"procedure:[+]")
(test @t{(module m mzscheme
           (require (prefix x: (lib "list.ss")) (lib "list.ss")))}
      @t{foldl}
      #rx"foldl>")
(test @t{(module m mzscheme
           (require (prefix x: (lib "list.ss")) (lib "list.ss")))}
      @t{x:foldl}
      #rx"foldl>")
(test @t{(module m (file @in-here{module-lang-test-tmp1.ss}) x)}
      @t{x}
      "1")
;; + shouldn't be bound in the REPL because it isn't bound in the module.
(test @t{(module m (file @in-here{module-lang-test-tmp1.ss}) x)}
      @t{+}
      ". . reference to an identifier before its definition: +")
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
      ". . reference to an identifier before its definition: a")
(test @t{(module m mzscheme
           (define-syntax (x stx) #'(define-syntax (a stx) #'10))
           x
           x)}
      @t{a}
      ". . reference to an identifier before its definition: a")
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
           (require-for-syntax (file @in-here{module-lang-test-tmp2.ss}))
           (provide s)
           (define-syntax (s stx) e))}
      @t{(require m) s}
      @rx{compile: bad syntax;
          literal data is not allowed, because no #%datum syntax transformer
          is bound in: 1$})
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
(test @t{#lang scheme
         (eval 'cons)}
      #f
      @rx{. compile: unbound identifier \(and no #%top syntax transformer is bound\) in: cons})
(test @t{(module m (file @in-here{module-lang-test-tmp1.ss}) 1 2 3)}
      @t{1} ;; just make sure no errors.
      "1")

;; check that we have a working repl in the right language after
;; syntax errors, unless it's a bad language
(test @t{#lang scheme
         (define x 1)
         (define y (/ 0))}
      @t{(+ 122 x)}
      @rx{. /: division by zero
          123}
      #t)
(test @t{#lang scheme
         (define x 1)
         (define y (/ 0))}
      @t{(if x 123)}
      @rx{. /: division by zero
          . if: bad syntax.*"else"}
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
(test @t{(module xx (file @in-here{module-lang-test-tmp4.ss})
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
          No such file or directory
          Module Language: invalid language specification
          Interactions disabled}
      #t)
(test @t{#lang setup/infotab}
      #f
      ;; test the complete buffer, to make sure that there is no error
      "\nInteractions disabled: setup/infotab does not support a REPL (no #%top-interaction)"
      #t)

;; test scheme/load behavior
(test @t{#lang scheme/load
         (module m mzscheme (provide x) (define x 2))
         (require 'm)
         (printf "~s\n" x)
         (flush-output)}
      #f
      "2")
(test @t{#lang scheme/load
         (module m mzscheme (provide x) (define x 2))
         (module n scheme/base (require 'm) (provide y) (define y (* x x)))
         (require 'n)
         (printf "~s\n" y)
         (flush-output)}
      #f
      "4")

(test @t{#lang scheme
         (define-syntax (f stx)
           (syntax-case stx ()
             [(f)
              (raise (make-exn:fail:syntax "both" (current-continuation-marks) (list #'f stx)))]))}
      @t{(f)}
      #<<--
> (f)
. both in:
  f
  (f)
--
      #t)

;; test protection against user-code changing the namespace
(test @t{#lang scheme/base
         (current-namespace (make-base-namespace))}
      "(+ 1 2)"
      "3")
(test @t{#lang scheme/base
         (current-namespace (make-base-empty-namespace))}
      "(+ 1 2)"
      "3")


(require "drracket-test-util.ss")
(fire-up-drscheme-and-run-tests run-test)
