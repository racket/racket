#reader scribble/reader
#lang scheme/gui
(require "module-lang-test-utils.ss")
(provide run-test)

;; set up for tests that need external files
(write-test-modules
 (module module-lang-test-tmp mzscheme
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
   (provide #%module-begin [rename bug-datum #%datum])))

(test @t{}
      #f
      #rx"Module Language: There must be a valid module .* Try starting"
      #t)
(test @t{1}
      #f
      #rx"Module Language: only a module expression is allowed"
      #t)
(test @t{(module m mzscheme) 1}
      #f
      #rx"Module Language: there can only be one expression in the definitions"
      #t)
(test @t{#lang mzscheme
         (define x 1)}
      @t{x}
      "1")
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
      #rx"procedure:+")
(test @t{(module n mzscheme (provide (all-from-except mzscheme +)))}
      @t{+}
      #rx"procedure:+")
(test @t{(module m mzscheme
           (require (prefix x: (lib "list.ss")) (lib "list.ss")))}
      @t{foldl}
      #rx"foldl>")
(test @t{(module m mzscheme
           (require (prefix x: (lib "list.ss")) (lib "list.ss")))}
      @t{x:foldl}
      #rx"foldl>")
(test @t{(module m (file "@in-here{module-lang-test-tmp.ss}") x)}
      @t{x}
      "1")
;; + shouldn't be bound in the REPL because it isn't bound in the module.
(test @t{(module m (file "@in-here{module-lang-test-tmp.ss}") x)}
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
           (require-for-syntax (file "@in-here{module-lang-test-tmp2.ss}"))
           (provide s)
           (define-syntax (s stx) e))}
      @t{(require m) s}
      #rx"module-lang-test-tmp2.ss:1:[67][90]: compile: bad syntax; literal data is not allowed, because no #%datum syntax transformer is bound in: 1$")
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
      ". compile: bad syntax; reference to top-level identifier is not allowed, because no #%top syntax transformer is bound in: cons")
(test @t{(module m (file "@in-here{module-lang-test-tmp.ss}") 1 2 3)}
      @t{1} ;; just make sure no errors.
      "1")
