
(load-relative "loadtest.rktl")

(Section 'submodule)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #t module-path? '(submod "."))
(test #t module-path? '(submod "." x))
(test #t module-path? '(submod "." x y))
(test #t module-path? '(submod "." x ".." y))
(test #t module-path? '(submod "." x ".." y ".." ".." ".."))
(test #f module-path? '(submod "." "x" y))
(test #f module-path? '(submod "." x "y"))
(test #t module-path? '(submod ".."))
(test #t module-path? '(submod ".." x))
(test #t module-path? '(submod ".." x y))
(test #f module-path? '(submod ".." "x" y))
(test #f module-path? '(submod ".." x "y"))
(test #t module-path? '(submod ".." ".."))
(test #f module-path? '(submod ".." "."))

(test #t module-path? '(submod x a b))
(test #f module-path? '(submod x "a" b))

(test #t module-path? '(submod 'x a))
(test #t module-path? '(submod 'x))

(define (check-resolution root [root-mod root])
  (test root resolved-module-path-name
        (module-path-index-resolve (module-path-index-join `(submod ,root-mod) #f)))
  (test root resolved-module-path-name
        (module-path-index-resolve (module-path-index-join `(submod ".") (make-resolved-module-path root))))
  (test root resolved-module-path-name
        (module-path-index-resolve (module-path-index-join `(submod "." "..") (make-resolved-module-path (list root 'y)))))
  (test root resolved-module-path-name
        (module-path-index-resolve (module-path-index-join `(submod "..") (make-resolved-module-path (list root 'y)))))
  (err/rt-test
   (module-path-index-resolve (module-path-index-join `(submod "..") (make-resolved-module-path root)))
   exn:fail?))
(check-resolution 'x ''x)
(check-resolution (path->complete-path "file.rkt"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module subm-example-1 racket/base
  (define x 1)
  (provide x)
  (module* a racket/base
    (define x '1a)
    (provide x)
    (module* i racket/base
      (define x '1ai)
      (provide x)))
  (module* b racket/base
    (define x '1b)
    (provide x)))

(test 1 dynamic-require ''subm-example-1 'x)
(test '1a dynamic-require '(submod 'subm-example-1 a) 'x)
(test '1ai dynamic-require '(submod 'subm-example-1 a i) 'x)
(test '1b dynamic-require '(submod 'subm-example-1 b) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module subm-example-2 racket/base
  (define x 1)
  (provide x)
  (module a racket/base
    (define x '1a)
    (provide x)
    (module i racket/base
      (define x '1ai)
      (provide x)))
  (module b racket/base
    (define x '1b)
    (provide x)))

(test 1 dynamic-require ''subm-example-2 'x)
(test '1a dynamic-require '(submod 'subm-example-2 a) 'x)
(test '1ai dynamic-require '(submod 'subm-example-2 a i) 'x)
(test '1b dynamic-require '(submod 'subm-example-2 b) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module subm-example-9 racket/base
  (define x '(1))
  (provide x)
  (module* a #f
    (require racket/list)
    (define z (last x))
    (provide z)
    (define-syntax-rule (as-last) last)
    (provide as-last)))

(test '(1) dynamic-require ''subm-example-9 'x)
(test 1 dynamic-require '(submod 'subm-example-9 a) 'z)

(module subm-example-use-9 racket/base
  (require (submod 'subm-example-9 a))
  (define x ((as-last) '(1 2 3)))
  (provide x))
(test 3 dynamic-require ''subm-example-use-9 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([o (open-output-bytes)])
  (write (compile '(module subm-example-0 racket/base
                     (define x 1)
                     (provide x)
                     (module z racket/base
                       (define z 26))
                     (module* a racket/base
                       (define x '1a)
                       (provide x)
                       (module* i racket/base
                         (define x '1ai)
                         (provide x)))
                     (module* b racket/base
                       (define x '1b)
                       (provide x))))
         o)
  (define c (parameterize ([read-accept-compiled #t])
              (read (open-input-bytes (get-output-bytes o)))))
  (test 'subm-example-0 values (module-compiled-name c))
  (test 2 values (length (module-compiled-submodules c #f)))
  (for ([sub (in-list (append (module-compiled-submodules c #t)
                              (module-compiled-submodules c #f)))]
        [name '(z a b)])
    (test (list 'subm-example-0 name) values (module-compiled-name sub))
    (when (eq? name 'a)
      (test 1 values (length (module-compiled-submodules sub #f)))
      (test '(subm-example-0 a i) values (module-compiled-name (car (module-compiled-submodules sub #f))))))
  (define a (module-compiled-name (car (module-compiled-submodules c #f))
                                  'reset))
  (test 'reset values (module-compiled-name a))
  (test '(reset i) values (module-compiled-name (car (module-compiled-submodules a #f))))
  (define aa (module-compiled-submodules a #f (list (module-compiled-name a 'again))))
  (test '(reset again) values (module-compiled-name (car (module-compiled-submodules aa #f))))
  (test '(reset again i) values (module-compiled-name (car (module-compiled-submodules (car (module-compiled-submodules aa #f)) #f)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([decl '(module m racket/base
               (module z racket/base
                 (provide z)
                 (define z 26))
               (require (submod "." z))
               (provide !)
               (define ! (add1 z))
               (module* a racket/base
                 (provide a)
                 (define a 1))
               (module* b racket/base
                 (require (submod "." ".." a))
                 (provide b)
                 (define b (+ a 1))))])
  ;; write a module as part of a top-level sequence:
  (parameterize ([current-namespace (make-base-namespace)])
    (define o (open-output-bytes))
    (write (compile `(begin ,decl 10)) o)
    (define e (parameterize ([read-accept-compiled #t])
                (read (open-input-bytes (get-output-bytes o)))))
    (eval e)
    (test 2 eval '(dynamic-require '(submod 'm b) 'b))
    (test 27 eval '(dynamic-require ''m '!)))
  ;; write a module as directory:
  (parameterize ([current-namespace (make-base-namespace)])
    (define o (open-output-bytes))
    (write (compile decl) o)
    (define s (get-output-bytes o))
    (define e (parameterize ([read-accept-compiled #t])
                (read (open-input-bytes s))))
    (test 1 values (length (module-compiled-submodules e #t)))
    (test 2 values (length (module-compiled-submodules e #f)))
    (eval e)
    (test 2 eval '(dynamic-require '(submod 'm b) 'b))
    (test 27 eval '(dynamic-require ''m '!))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module submodule-exp5 racket/base
  (require (for-syntax racket/base))
  (define root 0)
  (define-syntax (rooty stx) #'8)
  (begin-for-syntax
   (define nine 9))
  (module* a #f
    (provide root))
  (module* b #f
    (define root 1)
    (module* c #f
      (provide root 
               rooty
               (for-syntax nine))))
  (module* d racket/base
    (require (for-syntax racket/base))
    (require (submod "." ".." b c))
    (define-syntax (use-nine stx) (datum->syntax #'here nine))
    (define rootd (use-nine))
    (provide rootd)))
(test 0 dynamic-require '(submod 'submodule-exp5 a) 'root)
(test 1 dynamic-require '(submod 'submodule-exp5 b c) 'root)
(test 8 dynamic-require '(submod 'submodule-exp5 b c) 'rooty)
(test 9 dynamic-require '(submod 'submodule-exp5 d) 'rootd)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define (simplify base p)
    (resolved-module-path-name (module-path-index-resolve (module-path-index-join p base))))
  (define (check root-path)
    (define a (module-path-index-join root-path #f))
    (define ar (resolved-module-path-name  (module-path-index-resolve a)))
    
    (test (cons ar '(x)) simplify a '(submod "." x))
    (test (cons ar '(x y z)) simplify a '(submod "." x y z))
    (test (cons ar '(y z)) simplify a '(submod "." x ".." y z))
    (test (cons ar '(z)) simplify a '(submod "." x y ".." ".." z))
    (test (cons ar '(q x)) simplify 
          (module-path-index-join '(submod "." q) a)
          '(submod "." x))
    (test (cons ar '(x)) simplify 
          (module-path-index-join '(submod "." q) a)
          '(submod "." ".." x))
    (test ar simplify 
          (module-path-index-join '(submod "." q) a)
          '(submod "." "..")))
  (check ''a)
  (check 'racket/base))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that re-expansion with submodules works right:

(eval 
 (expand
  (expand '(module sub1-m racket/base
             (module* n racket/base
               (define-syntax-rule (mk v)
                 (begin
                   (define x v)
                   (provide x)))
               (define x 11)
               (mk 12))))))

(test 12 dynamic-require '(submod 'sub1-m n) 'x)

(eval 
 (expand
  (expand '(module sub4-m racket/base
             (module n racket/base
               (define-syntax-rule (mk v)
                 (begin
                   (define x v)
                   (provide x)))
               (define x 11)
               (mk 120))))))

(test 120 dynamic-require '(submod 'sub4-m n) 'x)

(eval 
 (expand
  (expand '(module sub3.5-m racket
             (begin-for-syntax
              (module* n #f (define x 8.5) (provide x)))))))
(test 8.5 dynamic-require '(submod 'sub3.5-m n) 'x)

(eval 
 (expand
  (expand '(module sub3-m racket/base
             (module* n #f (define x 8) (provide x))))))
(test 8 dynamic-require '(submod 'sub3-m n) 'x)

(eval 
 (expand
  (expand '(module sub2-m racket/base
             (define x 11)
             (module* n #f
               (define-syntax-rule (mk v)
                 (begin
                   (define y v)
                   (provide y)))
               (mk x))))))

(test 11 dynamic-require '(submod 'sub2-m n) 'y)

(expand
 (expand
  #'(module s racket/base
      (struct node (height))
      (node-height 0)
      (module+ main))))

(expand
 (expand
  #'(module s racket/base
      (module* main #f)
      (struct node (height))
      (node-height 0))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `begin-for-syntax' doesn't affect `module' with non-#f language:

(module subm-example-6 racket
  (begin-for-syntax
   (module* a racket
     (define x '6a)
     (provide x)
     (begin-for-syntax
      (module* i racket/base
        (define x '6ai)
        (provide x))))))

(test '6a dynamic-require '(submod 'subm-example-6 a) 'x)
(test '6ai dynamic-require '(submod 'subm-example-6 a i) 'x)

(module subm-example-x6 racket
  (begin-for-syntax
   (module a racket
     (define x 'x6a)
     (provide x)
     (begin-for-syntax
      (module i racket/base
        (define x 'x6ai)
        (provide x))))))

(test 'x6a dynamic-require '(submod 'subm-example-x6 a) 'x)
(test 'x6ai dynamic-require '(submod 'subm-example-x6 a i) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module subm-example-7 racket/base
  (require (for-syntax racket/base))
  (define x 7)
  (begin-for-syntax
   (define x 8)
   (module* a #f
     (define z x)
     (provide z))))

(test '8 dynamic-require '(submod 'subm-example-7 a) 'z)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module subm-example-11 racket/base
  (require (for-syntax racket/base))
  (define (listify x) (list x))

  (define-syntax (add-submodule stx)
    (with-syntax (#;[(define-b-module) (generate-temporaries '(define-b))])
      (syntax-local-lift-module-end-declaration
       #'(module* a #f
           (provide a)
           (define a (listify 'a))))
      (syntax-local-lift-module-end-declaration
       #'(define-b-module))
      #'(define-syntax (define-b-module stx)
          #'(module* b #f
              (define b 'b)
              (provide b)))))
  (add-submodule)
  (provide add-submodule))

(test '(a) dynamic-require '(submod 'subm-example-11 a) 'a)
(test 'b dynamic-require '(submod 'subm-example-11 b) 'b)

(module subm-example-12 racket/base
  (require 'subm-example-11)
  (add-submodule))

(test '(a) dynamic-require '(submod 'subm-example-12 a) 'a)
(test 'b dynamic-require '(submod 'subm-example-12 b) 'b)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The `section' form:

(module module+-example-1 racket/base
  (module+ alpha (define a root) (provide a))
  (module+ beta (define b (+ root 1)) (provide b))
  (module+ alpha (define aa (+ a a)) (provide aa))
  (module+ gamma
    (require (submod "." ".." beta))
    (provide c)
    (define c (+ b 1)))
  (module+ beta)
  (module+ beta)
  (module+ beta)
  (define root 1))

(test 1 dynamic-require '(submod 'module+-example-1 alpha) 'a)
(test 2 dynamic-require '(submod 'module+-example-1 alpha) 'aa)
(test 2 dynamic-require '(submod 'module+-example-1 beta) 'b)
(test 3 dynamic-require '(submod 'module+-example-1 gamma) 'c)

(syntax-test #'(module+ a))
(err/rt-test (eval #'(module m racket/base module+)) exn:fail:syntax?)
(err/rt-test (eval #'(module m racket/base (module+))) exn:fail:syntax?)
(err/rt-test (eval #'(module m racket/base (module+ 1))) exn:fail:syntax?)
(err/rt-test (eval #'(module m racket/base (module+ a . 2))) exn:fail:syntax?)

;; Check that `#%module-begin' context is reasonable:
(module module+-example-2 racket/base
  (module alt-mod-beg racket/base
    (provide (rename-out [module-begin #%module-begin])
             module+
             #%datum
             #%app
             void)
    (define-syntax-rule (module-begin a b c)
      (#%module-begin a (define x (+ b c)) (provide x))))
  (module a (submod "." ".." alt-mod-beg)
    (module+ b (void) 1 2)
    3 4))

(test 3 dynamic-require '(submod 'module+-example-2 a b) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that various shaodwings are allowed:

(module subm-example-20 racket/base
  (require (only-in racket/list third))
  (provide first get-first second)

  (define first 1)
  (define (get-first) first) ;; verifies shadowing not mutation
  (module* first #f
    (define first 0)  ;; shadows outer 'first'
    (define f first)
    (provide first f))

  (define second 2)
  (module* second #f
    (require (only-in racket/list second))
    (define s second)
    (provide second s))

  (module* third #f
    (require (only-in mzlib/list third)) ; different binding than from `racket/list'
    (define t third)
    (provide third t)))

(test 1 dynamic-require ''subm-example-20 'first)
(test 1 (dynamic-require ''subm-example-20 'get-first))
(test 2 dynamic-require ''subm-example-20 'second)
(test 'b (dynamic-require '(submod 'subm-example-20 second) 'second) '(a b c))
(test 'b (dynamic-require '(submod 'subm-example-20 second) 's) '(a b c))
(test 'c (dynamic-require '(submod 'subm-example-20 third) 'third) '(a b c))
(test 'c (dynamic-require '(submod 'subm-example-20 third) 't) '(a b c))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `all-defined-out':

(module subm-all-defined-1 racket/base
  (module+ main
    (define x 10)
    (provide (all-defined-out))))

(test 10 dynamic-require '(submod 'subm-all-defined-1 main) 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
