
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
                     (#%printing-module-begin
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
                        (provide x)))))
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
               (#%printing-module-begin
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
                  (define b (+ a 1)))))])
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

(expand
 (expand 
  #'(module m racket/base
      (define-syntax-rule (go x)
        (begin
          (define other 1)
          (define-syntax-rule (x) other)))
      (go f)
      (module* test #f
        (f)))))

(expand
 (expand 
  #'(module m racket/base
      (define (f #:opt [opt 3])
        opt)
      (module* test #f
        (f)))))

(expand
 (expand
  '(module m racket/base
     (define (print-cake n) n)
     (module* main2 #f
       (module* inner-main #f
         (print-cake 20))))))

(syntax-case (parameterize ([current-namespace (make-base-namespace)])
               (expand
                '(module m racket/base
                   (define x 0)
                   (module* sub #f
                     (+ x 1))))) ()
  [(_ name lang (mb cr (def (x1) _) (mod sub #f
                                         (mb_ cr_ (app cwv (lam () (app_ + x2 _))
                                                       _)))))
   (begin
     (test #t free-identifier=? #'x1 #'x2)
     (let ([mpi (car (identifier-binding #'x2))])
       (define-values (a b) (module-path-index-split mpi))
       (test '(submod "..") values a)
       (test #t module-path-index? b)
       (define-values (ba bb) (module-path-index-split b))
       (test #f values ba)
       (test #f values bb)
       (test '(sub) module-path-index-submodule b)))])

(parameterize ([current-namespace (make-base-namespace)])
  (eval
   (expand
    '(module m racket
       (module X racket
         (define x 1)
         (provide x))
       (module Y racket
         (require (submod ".." X))
         (define y (add1 x)))))))

;; Check that we can wrap a `begin' around a submodule:
(parameterize ([current-namespace (make-base-namespace)])
  (define m (expand
             '(module m racket/base
                (module sub racket/base
                  (require (for-syntax racket/base))
                  (define-syntax (sym-app stx)
                    (syntax-case stx ()
                      [() 10]))))))
  (eval (syntax-case m ()
          [(md m r/b (m-b cr mod))
           #`(md m r/b (m-b (begin 10 mod)))])))

(parameterize ([current-namespace (make-base-namespace)])
  (eval
   (expand
    (expand
     '(module foo2 racket
        (begin-for-syntax 
         (define here 'here))
        
        (module+ m2
          (begin-for-syntax here)))))))

(parameterize ([current-namespace (make-base-namespace)])
  (eval
   (expand
    (expand '(module t racket
               (begin-for-syntax)
               (begin-for-syntax)
               (define x 7)
               (module* t #f x))))))

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
;; Check module-source for submodule:

(let ()
  (define (go set-name get-name)
    (parameterize ([current-namespace (make-base-namespace)])
      (parameterize ([current-module-declare-source set-name])
        (eval '(module m racket/base
                 (module+ sub
                   (provide v)
                   (define v (variable-reference->module-source
                              (#%variable-reference))))))
        (test get-name dynamic-require '(submod 'm sub) 'v))))
  (go #f 'm)
  (go 'other 'other))

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
;; Check `syntax-local-submodules':

(test '() 'local-submodules
      (let ()
        (define-syntax (m stx) #`(quote #,(syntax-local-submodules)))
        (m)))
                                                  
(module check-submodule-list racket/base
  (#%printing-module-begin
   (require (for-syntax racket/base))
   (provide x)
   (define-syntax (m stx) #`(quote #,(syntax-local-submodules)))
   (module m1 racket/base)
   (module m2 racket/base)
   (module* m3 racket/base)
   (define x (m))))

(test '(m1 m2) dynamic-require ''check-submodule-list 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `syntax-local-module-exports' uses submodules:

(module check-submodule-exports racket/base
  (require (for-syntax racket/base))
  (provide x)
  (define-syntax (m stx) 
    #`(quote #,(cdr (assoc 0 (syntax-local-module-exports ''m1)))))
  (module m1 racket/base (provide s) (define s 10))
  (define x (m)))

(test '(s) dynamic-require ''check-submodule-exports 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory for testing

(define temp-dir
  (build-path (find-system-path 'temp-dir)
              (format "submodule-tests-~s" (current-seconds))))
(make-directory temp-dir)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check submodule resolution of relative paths:

(parameterize ([current-load-relative-directory temp-dir])
  (define p (collection-file-path "has-submod.rkt" "tests" "racket"))
  (dynamic-require p #f)
  (test 'for-submod dynamic-require `(submod ,p main) 'has-submod))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try various submodule combinations to test sorting:

(let ()
  (define (try-submods l)
    (define e `(module e racket/base
                 ,@(for/list ([n l])
                     `(module+ ,n
                        (define name ',n)
                        (provide name)))))
    (define fn (build-path temp-dir "has-submod.rkt"))
    (define dir (build-path temp-dir "compiled"))
    (define fn-zo (build-path dir "has-submod_rkt.zo"))
    (unless (directory-exists? dir) (make-directory dir))
    (with-output-to-file fn-zo
      #:exists 'truncate
      (lambda () (write (compile e))))
    (for ([n l])
      (test n dynamic-require `(submod ,fn ,n) 'name)))
  (try-submods '(a b))
  (try-submods '(xa xb))
  (try-submods '(test main)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check stickiness of submodule-from-bytecode loading:

(let ()
  (define e `(module e racket/base
               (module sub1 racket/base (provide x) (define x 'sub1))
               (module sub2 racket/base (provide x) (define x 'sub2))
               (module sub3 racket/base (provide x) (define x 'sub3))))
  (define fn (build-path temp-dir "has-submod2.rkt"))
  (define dir (build-path temp-dir "compiled"))
  (define fn-zo (build-path dir "has-submod2_rkt.zo"))
  (unless (directory-exists? dir) (make-directory dir))
  (with-output-to-file fn
    #:exists 'truncate
    (lambda () (write e)))
  (with-output-to-file fn-zo
    #:exists 'truncate
    (lambda () (write (compile e))))
  (test 'sub1 dynamic-require `(submod ,fn sub1) 'x)
  (parameterize ([use-compiled-file-paths null])
    (test 'sub2 dynamic-require `(submod ,fn sub2) 'x))
  (let ([ns (current-namespace)])
    (parameterize ([current-namespace (make-base-namespace)]
                   [use-compiled-file-paths null])
      (namespace-attach-module ns `(submod ,fn sub1))
      (test 'sub3 dynamic-require `(submod ,fn sub3) 'x))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete the temp-dir

(let loop ([x temp-dir])
  (cond [(file-exists? x) (delete-file x)]
        [(directory-exists? x) (parameterize ([current-directory x])
                                 (for-each loop (directory-list)))
                               (delete-directory x)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module attach

(let ()
  (define (attach-tests use-path?)
    (define (test-attach decl-only? pre-check?)
      (define path (and use-path?
                        (build-path (find-system-path 'temp-dir) "mod.rkt")))
      (let ([ns1 (make-base-namespace)]
            [ns2 (make-base-namespace)]
            [ns3 (make-base-namespace)])
        (parameterize ([current-namespace ns1])
          (parameterize ([current-module-declare-name (and use-path?
                                                           (make-resolved-module-path path))])
            (eval '(module m racket/base 
                     (provide root) (define root 'm)
                     (module+ n (provide x) (define x 'x)))))
          (unless decl-only?
            (dynamic-require (or path ''m) #f)
            (when pre-check?
              (test 'x dynamic-require `(submod ,(or path ''m) n) 'x))))
        (parameterize ([current-namespace ns2])
          ((if decl-only? namespace-attach-module-declaration namespace-attach-module) 
           ns1 
           (or path ''m))
          (test 'x dynamic-require `(submod ,(or path ''m) n) 'x))
        (unless decl-only?
          (parameterize ([current-namespace ns1])
            (test 'x dynamic-require `(submod ,(or path ''m) n) 'x)))
        (parameterize ([current-namespace ns3])
          ((if decl-only? namespace-attach-module-declaration namespace-attach-module) 
           ns1 
           `(submod ,(or path ''m) n))
          (test 'm dynamic-require (or path ''m) 'root))))
    (test-attach #f #f)
    (test-attach #f #t)
    (test-attach #t #f))
  (attach-tests #f)
  (attach-tests #t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; module->namespace

(module check-to-namespace-1 racket/base
  (module* main #f
    (define x 10)
    (define v
      (eval 'x (variable-reference->namespace (#%variable-reference))))
    (provide v)))

(test 10 dynamic-require `(submod 'check-to-namespace-1 main) 'v)

(module check-to-namespace-2 racket/base
  (require racket/math)
  (module* main #f
    (define v
      (eval 'pi (variable-reference->namespace (#%variable-reference))))
    (provide v)))

(require racket/math)
(test pi dynamic-require `(submod 'check-to-namespace-2 main) 'v)

(module check-to-namespace-3.0 racket/base
  (define x 13)
  (define v
    (eval 'x (variable-reference->namespace (#%variable-reference))))
  (provide v))

(test 13 dynamic-require ''check-to-namespace-3.0 'v)

(module check-to-namespace-3 racket/base
  (define x 13)
  (module* main #f
    (define v
      (eval 'x (variable-reference->namespace (#%variable-reference))))
    (provide v)))

(test 13 dynamic-require `(submod 'check-to-namespace-3 main) 'v)

(let ([path (build-path (current-directory) "ctn-no-such-file.rkt")])
  (parameterize ([current-module-declare-name (make-resolved-module-path path)])
    (eval
     '(module check-to-namespace-3 racket/base
        (define x 130)
        (module* main #f
          (define v
            (eval 'x (variable-reference->namespace (#%variable-reference))))
          (provide v)))))
  (test 130 dynamic-require `(submod ,path main) 'v))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module local-expand-lang racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [mb #%module-begin]) (except-out (all-from-out racket/base) #%module-begin))
  (define-syntax (mb stx)
    (syntax-case stx ()
      [(_ rest ...)
       (local-expand #'(#%plain-module-begin rest ...) 'module-begin (list #'module*))])))

(module local-expand-lang2 racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [mb #%module-begin]) (except-out (all-from-out racket/base) #%module-begin))
  (define-syntax (mb stx)
    (syntax-case stx ()
      [(_ rest ...)
       #'(#%plain-module-begin (begin-for-syntax (module* foo #f)) rest ...)])))

;; check that the macro-introduced `module*' works right:
(module local-expand-lang-test 'local-expand-lang
  (module m 'local-expand-lang2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that quoted submodule paths work with `all-from-out':

(module has-submodule-all-from-out racket/base
  (module a racket/base
    (define x-from-submodule-out 10)
    (provide x-from-submodule-out))
  
  (require 'a)
  (void x-from-submodule-out)
  (provide (all-from-out 'a)))
(require 'has-submodule-all-from-out)
(test 10 values x-from-submodule-out)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check `syntax-local-submodules' (and `syntax-local-module-exports'
;; for submodules) in compile and expand modes

(let ([e '(module x racket/base
            (#%printing-module-begin
             (require (for-syntax racket/base))
             
             (module m racket/base)
             
             (define-syntax (m stx)
               (syntax-local-module-exports ''m) ; should succeed
               #`(quote #,(syntax-local-submodules)))
             
             (define x (m))
             x
             (provide x)))])
  (parameterize ([current-namespace (make-base-namespace)])
    (eval e)
    (test '(m) dynamic-require ''x 'x))
  (parameterize ([current-namespace (make-base-namespace)])
    (eval (expand e))
    (test '(m) dynamic-require ''x 'x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check context on `#%module-begin' for a subform

(module check-submodule-module-begin '#%kernel
  (module mb racket/base
    (require (for-syntax racket/base))
    (provide (except-out (all-from-out racket/base) #%module-begin)
             (rename-out [module-begin #%module-begin]))
    (define-syntax (module-begin stx)
      #`(#%module-begin #,(datum->syntax stx ;; should have initial imports
                                         '(provide (all-defined-out))))))
  (module n (submod ".." mb)
    (void)
    (void)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check phase-level-2 submodules:

(module check-module-meta-2 racket
  (require (for-meta 2 racket/base))
  
  (begin-for-syntax
   (begin-for-syntax
    (module* main #f
      (define v 'ok)
      (provide v)))))

(test 'ok dynamic-require '(submod 'check-module-meta-2 main) 'v)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check an interaciton of submodules and taints

(module m racket
  (module q racket
    (provide (except-out (all-from-out racket)
                         #%module-begin)
             (rename-out [module-begin #%module-begin]))
    (define-syntax (module-begin stx)
      (syntax-case stx ()
        [(_ . r)
         (local-expand #`(#%module-begin . r) 'module-begin null)])))

  (module p (submod ".." q)
    (module n racket
      (provide #%module-begin)
      (define-syntax (#%module-begin stx)
        (define (arm p)
          (syntax-property (syntax-arm p) 'taint-mode 'opaque))
        (with-syntax ([mod #'(module m racket/base (add1 0))])
          (arm #'(#%plain-module-begin (begin-for-syntax mod))))))

    (module m (submod ".." n))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check submodule and `for-label`

(module requires-submodule-for-label racket/base
  (module foo racket/base)
  (require (for-label (submod "." foo))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
