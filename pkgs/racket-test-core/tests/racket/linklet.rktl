
(load-relative "loadtest.rktl")

(Section 'linklet)

(require racket/linklet
         racket/unsafe/ops)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define l (compile-linklet '(linklet
                                  ()
                                  ()
                                4)))
  (test #t linklet? l)
  (test #t instance? (instantiate-linklet l '()))
  (test 4 instantiate-linklet l '() (make-instance 'l))

  (err/rt-test (instantiate-linklet l) exn:fail:contract:arity?)
  (err/rt-test (instantiate-linklet l '#()))
  (err/rt-test (instantiate-linklet l (list 5)))
  (err/rt-test (instantiate-linklet l '() 5)))

(parameterize ([current-compile-target-machine #f])
  (define c (compile '(module m '#%kernel
                        (#%provide b)
                        (define-values (b) (+ 1 2)))))
  (test #t 'bundle? (linklet-bundle? c))
  (define l (hash-ref (linklet-bundle->hash c) 0))
  (test #t 'linklet? (linklet? l))
  (test '(linklet ((.get-syntax-literal!) (.set-transformer!))
             (b)
           (void)
           (define-values (b) (+ 1 2)) (void))
        'decompile (decompile-linklet l)))

(parameterize ([current-compile-target-machine #f])
  (define c (compile '(module m '#%kernel
                        (#%provide b)
                        (define-values (b) (+ 1 2))
                        (module sub '#%kernel))))
  (test #t 'directory? (linklet-directory? c))
  (define l (hash-ref (linklet-bundle->hash (hash-ref (linklet-directory->hash c) #f)) 0))
  (test #t 'linklet? (linklet? l))
  (test '(linklet ((.get-syntax-literal!) (.set-transformer!))
             (b)
           (void)
           (define-values (b) (+ 1 2)) (void))
        'decompile (decompile-linklet l)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
