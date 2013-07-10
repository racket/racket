#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])

  (contract-eval
   `(define (counter)
      (let ([c 0])
        (case-lambda
          [() c]
          [(x) (set! c (+ c 1)) #t]))))

  (ctest/rewrite 1
                 tail-arrow
                 (let ([c (counter)])
                   (letrec ([f
                             (contract (-> any/c c)
                                       (λ (x) (if (zero? x) x (f (- x 1))))
                                       'pos
                                       'neg)])
                     (f 3))
                   (c)))
  
  (ctest/rewrite 1
                 tail-unconstrained-domain-arrow
                 (let ([c (counter)])
                   (letrec ([f
                             (contract (unconstrained-domain-> c)
                                       (λ (x) (if (zero? x) x (f (- x 1))))
                                       'pos
                                       'neg)])
                     (f 3))
                   (c)))
  
  (ctest/rewrite 2
                 tail-multiple-value-arrow
                 (let ([c (counter)])
                   (letrec ([f
                             (contract (-> any/c (values c c))
                                       (λ (x) (if (zero? x) (values x x) (f (- x 1))))
                                       'pos
                                       'neg)])
                     (f 3))
                   (c)))
  
  (ctest/rewrite 2
                 tail-arrow-star
                 (let ([c (counter)])
                   (letrec ([f
                             (contract (->* (any/c) () (values c c))
                                       (λ (x) (if (zero? x) (values x x) (f (- x 1))))
                                       'pos
                                       'neg)])
                     (f 3))
                   (c)))
  
  
  (ctest/rewrite 1
                 case->-regular
                 (let ([c (counter)])
                   (letrec ([f
                             (contract (case-> (-> any/c c)
                                               (-> any/c any/c c))
                                       (case-lambda
                                         [(x) (if (zero? x) x (f (- x 1)))]
                                         [(x y) (f x)])
                                       'pos
                                       'neg)])
                     (f 4 1))
                   (c)))
  
  (ctest/rewrite 1
                 case->-rest-args
                 (let ([c (counter)])
                   (letrec ([f
                             (contract (case-> (-> any/c #:rest any/c c)
                                               (-> any/c any/c #:rest any/c c))
                                       (case-lambda
                                         [(x) (f x 1)]
                                         [(x y . z) (if (zero? x) x (apply f (- x 1) y (list y y)))])
                                       'pos
                                       'neg)])
                     (f 4))
                   (c)))
  
  (ctest/rewrite '(1)
                 mut-rec-with-any/c
                 (let ()
                   (define f
                     (contract (-> number? any/c)
                               (lambda (x)
                                 (if (zero? x)
                                     (continuation-mark-set->list (current-continuation-marks)
                                                                  'tail-test)
                                     (with-continuation-mark 'tail-test x
                                       (g (- x 1)))))
                               'something-that-is-not-pos
                               'neg))
                   
                   (define g
                     (contract (-> number? any/c)
                               (lambda (x)
                                 (f x))
                               'also-this-is-not-pos
                               'neg))
                   
                   (f 3)))
  
  
  (test/pos-blame 
   'different-blame=>cannot-drop-check
   '((contract (-> integer? integer?)
               (λ (x)
                 ((contract (-> integer? integer?)
                            (λ (x) #f)
                            'pos
                            'neg)
                  x))
               'abc
               'def)
     5))

  (test/pos-blame 'free-vars-change-so-cannot-drop-the-check
                  '(let ()
                     (define f
                       (contract (->i ([x number?]) () [_ (x) (</c x)])
                                 (lambda (x)
                                   (cond
                                     [(= x 0) 1]
                                     [else (f 0)]))
                                 'pos
                                 'neg))
                     (f 10))))
