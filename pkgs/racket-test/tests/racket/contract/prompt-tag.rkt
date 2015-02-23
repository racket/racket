#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  (test/spec-passed
   'prompt-tag/c-fo-1
   '(contract (prompt-tag/c string?)
              (make-continuation-prompt-tag)
              'pos 'neg))
  
  (test/pos-blame
   'prompt-tag/c-fo-2
   '(contract (prompt-tag/c string?) 5 'pos 'neg))
  
  (test/spec-passed
   'prompt-tag/c-ho-1
   '(let ([pt (contract (prompt-tag/c number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
       (λ () (abort-current-continuation pt 3))
       pt
       (λ (x) (+ x 1)))))
  
  (test/neg-blame
   'prompt-tag/c-ho-2
   '(let ([pt (contract (prompt-tag/c string?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
       (λ () (abort-current-continuation pt 3))
       pt
       (λ (x) (+ x 1)))))
  
  (test/neg-blame
   'prompt-tag/c-ho-3
   '(let ([pt (contract (prompt-tag/c (-> string? number?))
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
       (λ () (abort-current-continuation pt (λ (x) 5)))
       pt
       (λ (x) (x 8)))))
  
  (test/neg-blame
   'prompt-tag/c-ho-4
   '(let ([pt (contract (prompt-tag/c (-> string? number?))
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
       (λ () (abort-current-continuation pt (λ (x) "bad")))
       pt
       (λ (x) (x "potato")))))
  
  (test/pos-blame
   'prompt-tag/c-ho-5
   '(let* ([pt (make-continuation-prompt-tag)]
           [do-prompt (contract
                       (-> (-> (prompt-tag/c (-> number? number?))
                               any)
                           number?)
                       (λ (f) (call-with-continuation-prompt
                               (λ () (f pt))
                               pt
                               (λ (f) (f "bad"))))
                       'pos
                       'neg)])
      (do-prompt (λ (pt)
                   (abort-current-continuation pt (λ (v) (+ v 1)))))))
  
  (test/spec-failed
   'prompt-tag/c-ho-5
   '(let* ([pt (make-continuation-prompt-tag)]
           [do-prompt (contract
                       (-> (-> (prompt-tag/c (-> number? number?))
                               any)
                           number?)
                       (λ (f) (call-with-continuation-prompt
                               (λ () (f pt))
                               pt
                               (λ (f) (f 0))))
                       'A
                       'B)]
           [do-prompt2 (contract
                        (-> (-> (prompt-tag/c (-> string? number?))
                                any)
                            number?)
                        do-prompt
                        'B
                        'C)])
      (do-prompt2
       (λ (pt) (abort-current-continuation pt (λ (v) (+ v 1))))))
   "B")
  
  (test/neg-blame
   'prompt-tag/c-ho-6
   '(let ([pt (contract (prompt-tag/c string? number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
       (λ () (abort-current-continuation pt 3 "bad"))
       pt
       (λ (x y) (values x y)))))
  
  (test/spec-passed
   'prompt-tag/c-ho-7
   '(let ([pt (contract (prompt-tag/c string? number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (call-with-continuation-prompt
       (λ () (abort-current-continuation pt "good" 5))
       pt
       (λ (x y) (values x y)))))
  
  (test/spec-passed
   'prompt-tag/c-call/cc-1
   '(let* ([pt (contract (prompt-tag/c string?
                                       #:call/cc string?)
                         (make-continuation-prompt-tag)
                         'pos
                         'neg)]
           [abort-k (call-with-continuation-prompt
                     (λ () (call/cc (λ (k) k) pt))
                     pt)])
      (call-with-continuation-prompt
       (λ () (abort-k "ok"))
       pt
       (λ (s) (string-append s "post")))))
  
  (test/spec-passed
   'prompt-tag/c-call/cc-2
   '(let* ([pt (contract (prompt-tag/c string?
                                       #:call/cc (values string? integer?))
                         (make-continuation-prompt-tag)
                         'pos
                         'neg)]
           [abort-k (call-with-continuation-prompt
                     (λ () (call/cc (λ (k) k) pt))
                     pt)])
      (call-with-continuation-prompt
       (λ () (abort-k "ok" 5))
       pt
       (λ (s n) (string-append s "post")))))
  
  (test/neg-blame
   'prompt-tag/c-call/cc-2
   '(letrec ([pt (make-continuation-prompt-tag)]
             [do-test (λ ()
                        (+ 1
                           (call-with-continuation-prompt
                            (lambda ()
                              (+ 1 (abort-k 1)))
                            pt)))]
             [cpt (contract (prompt-tag/c #:call/cc number?)
                            pt
                            'pos
                            'neg)]
             [abort-k (call-with-continuation-prompt
                       (λ ()
                         (let ([v (call/cc (lambda (k) k) cpt)])
                           (if (procedure? v)
                               v
                               (format "~a" v))))
                       pt)])
      (do-test)))
  
  (test/spec-passed/result
   'prompt-tag/c-has-contract
   '(let ([pt (contract (prompt-tag/c string? number?)
                        (make-continuation-prompt-tag)
                        'pos
                        'neg)])
      (has-contract? pt))
   #t))
