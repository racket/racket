#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  
  (test/spec-passed
   'continuation-mark-key/c-fo-1
   '(contract (continuation-mark-key/c string?)
              (make-continuation-mark-key)
              'pos 'neg))
  
  (test/pos-blame
   'continuation-mark-key/c-fo-2
   '(contract (continuation-mark-key/c string?) 5 'pos 'neg))
  
  (test/neg-blame
   'continuation-mark-key/c-ho-1
   '(let ([mark (contract (continuation-mark-key/c number?)
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark "bad"
        42)))
  
  (test/spec-passed
   'continuation-mark-key/c-ho-2
   '(let ([mark (contract (continuation-mark-key/c number?)
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark 5
        (continuation-mark-set-first
         (current-continuation-marks) mark))))
  
  (test/neg-blame
   'continuation-mark-key/c-ho-3
   '(let ([mark (contract (continuation-mark-key/c number?)
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark "bad"
        (continuation-mark-set-first
         (current-continuation-marks) mark))))
  
  (test/neg-blame
   'continuation-mark-key/c-ho-4
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (-> (continuation-mark-key/c (-> number? number?))
                                      number?)
                                  number?)
                              (lambda (f)
                                (with-continuation-mark mark (lambda (x) (+ x 1))
                                  (f mark)))
                              'pos
                              'neg)])
      (do-mark
       (lambda (mark)
         ((continuation-mark-set-first
           (current-continuation-marks) mark)
          "bad")))))
  
  (test/pos-blame
   'continuation-mark-key/c-ho-5
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (-> (continuation-mark-key/c (-> number? number?))
                                      number?)
                                  number?)
                              (lambda (f)
                                (with-continuation-mark mark (lambda (x) "bad")
                                  (f mark)))
                              'pos
                              'neg)])
      (do-mark
       (lambda (mark)
         ((continuation-mark-set-first
           (current-continuation-marks) mark)
          0)))))
  
  (test/spec-passed
   'continuation-mark-key/c-ho-6
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (-> (continuation-mark-key/c (-> number? number?))
                                      number?)
                                  number?)
                              (lambda (f)
                                (with-continuation-mark mark (lambda (x) (+ x 1))
                                  (f mark)))
                              'pos
                              'neg)])
      (do-mark
       (lambda (mark)
         ((continuation-mark-set-first
           (current-continuation-marks) mark)
          0)))))
  
  (test/neg-blame
   'continuation-mark-key/c-ho-7
   '(let ([mark (contract (continuation-mark-key/c (-> number? number?))
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark (lambda (x) "bad")
        ((continuation-mark-set-first
          (current-continuation-marks) mark)
         5))))
  
  (test/spec-passed
   'continuation-mark-key/c-ho-8
   '(let ([mark (contract (continuation-mark-key/c (-> number? number?))
                          (make-continuation-mark-key)
                          'pos
                          'neg)])
      (with-continuation-mark mark (lambda (x) (+ x 1))
        ((continuation-mark-set-first
          (current-continuation-marks) mark)
         0))))
  
  (test/pos-blame
   'continuation-mark-key/c-ho-9
   '(let* ([mark (make-continuation-mark-key)]
           [do-mark (contract (-> (continuation-mark-key/c (-> number? number?))
                                  number?)
                              (lambda (mark)
                                ((continuation-mark-set-first
                                  (current-continuation-marks) mark)
                                 "bad"))
                              'pos
                              'neg)])
      (with-continuation-mark mark (lambda (x) (+ x 1))
        (do-mark mark))))
  
  (test/pos-blame
   'continuation-mark-key/c-ho-10
   '(let* ([mark (make-continuation-mark-key)]
           [ctc-mark (contract (continuation-mark-key/c number?)
                               mark
                               'pos
                               'neg)])
      (with-continuation-mark mark "not a number"
        (+ 1 (continuation-mark-set-first #f ctc-mark)))))
  
  (test/spec-passed
   'continuation-mark-key/c-ho-11
   '(let* ([mark (make-continuation-mark-key)]
           [ctc-mark (contract (continuation-mark-key/c number?)
                               mark
                               'pos
                               'neg)])
      (continuation-mark-set-first #f ctc-mark)))
  
  (test/spec-passed/result
   'continuation-mark-key/c-has-contract
   '(let* ([mark (make-continuation-mark-key)]
           [ctc-mark (contract (continuation-mark-key/c number?)
                               mark
                               'pos
                               'neg)])
      (has-contract? ctc-mark))
   #t))