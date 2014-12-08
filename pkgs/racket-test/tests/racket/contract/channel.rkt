#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/spec-passed
   'channel/c1
   '(contract (channel/c symbol?)
              (make-channel)
              'pos
              'neg))

  (test/spec-passed
   'channel/c1b
   '(let ([ch (contract (channel/c symbol?)
                        (make-channel)
                        'pos
                        'neg)])
      (thread (λ () (channel-get ch)))
      (channel-put ch 'x)))

  (test/neg-blame
   'channel/c1c
   '(let ([ch (contract (channel/c symbol?)
                        (make-channel)
                        'pos
                        'neg)])
      (thread (λ () (channel-get ch)))
      (channel-put ch 5)))

  (test/neg-blame
   'channel/c1d
   '(let ([ch (contract (channel/c symbol?)
                        (make-channel)
                        'pos
                        'neg)])
      (thread (λ () (channel-get ch)))
      (channel-put ch 5)))

  (test/neg-blame
   'channel/c1e
   '(let ([ch (contract (channel/c symbol?)
                        (make-channel)
                        'pos
                        'neg)])
      (channel-put ch 5)))

  (test/spec-passed
   'channel/c2
   '(contract (channel/c symbol?)
              (let ([ch (make-channel)])
                (thread (λ () (channel-put ch 'x)))
                ch)
              'pos
              'neg))

  (test/spec-passed
   'channel/c2b
   '(contract (channel/c symbol?)
              (let ([ch (make-channel)])
                (thread (λ () (channel-put ch 3)))
                ch)
              'pos
              'neg))

  (test/pos-blame
   'channel/c3
   '(channel-get
     (contract (channel/c symbol?)
               (let ([ch (make-channel)])
                 (thread (λ () (channel-put ch 3)))
                 ch)
               'pos
               'neg)))

  (test/pos-blame
   'channel/c4
   '(contract (channel/c symbol?)
              "not a channel"
              'pos
              'neg))

  (test/pos-blame
   'channel/c5
   '(channel-get
     (contract (channel/c symbol?)
               (let ([ch (make-channel)])
                 (thread (λ () (channel-put ch 3)))
                 ch)
               'pos
               'neg)))

  (test/spec-passed
   'channel/c6
   '((channel-get
      (contract (channel/c (-> number? number?))
                (let ([ch (make-channel)])
                  (thread (λ () (channel-put ch (λ (x) x))))
                  ch)
                'pos
                'neg))
     5))

  (test/pos-blame
   'channel/c6b
   '((channel-get
      (contract (channel/c (-> number? number?))
                (let ([ch (make-channel)])
                  (thread (λ () (channel-put ch (λ (x) 'bad))))
                  ch)
                'pos
                'neg))
     5))

  (test/neg-blame
   'channel/c6c
   '((channel-get
      (contract (channel/c (-> number? number?))
                (let ([ch (make-channel)])
                  (thread (λ () (channel-put ch (λ (x) 3))))
                  ch)
                'pos
                'neg))
     'bad)))
