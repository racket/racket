#lang racket/base

(require racket/place)
(provide start)

(define (start ch)
  (place-channel-get ch)
  (println "before custodian")
  (println "before gc")
  (collect-garbage) ;; makes it crash sooner
  (println "after gc")
  (define custodian (make-custodian))
  (current-memory-use custodian)
  (println "after custodian")
  (place-channel-put ch #t))

(module+ main
  (for ([_ (in-range 10)])
    (println "before place")
    (place-channel-put/get (dynamic-place '(file "/tmp/c.rkt") 'start) #t)
    (println "after place")))
