#lang racket

;; created in response to pr 12857
;; make sure the name of a world is transmitted to the server 

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(define NAME 'ian-johnson)

(define c (make-custodian))

(define-values (_ n)
  (parameterize ((current-custodian c))
    (launch-many-worlds
     ;; --- world: 
     (big-bang 10
               (on-tick sub1)
               (to-draw (lambda (w) (empty-scene 200 200)))
               (name NAME)
               (register LOCALHOST))
     ;; --- universe: 
     (universe #f
               (on-new (lambda (u w) (make-bundle (iworld-name w) '() '())))
               (on-msg (lambda (u w m) (make-bundle u '() '())))
               (on-tick (lambda (u) (make-bundle u '() '())) 1 1)))))

(check-equal? n NAME)

(custodian-shutdown-all c)
