#lang typed/racket

;; Test for PR 13821
;;
;; Make sure type instantiation with struct names doesn't
;; loop forever

(struct: (X) union ([fst : (ISet X)] [snd : (ISet X)]) #:transparent)
(struct: (X) intersection ([fst : (ISet X)] [snd : (ISet X)]) #:transparent)
(struct: (X) complement ([fst : (ISet X)] [snd : (ISet X)]) #:transparent)
(define-type (ISet X) (U (union X) (intersection X) (complement X) (Setof X)))

;; This involves type instantiation and could loop forever
;; with the bug
(: iset->set (All (X) ((ISet X) -> (Setof X))))
(define (iset->set A)
  (union? A)
  (error 'unimplemented))

;; A simpler way to reproduce the problem
(union? 5)

