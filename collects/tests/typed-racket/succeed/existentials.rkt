#lang typed/racket

(: complicated Boolean)
(define complicated #f)

#|
(: poss-box (U Symbol (Boxof Symbol) (Sequenceof Symbol)))
(define poss-box 'three)

(ann (if (box? poss-box) (unbox poss-box) 'zero) Symbol)

(: poss-channel (U Symbol (Channelof Symbol) (Sequenceof Symbol)))
(define poss-channel 3)

;(ann (if (channel? poss-channel) (channel-get poss-channel) 0) Symbol)

(: poss-vector (U Symbol (Vector Symbol) (Sequenceof Symbol)))
(define poss-vector 3)

;(ann (if (vector? poss-vector) (vector-ref poss-vector 0) 0) Symbol)

|#


(: poss-mpair (U Symbol (MPairof Symbol Symbol) (Sequenceof Symbol)))
(define poss-mpair 'three3)

(ann (if (mpair? poss-mpair) (if complicated (mcar poss-mpair) (mcdr poss-mpair)) 'zero) Symbol)

#|

(: poss-thread-cell (U Symbol (Thread-Cellof Symbol) (Sequenceof Symbol)))
(define poss-thread-cell 3)

;(ann (if (thread-cell? poss-thread-cell) (thread-cell-ref poss-thread-cell) 0) Symbol)

(: poss-hash (U Symbol (HashTable Symbol Symbol) (Sequenceof Symbol)))
(define poss-hash 3)

#;
(ann (if (hash? poss-hash)
       (+ (first (hash-keys poss-hash))
          (hash-ref poss-hash 0))
       0)
     Symbol)
|#
