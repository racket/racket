#;
(exn-pred 2)
#lang typed/racket


(define-struct: (T) Node ([v : T] [l : (BinTreeof t)] [r : (BinTreeof t)]))
(define-type (BinTreeof t) (U 'empty [Node t]))
