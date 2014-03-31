
#lang unstable/2d typed/racket/base
(require unstable/2d/match)

(: f : Integer Integer -> Integer)
(define (f x y)
  #2dmatch
  ╔════════════════╦═══╦════════════════════════╗
  ║   y            ║ 0 ║       #{b : Integer}   ║
  ║ x              ║   ║                        ║
  ╠════════════════╬═══╬════════════════════════╣
  ║  0             ║ 0 ║      0                 ║
  ╠════════════════╬═══╬════════════════════════╣
  ║#{a : Integer}  ║ 0 ║     (let loop ([a a]   ║
  ║                ║   ║                [b b])  ║
  ║                ║   ║        (loop a b))     ║
  ╚════════════════╩═══╩════════════════════════╝) 

(f 0 1)