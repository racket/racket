#;
(
one-arg-arith.rkt line 40 col 1 - - - unary fixnum
one-arg-arith.rkt line 41 col 1 - - - unary float
one-arg-arith.rkt line 42 col 1 - / - unary float
one-arg-arith.rkt line 44 col 1 - + - unary number
one-arg-arith.rkt line 45 col 1 - + - unary number
one-arg-arith.rkt line 46 col 1 - + - unary number
one-arg-arith.rkt line 47 col 1 - * - unary number
one-arg-arith.rkt line 48 col 1 - * - unary number
one-arg-arith.rkt line 49 col 1 - * - unary number
one-arg-arith.rkt line 50 col 1 - min - unary number
one-arg-arith.rkt line 51 col 1 - min - unary number
one-arg-arith.rkt line 52 col 1 - min - unary number
one-arg-arith.rkt line 53 col 1 - max - unary number
one-arg-arith.rkt line 54 col 1 - max - unary number
one-arg-arith.rkt line 55 col 1 - max - unary number
-12
-12.0
0.23809523809523808
1
1.0
1267650600228229401496703205376
1
1.0
1267650600228229401496703205376
1
1.0
1267650600228229401496703205376
1
1.0
1267650600228229401496703205376
)

#lang typed/scheme
#:optimize



(- 12)
(- 12.0)
(/ 4.2)

(+ 1)
(+ 1.0)
(+ (expt 2 100))
(* 1)
(* 1.0)
(* (expt 2 100))
(min 1)
(min 1.0)
(min (expt 2 100))
(max 1)
(max 1.0)
(max (expt 2 100))
