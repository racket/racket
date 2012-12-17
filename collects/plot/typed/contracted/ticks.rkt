#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/contracted/ticks
 [24h-descending-date-ticks-formats  (Listof String)]
 [12h-descending-date-ticks-formats  (Listof String)]
 [24h-descending-time-ticks-formats  (Listof String)]
 [12h-descending-time-ticks-formats  (Listof String)]
 [us-currency-scales  (Listof String)]
 [uk-currency-scales  (Listof String)]
 [eu-currency-scales  (Listof String)]
 [us-currency-formats  (Listof String)]
 [uk-currency-formats  (Listof String)]
 [eu-currency-formats  (Listof String)]
 
 [ticks-default-number (Parameterof Integer Positive-Integer)]
 [date-ticks-formats (Parameterof (Listof String))]
 [time-ticks-formats (Parameterof (Listof String))]
 [currency-ticks-scales (Parameterof (Listof String))]
 [currency-ticks-formats (Parameterof (List String String String))]
 
 [no-ticks-layout Ticks-Layout]
 [no-ticks-format Ticks-Format]
 [no-ticks ticks]
 
 [ticks-mimic ((-> ticks) -> ticks)]
 [ticks-scale (ticks invertible-function -> ticks)]
 [ticks-add (case-> (ticks (Listof Real) -> ticks)
                    (ticks (Listof Real) Boolean -> ticks))]
 
 [linear-scale (case-> (Real -> invertible-function)
                       (Real Real -> invertible-function))]
 
 [linear-ticks-layout
  ([#:number Integer] [#:base Integer] [#:divisors (Listof Integer)] -> Ticks-Layout)]
 [linear-ticks-format (-> Ticks-Format)]
 [linear-ticks ([#:number Integer] [#:base Integer] [#:divisors (Listof Integer)] -> ticks)]
 
 [log-ticks-layout ([#:number Integer] [#:base Integer] -> Ticks-Layout)]
 [log-ticks-format ([#:base Integer] -> Ticks-Format)]
 [log-ticks ([#:number Integer] [#:base Integer] -> ticks)]

 [date-ticks-layout ([#:number Integer] -> Ticks-Layout)]
 [date-ticks-format ([#:formats (Listof String)] -> Ticks-Format)]
 [date-ticks ([#:number Integer] [#:formats (Listof String)] -> ticks)]
 
 [time-ticks-layout ([#:number Integer] -> Ticks-Layout)]
 [time-ticks-format ([#:formats (Listof String)] -> Ticks-Format)]
 [time-ticks ([#:number Integer] [#:formats (Listof String)] -> ticks)]

 [bit/byte-ticks-format ([#:size (U 'byte 'bit)] [#:kind (U 'CS 'SI)] -> Ticks-Format)]
 [bit/byte-ticks ([#:number Integer] [#:size (U 'byte 'bit)] [#:kind (U 'CS 'SI)] -> ticks)]
 
 [fraction-ticks-format ([#:base Integer] [#:divisors (Listof Integer)] -> Ticks-Format)]
 [fraction-ticks ([#:base Integer] [#:divisors (Listof Integer)] -> ticks)]

 [currency-ticks-format ([#:kind (U String Symbol)]
                         [#:scales (Listof String)]
                         [#:formats (List String String String)]
                         -> Ticks-Format)]
 [currency-ticks ([#:number Integer]
                  [#:kind (U String Symbol)]
                  [#:scales (Listof String)]
                  [#:formats (List String String String)]
                  -> Ticks-Format)]
 
 [contour-ticks (ticks Real Real Contour-Levels Boolean -> (Listof tick))]
 
 [format-tick-labels  (ticks Real Real (Listof Real) -> (Listof String))]
 )
