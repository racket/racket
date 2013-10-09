#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/private/contracted/format
 
 [integer->superscript (Integer -> String)]
 
 [digits-for-range (case-> (Real Real -> Integer)
                           (Real Real Integer -> Integer)
                           (Real Real Integer Integer -> Integer))]
 
 [real->decimal-string* (case-> (Real Integer -> String)
                                (Real Integer Integer -> String))]
 
 [real->string/trunc (Real Integer -> String)]
 
 [real->plot-label (case-> (Real Integer -> Any)
                           (Real Integer Boolean -> Any))]
 
 [ivl->plot-label (case-> (ivl -> String)
                          (ivl Integer -> String))]
 
 [->plot-label (case-> (Any -> String)
                       (Any Integer -> String))]
 
 [parse-format-string (String -> (Listof (U String Symbol)))]
 
 [apply-formatter (All (T) ((Symbol T -> (U String #f)) (Listof (U String Symbol)) T
                                                        -> (Listof String)))]
 )
