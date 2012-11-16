#lang typed/racket/base

(require (only-in "mpfr.rkt" consts 0ary-funs)
         "bigfloat-mpfr.rkt"
         "utils.rkt")

(req/prov-uniform-collection "mpfr.rkt" consts (Promise Bigfloat))
(req/prov-uniform-collection "mpfr.rkt" 0ary-funs (-> Bigfloat))
