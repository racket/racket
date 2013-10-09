#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/private/contracted/date-time
 #;
 [datetime->real ((U plot-time Date Date* SQL-Date SQL-Time SQL-Timestamp)
                  -> Real)]
 
 [datetime->real (plot-time -> Real)]
 [plot-time->seconds (plot-time -> Real)]
 [seconds->plot-time (Real -> plot-time)]
 )
