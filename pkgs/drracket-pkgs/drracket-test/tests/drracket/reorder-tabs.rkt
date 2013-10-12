#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/list
         racket/class
         rackunit)

(fire-up-drracket-and-run-tests 
 (λ ()
   (define drs (wait-for-drracket-frame))
   (send drs create-new-tab)
   (send drs create-new-tab)
   (define tabs (send drs get-tabs))

   (send drs reorder-tabs (reverse (range (length tabs))))
   (define new-tabs (send drs get-tabs))
   (check-equal? new-tabs (reverse tabs))

   (send drs reorder-tabs (reverse (range (length tabs))))
   (define new-tabs2 (send drs get-tabs))
   (check-equal? new-tabs2 tabs)
   ))
