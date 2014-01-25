#lang typed/racket/base

(require racket/class
         typed/private/utils
         typed/mred/mred)

(define-type Style-List%
  (Class [find-named-style (String -> (Instance (Class [get-font (-> (Instance Font%))])))]))

(require/typed/provide
 framework/framework
 [preferences:set-default (Symbol Sexp (Any -> Boolean) -> Void)]
 [preferences:set (Symbol Sexp -> Void)]
 [editor:get-standard-style-list
  (-> (Instance Style-List%))]
 [racket:text% Text:Basic%]
 [gui-utils:ok/cancel-buttons
  ((Instance Horizontal-Panel%)
   ((Instance Button%) (Instance Event%) -> Void)
   ((Instance Button%) (Instance Event%) -> Void)
   -> (values Any Any))])

(require/typed/provide "prefs-contract.rkt"
                       [preferences:get-drracket:large-letters-font (-> (U #f (Pair String Integer)))])

(require (only-in "prefs-contract.rkt" preferences:get))
(provide preferences:get)

