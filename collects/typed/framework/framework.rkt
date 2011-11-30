#lang typed/racket/base

(require typed/private/utils typed/mred/mred)

(dt Style-List% (Class ()
                       ()
                       ([find-named-style
                         (String -> (Instance (Class ()
                                                     ()
                                                     ([get-font (-> (Instance Font%))]))))])))

(dt Racket:Text% (Class ()
                        ()
                        ([begin-edit-sequence (-> Void)]
                         [end-edit-sequence (-> Void)]
                         [lock (Boolean -> Void)]
                         [last-position (-> Number)]
                         [last-paragraph (-> Exact-Nonnegative-Integer)]
                         [delete (Number Number -> Void)]
                         [auto-wrap (Any -> Void)]
                         [paragraph-end-position (Number -> Natural)]
                         [paragraph-start-position (Number -> Natural)]
                         [get-start-position (-> Number)]
                         [get-end-position (-> Number)]
                         [insert (String Number Number -> Void)])))

(require/typed/provide
 framework/framework
 [preferences:set-default (Symbol Sexp (Any -> Boolean) -> Void)]
 [preferences:set (Symbol Sexp -> Void)]
 [editor:get-standard-style-list
  (-> (Instance Style-List%))]
 [racket:text% Racket:Text%]
 [gui-utils:ok/cancel-buttons
  ((Instance Horizontal-Panel%) ((Instance Button%) (Instance Event%) -> Void) ((Instance Button%) (Instance Event%) -> Void) -> (values Any Any))])

(require/typed/provide "prefs-contract.rkt"
                       [preferences:get-drracket:large-letters-font (-> (U #f (Pair String Integer)))])

(require (only-in "prefs-contract.rkt" preferences:get))
(provide preferences:get)

