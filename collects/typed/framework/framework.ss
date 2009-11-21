#lang typed-scheme
             
(require typed/private/utils
         (only-in typed/mred/mred Font%))

(dt Style-List% (Class () 
                       ()
                       ([find-named-style 
                         (String -> (Instance (Class ()
                                                     () 
                                                     ([get-font (-> (Instance Font%))]))))])))

(dt Scheme:Text% (Class ()
                        ()
                        ([begin-edit-sequence (-> Void)]
                         [end-edit-sequence (-> Void)]
                         [lock (Boolean -> Void)]
                         [last-position (-> Number)]
                         [last-paragraph (-> Number)]
                         [delete (Number Number -> Void)]
                         [auto-wrap (Any -> Void)]
                         [paragraph-end-position (Number -> Natural)]
                         [paragraph-start-position (Number -> Natural)]
                         [get-start-position (-> Number)]
                         [get-end-position (-> Number)]
                         [insert (String Number Number -> Void)])))

(require/typed/provide framework/framework
                       [preferences:set-default (Symbol Any Any -> Void)]
                       [preferences:set (Symbol Any -> Void)]
                       [editor:get-standard-style-list 
                        (-> (Instance Style-List%))]
                       [scheme:text% Scheme:Text%]
                       [gui-utils:ok/cancel-buttons (Any (Any Any -> Any) (Any Any -> Any) -> (values Any Any))])

(require/typed/provide "prefs-contract.ss"
                       [preferences:get-drscheme:large-letters-font (-> (U #f (Pair String Number)))])

(require (only-in "prefs-contract.ss" preferences:get))
(provide preferences:get)

