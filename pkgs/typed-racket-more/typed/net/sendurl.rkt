#lang typed/racket/base
(require/typed net/sendurl
               [send-url (String -> Void)]
               [unix-browser-list (Listof Symbol)]
               [browser-preference? (String -> Boolean)]
               [external-browser  (-> (U Symbol #f (Pair String String)))])

(provide send-url unix-browser-list browser-preference? external-browser)
