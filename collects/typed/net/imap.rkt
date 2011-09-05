#lang typed/racket/base

(require typed/private/utils)

(require/opaque-type IMAP-Connection imap-connection? net/imap)

(define-type-alias bstring (U String Bytes))

(require/typed/provide net/imap
  [imap-port-number (Number -> Void)]

  [imap-connect (String String String String -> (values IMAP-Connection Number Number))]
  [imap-connect* (Number Number String String String -> (values IMAP-Connection Number Number))]
  [imap-disconnect (IMAP-Connection -> Void)]
  [imap-force-disconnect (IMAP-Connection -> Void)]
  [imap-reselect (IMAP-Connection String -> (values Number Number))]
  [imap-examine (IMAP-Connection String -> (values Number Number))]
  [imap-noop (IMAP-Connection -> (values Number Number))]
  [imap-status (IMAP-Connection String (Listof Symbol) -> (Listof (Listof Number)))]
  [imap-poll (IMAP-Connection -> Void)]

  [imap-new? (IMAP-Connection -> Boolean)]
  [imap-messages (IMAP-Connection -> Number)]
  [imap-recent (IMAP-Connection -> Number)]
  [imap-uidnext (IMAP-Connection -> (Option Number))]
  [imap-uidvalidity (IMAP-Connection -> (Option Number))]
  [imap-unseen (IMAP-Connection -> (Option Number))]
  [imap-reset-new! (IMAP-Connection -> Void)]

  [imap-get-expunges (IMAP-Connection -> (Listof Number))]
  [imap-pending-expunges? (IMAP-Connection -> Boolean)]
  [imap-get-updates (IMAP-Connection -> (Listof (cons Number (Listof (Pair Any Any)))))]
  [imap-pending-updates? (IMAP-Connection -> Boolean)]

  [imap-get-messages
   (IMAP-Connection (Listof Number) Symbol -> (Listof (Listof (U Number String String (Listof Symbol)))))]
  [imap-copy  (IMAP-Connection (Listof Number) String -> Void)]
  [imap-append (IMAP-Connection String String -> Void)]
  [imap-store (IMAP-Connection Symbol (Listof Number) Symbol -> Void)]
  [imap-flag->symbol (Symbol -> Symbol)]
  [symbol->imap-flag (Symbol -> Symbol)]
  [imap-expunge (IMAP-Connection -> Void)]

  [imap-mailbox-exists? (IMAP-Connection String -> Boolean)]
  [imap-create-mailbox (IMAP-Connection String -> Void)]

  [imap-list-child-mailboxes
   (case-lambda (IMAP-Connection bstring -> (Listof (cons (Listof Symbol) (cons String '()))))
                (IMAP-Connection bstring (Option bstring) -> (Listof (List (Listof Symbol) String))))]
  [imap-mailbox-flags (IMAP-Connection String -> (Listof Symbol))]
  [imap-get-hierarchy-delimiter (IMAP-Connection -> String)])

(provide
 imap-connection?
 IMAP-Connection)
