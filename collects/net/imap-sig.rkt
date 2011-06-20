#lang racket/signature

imap-port-number
imap-connection?

imap-connect imap-connect*
imap-disconnect
imap-force-disconnect
imap-reselect
imap-examine
imap-noop
imap-status
imap-poll

imap-new?
imap-messages
imap-recent
imap-uidnext
imap-uidvalidity
imap-unseen
imap-reset-new!

imap-get-expunges
imap-pending-expunges?
imap-get-updates
imap-pending-updates?

imap-get-messages
imap-copy imap-append
imap-store imap-flag->symbol symbol->imap-flag
imap-expunge

imap-mailbox-exists?
imap-create-mailbox

imap-list-child-mailboxes
imap-mailbox-flags
imap-get-hierarchy-delimiter
