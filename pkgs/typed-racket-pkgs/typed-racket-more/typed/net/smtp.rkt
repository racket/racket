#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/smtp
  [smtp-send-message (String String (Listof String) String (Listof String) -> Void)]
  [smtp-sending-end-of-message (Parameter (-> Any))])

(provide smtp-send-message smtp-sending-end-of-message)
