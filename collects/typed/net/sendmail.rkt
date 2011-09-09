#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/sendmail
  [send-mail-message/port
   (String String (Listof String) (Listof String) (Listof String) String * -> Output-Port)]
  [send-mail-message
   (String String (Listof String) (Listof String) (Listof String) (Listof String) String * -> Output-Port)])

(provide send-mail-message/port send-mail-message #;no-mail-recipients)
