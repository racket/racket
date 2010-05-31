#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide item%)

(defclass item% window%
  (inherit get-client-gtk)

  (super-new)

  (let ([client-gtk (get-client-gtk)])
    (connect-focus client-gtk)
    (connect-key-and-mouse client-gtk))

  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)
  (def/public-unimplemented command))




