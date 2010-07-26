#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide item%)

(defclass item% window%
  (inherit get-client-gtk)

  (init-field [callback void])

  (super-new)

  (let ([client-gtk (get-client-gtk)])
    (connect-focus client-gtk)
    (connect-key-and-mouse client-gtk))

  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)

  (define/public (command e)
    (callback this e)))





