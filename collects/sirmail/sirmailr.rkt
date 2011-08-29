(module sirmailr mzscheme
  (require mzlib/unit
           mred/mred-sig)

  (require "sirmails.rkt")

  (require net/imap
           net/smtp
           net/head
           net/base64
           net/mime
           net/qp)
  
  (require mrlib/hierlist/hierlist-sig)
  
  (require "utilr.rkt"
           "optionr.rkt"
           "readr.rkt"
           "sendr.rkt")

  ;; The sirmail@ unit implements a single reader window. See
  ;; "sirmail.rkt" for its use: 
  (provide sirmail@)
  (define-compound-unit/infer sirmail@
    (import (ENV : sirmail:environment^)
            mred^
            hierlist^)
    (export)

    (link util@
          option@
          read@
          [() send@ ENV])))

