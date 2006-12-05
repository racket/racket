
(module sirmailr mzscheme
  (require (lib "unit.ss")
	   (lib "mred-sig.ss" "mred"))

  (require "sirmails.ss")

  (require (lib "imap-sig.ss" "net")
           (lib "smtp-sig.ss" "net")
           (lib "head-sig.ss" "net")
           (lib "base64-sig.ss" "net")
           (lib "mime-sig.ss" "net")
           (lib "qp-sig.ss" "net"))
  
  (require (lib "hierlist-sig.ss" "hierlist"))
  
  (require "utilr.ss"
           "optionr.ss"
           "readr.ss"
           "sendr.ss")

  ;; The sirmail@ unit implements a single reader window. See
  ;; "sirmail.ss" for its use: 
  (provide sirmail@)
  (define-compound-unit/infer sirmail@
    (import (ENV : sirmail:environment^)
            mred^
            imap^
            smtp^
            head^
            base64^
            mime^
            qp^
            hierlist^)
    (export)

    (link util@
          option@
          read@
          [() send@ ENV])))

