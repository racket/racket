
(module sirmailr mzscheme
  (require mzlib/unit
	   mred/mred-sig)

  (require "sirmails.ss")

  (require net/imap-sig
           net/smtp-sig
           net/head-sig
           net/base64-sig
           net/mime-sig
           net/qp-sig)
  
  (require mrlib/hierlist/hierlist-sig)
  
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

