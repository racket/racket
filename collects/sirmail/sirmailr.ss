
(module sirmailr mzscheme
  (require (lib "unitsig.ss")
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
  (define sirmail@
    (compound-unit/sig
     (import (ENV : sirmail:environment^)
	     (MRED : mred^)
	     (IMAP : net:imap^)
	     (SMTP : net:smtp^)
	     (HEAD : net:head^)
	     (BASE64 : net:base64^)
	     (MIME : net:mime^)
	     (QP : net:qp^)
	     (HIER : hierlist^))
     (link [UTILS : sirmail:utils^
		  (util@
		   MRED
		   BASE64
		   QP)]
	   [OPTIONS : sirmail:options^
		    (option@
		     ENV
		     IMAP
                     MRED)]
	   [READ : sirmail:read^
		 (read@
		  OPTIONS ENV UTILS SEND
		  MRED IMAP SMTP HEAD BASE64 MIME QP HIER)]
	   [SEND : sirmail:send^
		 (send@
		  (ENV : (exit-sirmail)) UTILS OPTIONS READ ENV
		  MRED IMAP SMTP HEAD BASE64 QP HIER)])
     (export))))
