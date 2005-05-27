;;;
;;; <receive.ss> ---- SRFI 8 Binding to Multiple Values port to PLT Scheme
;;; Time-stamp: <02/05/10 12:22:37 solsona>
;;;
;;; Usually, I would add a copyright notice, and the announce that
;;; this code is under the LGPL licence.  Nevertheless, I only did the
;;; port to PLT Scheme v200 (it was written for the 103), and here is
;;; the copyright notice, comments, and licence from the original
;;; source:
;;;
;;; oh well, there is no such comment.

(module receive mzscheme
  (provide receive)

  ;; (receive vars producer . body)
  (define-syntax receive
    (syntax-rules ()
		  ((receive ?vars ?producer . ?body)
		   (call-with-values (lambda () ?producer)
				     (lambda ?vars . ?body)))))
  )