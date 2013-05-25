#lang scheme/base

;; FIXME: there could be all sorts of mismatches between the R6RS
;;  definitions and those in `scheme/base'.

(provide
 char-upcase
 char-downcase
 char-titlecase
 char-foldcase
 char-ci=?
 char-ci<?
 char-ci>?
 char-ci<=?
 char-ci>=?
 char-alphabetic?
 char-numeric?
 char-whitespace?
 char-upper-case?
 char-lower-case?
 char-title-case?
 (rename-out [r6rs:char-general-category char-general-category])
 
 string-upcase
 string-downcase
 string-titlecase
 string-foldcase
 string-ci=?
 string-ci<?
 string-ci>?
 string-ci<=?
 string-ci>=?
 
 string-normalize-nfd
 string-normalize-nfkd
 string-normalize-nfc
 string-normalize-nfkc)

(define (r6rs:char-general-category ch)
  (hash-ref #hasheq((ll . Ll)
                    (lu . Lu)
                    (lt . Lt)
                    (lm . Lm)
                    (lo . Lo)
                    (mn . Mn)
                    (mc . Mc)
                    (me . Me)
                    (nl . Nl)
                    (no . No)
                    (nd . Nd)
                    (zl . Zl)
                    (zs . Zs)
                    (zp . Zp)
                    (pc . Pc)
                    (pd . Pd)
                    (ps . Ps)
                    (pe . Pe)
                    (pi . Pi)
                    (pf . Pf)
                    (po . Po)
                    (sm . Sm)
                    (sc . Sc)
                    (sk . Sk)
                    (so . So)
                    (cf . Cf)
                    (cn . Cn)
                    (co . Co)
                    (cc . Cc))
            (char-general-category ch)))
