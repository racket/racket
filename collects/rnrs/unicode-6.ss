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
 char-general-category
 
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
