#lang racket

;; this file defines the XID_start and XID_continue character sets,
;; the unicode standard for "start-of-identifier" and "continuing-
;; identifier" character sets.

;; there are a rather staggering number of discrete ranges required
;; to specify these sets. As of this writing, it's ~500 individual
;; ranges for XID_Start and about ~900 for XID_Continue.

;; for details, see
;; Unicode Standard Annex #31: Unicode Identifier and Pattern Syntax
;; http://www.unicode.org/reports/tr31/ 
;; I consulted version 17 (skimmed it, really).
;; the character set is actually specified by
;; Unicode Standard Annex #44: Unicode Character Database
;; http://www.unicode.org/reports/tr44/
;; the actual file from which the info is extracted is called
;; DerivedCoreProperties.txt, and it's wicked hard to get to from
;; the technical report....
;; to see the version of the text file, read its header; it should
;; be in this directory. As of this writing, it's 6.2.0, dating 
;; from 2012.

;; in order to make it easy to update, this file automatically
;; extracts the ranges from the text file.  This is pretty quick,
;; taking about 1/3 of a second on my laptop.

(require racket/runtime-path)

(provide xid-start-ranges
         xid-continue-ranges)

(define-runtime-path derived-core-properties 
  "./DerivedCoreProperties.txt")

;; string -> (listof (cons nat nat))
;; given a character class mentioned in the file, produce 
;; a list of character ranges for that class. 
(define (matching-lines char-class-name)
  (define line-regexp
    (pregexp (string-append "^([^;]*); " char-class-name)))
  (call-with-input-file derived-core-properties
    (lambda (ip)
      (for/list ([line (in-lines ip)]
                 #:when (regexp-match line-regexp line))
        (match-define (list _ char-range) 
          (regexp-match line-regexp line))
        (match char-range
          [(regexp 
            #px"^([[:xdigit:]]{4})\\.\\.([[:xdigit:]]{4})"
            (list _ pre post))
           (cons (hex->num pre) (hex->num post))]
          [(regexp #px"^([[:xdigit:]]{4})"
            (list _ only))
           (cons (hex->num only) (hex->num only))])))))

;; given a hex number string, return a number
(define (hex->num str)
  (string->number str 16))

(define xid-start-ranges (matching-lines "XID_Start"))
(define xid-continue-ranges (matching-lines "XID_Continue"))

