#lang scheme/base
(provide check check-results make-config
	 quickcheck quickcheck-results
	 check-result? result-arguments-list
	 choose-integer choose-real
	 choose-ascii-char choose-ascii-letter choose-printable-ascii-char choose-char
	 choose-list choose-vector choose-string choose-symbol
	 generator-unit generator-bind generator-sequence
	 sized choose-one-of choose-mixed choose-with-frequencies
	 arbitrary-boolean arbitrary-char arbitrary-ascii-char arbitrary-printable-ascii-char
	 arbitrary-integer arbitrary-natural arbitrary-rational arbitrary-real
	 arbitrary-mixed arbitrary-one-of
	 arbitrary-pair
	 arbitrary-list
	 arbitrary-vector
	 arbitrary-string
	 arbitrary-ascii-string arbitrary-printable-ascii-string
	 arbitrary-symbol
	 arbitrary-procedure
	 property
	 ==>
	 label
	 classify
	 trivial
	 collect
	 )
(require srfi/9
	 "random.ss")

; exceptions
(define (assertion-violation who msg . irritants)
  (apply error msg irritants))

; extended-ports
(define make-string-output-port open-output-string)
(define string-output-port-output get-output-string)

; sorting
(define (list-sort < lis)
  (sort lis <))

(require scheme/include)
(include "quickcheck.scm")
	 