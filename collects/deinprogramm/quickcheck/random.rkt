#lang scheme/base

(provide make-random-generator
	 random-generator-next
	 random-generator-split
	 random-integer
	 random-real)

(require srfi/9)

(define-syntax define-record-discloser
  (syntax-rules ()
    ((define-record-discloser ?:type ?discloser)
     (values))))

(require scheme/include)
(include "random.scm")
