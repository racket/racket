#lang racket 

(require 2htdp/batch-io rackunit)

(define url "http://www.ccs.neu.edu/home/matthias/HtDP2e/Files/")

(define local-machine-configuration "batch-io-xexpr-machine.xml")
(define remote-machine-configuration (string-append url "machine-configuration.xml"))

(check-true (url-exists? remote-machine-configuration))

(define xm0 (read-plain-xexpr local-machine-configuration))
(define xm1 (read-xexpr local-machine-configuration))
(define rxm0 (read-plain-xexpr/web remote-machine-configuration))
(define rxm1 (read-xexpr/web remote-machine-configuration))

(check-equal? xm0 rxm0 "compare remote and local machine config, plain XML")
(check-equal? xm1 rxm1 "compare remote and local machine config, XML with white space")

(define local-enumeration "batch-io-xexpr-enumeration.xml")
(define remote-enumeration (string-append url "enumeration.xml"))

(check-true (url-exists? remote-enumeration))

(define en0 (read-plain-xexpr local-enumeration))
(define en1 (read-xexpr local-enumeration))
(define ren0 (read-plain-xexpr/web remote-enumeration))
(define ren1 (read-xexpr/web remote-enumeration))

(check-equal? xm0 rxm0 "compare remote and local enumeration, plain XML")
(check-equal? xm1 rxm1 "compare remote and local enumeration, XML with white space")