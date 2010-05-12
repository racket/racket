#lang typed/scheme

(: convert (Number -> Syntax))
(define (convert n) (datum->syntax #f n))

(: convert/loc (Number Syntax -> Syntax))
(define (convert/loc n loc) (datum->syntax #f n loc))

(: convert/prop (Number Syntax -> Syntax))
(define (convert/prop n prop) (datum->syntax #f n #f prop))

(: convert/cert (Number Syntax -> Syntax))
(define (convert/cert n cert) (datum->syntax #f n #f #f cert))

(convert 3)
(convert/loc 3 #'loc)
(convert/prop 3 #'prop)
(convert/cert 3 #'cert)
