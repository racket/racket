#;#;
#<<END
END
""
#lang typed/scheme #:optimize
#reader tests/typed-racket/optimizer/reset-port

;; will be imported by cross-module-struct2
(provide (struct-out x))
(define-struct: x ((x : Integer)))
