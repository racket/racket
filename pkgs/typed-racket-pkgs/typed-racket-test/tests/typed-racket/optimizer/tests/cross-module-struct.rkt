#;#;
#<<END
END
""

#lang typed/scheme #:optimize

;; will be imported by cross-module-struct2
(provide (struct-out x))
(define-struct: x ((x : Integer)))
