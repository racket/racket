#;#;
#<<END
TR info: struct-constructor.rkt 6:7 foo -- struct constructor
END
""
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(provide (struct-out foo))

(struct: foo ())

(void (foo))
