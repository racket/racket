#;#;
#<<END
TR info: struct-constructor.rkt 14:7 foo -- struct constructor
END
""


#lang typed/racket

(provide (struct-out foo))

(struct: foo ())

(void (foo))
