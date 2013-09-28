#;#;
#<<END
TR info: struct-constructor.rkt 15:7 foo -- struct constructor
END
#<<END
END


#lang typed/racket

(provide (struct-out foo))

(struct: foo ())

(void (foo))
