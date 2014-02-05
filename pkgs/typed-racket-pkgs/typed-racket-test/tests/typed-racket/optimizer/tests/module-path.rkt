#;#;
#<<END
TR opt: module-path.rkt 3:0 (unless (module-path? 2) #f) -- dead then branch
END
#<<END
#t
#f
#f

END
#lang typed/racket #:optimize
#reader tests/typed-racket/optimizer/reset-port
(if (module-path? "a") #t #f)
(if (module-path? "\0") #t #f)
(unless (module-path? 2) #f)
