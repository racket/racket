#;
(
TR opt: module-path.rkt 12:0 (unless (module-path? 2) #f) -- dead then branch
#t
#f
#f
)

#lang typed/racket #:optimize
(if (module-path? "a") #t #f)
(if (module-path? "\0") #t #f)
(unless (module-path? 2) #f)
