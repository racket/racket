#;#;
#<<END
TR opt: derived-pair.rkt 2:0 (caar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 2:0 (caar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 3:0 (cadr (cons 1 (cons 2 3))) -- pair
TR opt: derived-pair.rkt 3:0 (cadr (cons 1 (cons 2 3))) -- pair
TR opt: derived-pair.rkt 4:0 (cdar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 4:0 (cdar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 5:0 (cddr (cons 1 (cons 2 3))) -- pair
TR opt: derived-pair.rkt 5:0 (cddr (cons 1 (cons 2 3))) -- pair
END
#<<END
1
2
2
3

END
#lang typed/racket #:optimize
#reader tests/typed-racket/optimizer/reset-port

(caar (cons (cons 1 2) 3))
(cadr (cons 1 (cons 2 3)))
(cdar (cons (cons 1 2) 3))
(cddr (cons 1 (cons 2 3)))
