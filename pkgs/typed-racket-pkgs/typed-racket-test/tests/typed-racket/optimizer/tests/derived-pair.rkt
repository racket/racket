#;#;
#<<END
TR opt: derived-pair.rkt 22:0 (caar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 22:0 (caar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 23:0 (cadr (cons 1 (cons 2 3))) -- pair
TR opt: derived-pair.rkt 23:0 (cadr (cons 1 (cons 2 3))) -- pair
TR opt: derived-pair.rkt 24:0 (cdar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 24:0 (cdar (cons (cons 1 2) 3)) -- pair
TR opt: derived-pair.rkt 25:0 (cddr (cons 1 (cons 2 3))) -- pair
TR opt: derived-pair.rkt 25:0 (cddr (cons 1 (cons 2 3))) -- pair
END
#<<END
1
2
2
3

END

#lang typed/racket #:optimize

(caar (cons (cons 1 2) 3))
(cadr (cons 1 (cons 2 3)))
(cdar (cons (cons 1 2) 3))
(cddr (cons 1 (cons 2 3)))
