#;#;
#<<END
TR missed opt: pair.rkt 12:16 (cdr (cdr (cdr (cdr (list 1 2 3))))) -- car/cdr on a potentially empty list -- caused by: 12:21 (cdr (cdr (cdr (list 1 2 3))))
TR missed opt: pair.rkt 15:0 (mcar (values (ann (mlist 1) (MListof Byte)))) -- car/cdr on a potentially empty list -- caused by: 15:6 (values (ann (mlist 1) (MListof Byte)))
TR missed opt: pair.rkt 17:0 (mcdr (values (ann (mlist 1) (MListof Byte)))) -- car/cdr on a potentially empty list -- caused by: 17:6 (values (ann (mlist 1) (MListof Byte)))
TR missed opt: pair.rkt 21:0 (set-mcar! (values (ann (mlist 2) (MListof Byte))) 2) -- car/cdr on a potentially empty list -- caused by: 21:11 (values (ann (mlist 2) (MListof Byte)))
TR missed opt: pair.rkt 23:0 (set-mcdr! (values (ann (mlist 2) (MListof Byte))) (values (ann (mlist 2) (MListof Byte)))) -- car/cdr on a potentially empty list -- caused by: 23:11 (values (ann (mlist 2) (MListof Byte)))
TR missed opt: pair.rkt 29:17 (mcar (quote ())) -- car/cdr on a potentially empty list -- caused by: 29:23 (quote ())
TR missed opt: pair.rkt 30:17 (mcdr (quote ())) -- car/cdr on a potentially empty list -- caused by: 30:23 (quote ())
TR missed opt: pair.rkt 31:17 (set-mcar! (quote ()) 2) -- car/cdr on a potentially empty list -- caused by: 31:28 (quote ())
TR missed opt: pair.rkt 32:17 (set-mcdr! (ann (quote ()) (MListof Integer)) (ann (mlist 3) (MListof Integer))) -- car/cdr on a potentially empty list -- caused by: 32:33 (quote ())
TR missed opt: pair.rkt 6:0 (car (ann (list 1 2 3) (Listof Byte))) -- car/cdr on a potentially empty list -- caused by: 6:10 (list 1 2 3)
TR missed opt: pair.rkt 8:0 (cdr (ann (list 1 2 3) (Listof Byte))) -- car/cdr on a potentially empty list -- caused by: 8:10 (list 1 2 3)
TR opt: pair.rkt 10:0 (cdr (cdr (list 1 2 3))) -- pair
TR opt: pair.rkt 10:5 (cdr (list 1 2 3)) -- pair
TR opt: pair.rkt 11:0 (cdr (cdr (cdr (list 1 2 3)))) -- pair
TR opt: pair.rkt 11:10 (cdr (list 1 2 3)) -- pair
TR opt: pair.rkt 11:5 (cdr (cdr (list 1 2 3))) -- pair
TR opt: pair.rkt 12:21 (cdr (cdr (cdr (list 1 2 3)))) -- pair
TR opt: pair.rkt 12:26 (cdr (cdr (list 1 2 3))) -- pair
TR opt: pair.rkt 12:31 (cdr (list 1 2 3)) -- pair
TR opt: pair.rkt 16:0 (mcar (mlist 1 2 3)) -- pair
TR opt: pair.rkt 18:0 (mcdr (mlist 1 2 3)) -- pair
TR opt: pair.rkt 19:0 (mcdr (mcdr (mlist 1 2 3))) -- pair
TR opt: pair.rkt 19:6 (mcdr (mlist 1 2 3)) -- pair
TR opt: pair.rkt 20:0 (mcdr (mcdr (mcdr (mlist 1 2 3)))) -- pair
TR opt: pair.rkt 20:12 (mcdr (mlist 1 2 3)) -- pair
TR opt: pair.rkt 20:6 (mcdr (mcdr (mlist 1 2 3))) -- pair
TR opt: pair.rkt 22:0 (set-mcar! (mlist 2 3 4) 2) -- pair
TR opt: pair.rkt 25:0 (mcar (mcons 2 3)) -- pair
TR opt: pair.rkt 26:0 (mcdr (mcons 2 3)) -- pair
TR opt: pair.rkt 27:0 (set-mcar! (mcons 2 3) 3) -- pair
TR opt: pair.rkt 28:0 (set-mcdr! (mcons 2 3) 4) -- pair
TR opt: pair.rkt 7:0 (car (list 1 2 3)) -- pair
TR opt: pair.rkt 9:0 (cdr (list 1 2 3)) -- pair
END
#<<END
1
1
'(2 3)
'(2 3)
'(3)
'()
1
1
'()
(mcons 2 (mcons 3))
(mcons 3)
'()
2
3

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(require compatibility/mlist)

;; car/cdr can be optimized if they are guaranteed to be applied only to
;; non-empty lists. otherwise, we miss a potential optimization

(car (ann (list 1 2 3) (Listof Byte)))
(car (list 1 2 3)) ; non-empty list type, shouldn't be reported
(cdr (ann (list 1 2 3) (Listof Byte)))
(cdr (list 1 2 3))
(cdr (cdr (list 1 2 3)))
(cdr (cdr (cdr (list 1 2 3))))
(define (dummy) (cdr (cdr (cdr (cdr (list 1 2 3)))))) ; unsafe, so missed opt

;; similar for mpairs
(mcar (values (ann (mlist 1) (MListof Byte))))
(mcar (mlist 1 2 3))
(mcdr (values (ann (mlist 1) (MListof Byte))))
(mcdr (mlist 1 2 3))
(mcdr (mcdr (mlist 1 2 3)))
(mcdr (mcdr (mcdr (mlist 1 2 3))))
(set-mcar! (values (ann (mlist 2) (MListof Byte))) 2)
(set-mcar! (mlist 2 3 4) 2)
(set-mcdr! (values (ann (mlist 2) (MListof Byte))) (values (ann (mlist 2) (MListof Byte))))

(mcar (mcons 2 3))
(mcdr (mcons 2 3))
(set-mcar! (mcons 2 3) 3)
(set-mcdr! (mcons 2 3) 4)
(define (dummy2) (mcar '()))
(define (dummy3) (mcdr '()))
(define (dummy4) (set-mcar! '() 2))
(define (dummy5) (set-mcdr! (ann '() (MListof Integer)) (ann (mlist 3) (MListof Integer))))
