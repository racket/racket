#;
(
TR missed opt: pair.rkt 58:0 (car (ann (list 1 2 3) (Listof Byte))) -- car/cdr on a potentially empty list -- caused by: 58:10 (list 1 2 3)
TR opt: pair.rkt 59:0 (car (list 1 2 3)) -- pair
TR missed opt: pair.rkt 60:0 (cdr (ann (list 1 2 3) (Listof Byte))) -- car/cdr on a potentially empty list -- caused by: 60:10 (list 1 2 3)
TR opt: pair.rkt 61:0 (cdr (list 1 2 3)) -- pair
TR opt: pair.rkt 62:0 (cdr (cdr (list 1 2 3))) -- pair
TR opt: pair.rkt 62:5 (cdr (list 1 2 3)) -- pair
TR opt: pair.rkt 63:0 (cdr (cdr (cdr (list 1 2 3)))) -- pair
TR opt: pair.rkt 63:5 (cdr (cdr (list 1 2 3))) -- pair
TR opt: pair.rkt 63:10 (cdr (list 1 2 3)) -- pair
TR missed opt: pair.rkt 64:16 (cdr (cdr (cdr (cdr (list 1 2 3))))) -- car/cdr on a potentially empty list -- caused by: 64:21 (cdr (cdr (cdr (list 1 2 3))))
TR opt: pair.rkt 64:21 (cdr (cdr (cdr (list 1 2 3)))) -- pair
TR opt: pair.rkt 64:26 (cdr (cdr (list 1 2 3))) -- pair
TR opt: pair.rkt 64:31 (cdr (list 1 2 3)) -- pair
TR missed opt: pair.rkt 67:0 (mcar (ann (mlist 1) (MListof Byte))) -- car/cdr on a potentially empty list -- caused by: 67:0 (mcar (ann (mlist 1) (MListof Byte)))
TR opt: pair.rkt 68:0 (mcar (mlist 1 2 3)) -- pair
TR missed opt: pair.rkt 69:0 (mcdr (ann (mlist 1) (MListof Byte))) -- car/cdr on a potentially empty list -- caused by: 69:0 (mcdr (ann (mlist 1) (MListof Byte)))
TR opt: pair.rkt 70:0 (mcdr (mlist 1 2 3)) -- pair
TR opt: pair.rkt 71:0 (mcdr (mcdr (mlist 1 2 3))) -- pair
TR opt: pair.rkt 71:6 (mcdr (mlist 1 2 3)) -- pair
TR opt: pair.rkt 72:0 (mcdr (mcdr (mcdr (mlist 1 2 3)))) -- pair
TR opt: pair.rkt 72:6 (mcdr (mcdr (mlist 1 2 3))) -- pair
TR opt: pair.rkt 72:12 (mcdr (mlist 1 2 3)) -- pair
TR missed opt: pair.rkt 73:0 (set-mcar! (ann (mlist 2) (MListof Byte)) 2) -- car/cdr on a potentially empty list -- caused by: 73:0 (set-mcar! (ann (mlist 2) (MListof Byte)) 2)
TR opt: pair.rkt 74:0 (set-mcar! (mlist 2 3 4) 2) -- pair
TR missed opt: pair.rkt 75:0 (set-mcdr! (ann (mlist 2) (MListof Byte)) (ann (mlist 2) (MListof Byte))) -- car/cdr on a potentially empty list -- caused by: 75:0 (set-mcdr! (ann (mlist 2) (MListof Byte)) (ann (mlist 2) (MListof Byte)))
TR opt: pair.rkt 77:0 (mcar (mcons 2 3)) -- pair
TR opt: pair.rkt 78:0 (mcdr (mcons 2 3)) -- pair
TR opt: pair.rkt 79:0 (set-mcar! (mcons 2 3) 3) -- pair
TR opt: pair.rkt 80:0 (set-mcdr! (mcons 2 3) 4) -- pair
TR missed opt: pair.rkt 81:17 (mcar (quote ())) -- car/cdr on a potentially empty list -- caused by: 81:23 (quote ())
TR missed opt: pair.rkt 82:17 (mcdr (quote ())) -- car/cdr on a potentially empty list -- caused by: 82:23 (quote ())
TR missed opt: pair.rkt 83:17 (set-mcar! (quote ()) 2) -- car/cdr on a potentially empty list -- caused by: 83:28 (quote ())
TR missed opt: pair.rkt 84:17 (set-mcdr! (ann (quote ()) (MListof Integer)) (ann (mlist 3) (MListof Integer))) -- car/cdr on a potentially empty list -- caused by: 84:33 (quote ())
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
)

#lang typed/racket
(require compatibility/mpair)

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
(mcar (ann (mlist 1) (MListof Byte)))
(mcar (mlist 1 2 3))
(mcdr (ann (mlist 1) (MListof Byte)))
(mcdr (mlist 1 2 3))
(mcdr (mcdr (mlist 1 2 3)))
(mcdr (mcdr (mcdr (mlist 1 2 3))))
(set-mcar! (ann (mlist 2) (MListof Byte)) 2)
(set-mcar! (mlist 2 3 4) 2)
(set-mcdr! (ann (mlist 2) (MListof Byte)) (ann (mlist 2) (MListof Byte)))

(mcar (mcons 2 3))
(mcdr (mcons 2 3))
(set-mcar! (mcons 2 3) 3)
(set-mcdr! (mcons 2 3) 4)
(define (dummy2) (mcar '()))
(define (dummy3) (mcdr '()))
(define (dummy4) (set-mcar! '() 2))
(define (dummy5) (set-mcdr! (ann '() (MListof Integer)) (ann (mlist 3) (MListof Integer))))
