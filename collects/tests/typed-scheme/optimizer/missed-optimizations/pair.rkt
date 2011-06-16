#;
(
TR missed opt: pair.rkt 58:0 (car (ann (list 1 2 3) (Listof Byte))) -- car/cdr on a potentially empty list -- caused by: 58:10 (list 1 2 3)
TR opt: pair.rkt 59:1 car -- pair
TR missed opt: pair.rkt 60:0 (cdr (ann (list 1 2 3) (Listof Byte))) -- car/cdr on a potentially empty list -- caused by: 60:10 (list 1 2 3)
TR opt: pair.rkt 61:1 cdr -- pair
TR opt: pair.rkt 62:1 cdr -- pair
TR opt: pair.rkt 62:6 cdr -- pair
TR opt: pair.rkt 63:1 cdr -- pair
TR opt: pair.rkt 63:6 cdr -- pair
TR opt: pair.rkt 63:11 cdr -- pair
TR missed opt: pair.rkt 64:16 (cdr (cdr (cdr (cdr (list 1 2 3))))) -- car/cdr on a potentially empty list -- caused by: 64:21 (cdr (cdr (cdr (list 1 2 3))))
TR opt: pair.rkt 64:22 cdr -- pair
TR opt: pair.rkt 64:27 cdr -- pair
TR opt: pair.rkt 64:32 cdr -- pair
TR missed opt: pair.rkt 67:0 (mcar (ann (mlist 1) (MListof Byte))) -- mpair op on a potentially empty mlist -- caused by: 67:0 (mcar (ann (mlist 1) (MListof Byte)))
TR opt: pair.rkt 68:1 mcar -- mutable pair
TR missed opt: pair.rkt 69:0 (mcdr (ann (mlist 1) (MListof Byte))) -- mpair op on a potentially empty mlist -- caused by: 69:0 (mcdr (ann (mlist 1) (MListof Byte)))
TR opt: pair.rkt 70:1 mcdr -- mutable pair
TR opt: pair.rkt 71:1 mcdr -- mutable pair
TR opt: pair.rkt 71:7 mcdr -- mutable pair
TR opt: pair.rkt 72:1 mcdr -- mutable pair
TR opt: pair.rkt 72:7 mcdr -- mutable pair
TR opt: pair.rkt 72:13 mcdr -- mutable pair
TR missed opt: pair.rkt 73:0 (set-mcar! (ann (mlist 2) (MListof Byte)) 2) -- mpair op on a potentially empty mlist -- caused by: 73:0 (set-mcar! (ann (mlist 2) (MListof Byte)) 2)
TR opt: pair.rkt 74:1 set-mcar! -- mutable pair
TR missed opt: pair.rkt 75:0 (set-mcdr! (ann (mlist 2) (MListof Byte)) (ann (mlist 2) (MListof Byte))) -- mpair op on a potentially empty mlist -- caused by: 75:0 (set-mcdr! (ann (mlist 2) (MListof Byte)) (ann (mlist 2) (MListof Byte)))
TR opt: pair.rkt 77:1 mcar -- mutable pair
TR opt: pair.rkt 78:1 mcdr -- mutable pair
TR opt: pair.rkt 79:1 set-mcar! -- mutable pair
TR opt: pair.rkt 80:1 set-mcdr! -- mutable pair
TR missed opt: pair.rkt 81:17 (mcar (quote ())) -- mpair op on a potentially empty mlist -- caused by: 81:23 (quote ())
TR missed opt: pair.rkt 82:17 (mcdr (quote ())) -- mpair op on a potentially empty mlist -- caused by: 82:23 (quote ())
TR missed opt: pair.rkt 83:17 (set-mcar! (quote ()) 2) -- mpair op on a potentially empty mlist -- caused by: 83:28 (quote ())
TR missed opt: pair.rkt 84:17 (set-mcdr! (ann (quote ()) (MListof Integer)) (ann (mlist 3) (MListof Integer))) -- mpair op on a potentially empty mlist -- caused by: 84:33 (quote ())
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
(require racket/mpair)

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
