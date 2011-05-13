#;
(
pair.rkt 36:0 (#%app car (#%app list (quote 1) (quote 2) (quote 3))) -- car/cdr on a potentially empty list -- caused by: 36:10 (#%app list (quote 1) (quote 2) (quote 3))
pair.rkt 38:0 (#%app cdr (#%app list (quote 1) (quote 2) (quote 3))) -- car/cdr on a potentially empty list -- caused by: 38:10 (#%app list (quote 1) (quote 2) (quote 3))
pair.rkt 42:16 (#%app cdr (#%app cdr (#%app cdr (#%app cdr (#%app list (quote 1) (quote 2) (quote 3)))))) -- car/cdr on a potentially empty list -- caused by: 42:21 (#%app cdr (#%app cdr (#%app cdr (#%app list (quote 1) (quote 2) (quote 3)))))
pair.rkt 45:0 (#%app mcar (#%app mcons (quote 1) null)) -- mpair op on a potentially empty mlist -- caused by: (no location) (#%app mcons (quote 1) null)
pair.rkt 47:0 (#%app mcdr (#%app mcons (quote 1) null)) -- mpair op on a potentially empty mlist -- caused by: (no location) (#%app mcons (quote 1) null)
pair.rkt 51:0 (#%app set-mcar! (#%app mcons (quote 2) null) (quote 2)) -- mpair op on a potentially empty mlist -- caused by: (no location) (#%app mcons (quote 2) null)
pair.rkt 53:0 (#%app set-mcdr! (#%app mcons (quote 2) null) (#%app mcons (quote 2) null)) -- mpair op on a potentially empty mlist -- caused by: (no location) (#%app mcons (quote 2) null)
pair.rkt 59:17 (#%app mcar (quote ())) -- mpair op on a potentially empty mlist -- caused by: 59:23 (quote ())
pair.rkt 60:17 (#%app mcdr (quote ())) -- mpair op on a potentially empty mlist -- caused by: 60:23 (quote ())
pair.rkt 61:17 (#%app set-mcar! (quote ()) (quote 2)) -- mpair op on a potentially empty mlist -- caused by: 61:28 (quote ())
pair.rkt 62:17 (#%app set-mcdr! (quote ()) (#%app mcons (quote 3) null)) -- mpair op on a potentially empty mlist -- caused by: 62:33 (quote ())
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
