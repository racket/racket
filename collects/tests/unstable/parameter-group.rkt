#lang racket/base

(require rackunit unstable/parameter-group)

(define p1 (make-parameter 1))
(define p2 (make-parameter 2))

(define-parameter-group ps1 (p1 p2))

(check-true (parameter-group? ps1))
(check-equal? (ps1) (ps1-value 1 2))  
(check-equal? (parameterize/group () (ps1)) (ps1))
(check-equal? (parameterize*/group () (ps1)) (ps1))
(check-equal? (parameterize/group ([ps1  (ps1-value 10 20)]) (ps1))
              (ps1-value 10 20))
(check-equal? (parameterize/group ([p1 10] [p2 20]) (ps1))
              (ps1-value 10 20))
(check-equal? (parameterize/group ([ps1  (ps1-value 10 20)]) (list (p1) (p2)))
              (list 10 20))
(check-equal? (ps1) (ps1-value 1 2))

(check-exn exn:fail:contract? (λ () (ps1 (list 1 2 3))))
(check-exn exn:fail:contract? (λ () (parameterize ([ps1  (list 1 2 3)]) (ps1))))

(ps1 (ps1-value 10 20))

(check-equal? (ps1) (ps1-value 10 20))
(check-equal? (parameterize/group ([ps1  (ps1-value 1 2)]) (ps1))
              (ps1-value 1 2))
(check-equal? (parameterize/group ([p1 1] [p2 2]) (ps1))
              (ps1-value 1 2))
(check-equal? (parameterize/group ([ps1  (ps1-value 1 2)]) (list (p1) (p2)))
              (list 1 2))
(check-equal? (ps1) (ps1-value 10 20))

(p1 1)
(p2 2)

(define p3 (make-parameter 3))

(define-parameter-group ps2 (ps1 p3))

(check-equal? (ps2) (ps2-value (ps1-value 1 2) 3))
(check-equal? (parameterize/group ([ps2  (ps2-value (ps1-value 10 20) 30)]) (ps2))
              (ps2-value (ps1-value 10 20) 30))
(check-equal? (parameterize/group ([p1 10] [p2 20] [p3 30]) (ps2))
              (ps2-value (ps1-value 10 20) 30))
(check-equal? (parameterize/group ([ps2  (ps2-value (ps1-value 10 20) 30)]) (list (p1) (p2) (p3)))
              (list 10 20 30))
(check-equal? (ps2) (ps2-value (ps1-value 1 2) 3))

(ps2 (ps2-value (ps1-value 10 20) 30))

(check-equal? (ps2) (ps2-value (ps1-value 10 20) 30))
(check-equal? (parameterize/group ([ps2  (ps2-value (ps1-value 1 2) 3)]) (ps2))
              (ps2-value (ps1-value 1 2) 3))
(check-equal? (parameterize/group ([p1 1] [p2 2] [p3 3]) (ps2))
              (ps2-value (ps1-value 1 2) 3))
(check-equal? (parameterize/group ([ps2  (ps2-value (ps1-value 1 2) 3)]) (list (p1) (p2) (p3)))
              (list 1 2 3))
(check-equal? (ps2) (ps2-value (ps1-value 10 20) 30))

(p1 1)
(p2 2)
(p3 3)

(check-equal? (parameterize/group ([ps1  (ps1-value 10 20)]
                                   [ps2  (ps2-value (ps1) 30)])
                                  (ps2))
              (ps2-value (ps1-value 1 2) 30))

(check-equal? (parameterize*/group ([ps1  (ps1-value 10 20)]
                                    [ps2  (ps2-value (ps1) 30)])
                                   (ps2))
              (ps2-value (ps1-value 10 20) 30))

(check-equal? (ps1-value-p1 (ps1)) 1)
(check-equal? (ps1-value-p2 (ps1)) 2)
(check-equal? (ps1-value-p1 (ps2-value-ps1 (ps2))) 1)
(check-equal? (ps1-value-p2 (ps2-value-ps1 (ps2))) 2)
(check-equal? (ps2-value-p3 (ps2)) 3)
