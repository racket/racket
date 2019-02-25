#lang racket/base
(require racket/place)

(define-values (i o) (place-channel))

(define (bounce v)
  (place-channel-put o v)
  (place-channel-get i))

(define (check a b)
  (unless (equal? a b)
    (error 'fail "different ~s ~s" a b)))

(check (vector 1 2 3)
       (bounce (chaperone-vector (vector 1 2 3)
                                 (lambda (v i val) val)
                                 (lambda (v i val) val))))

(check (hash 1 (vector 1 2 3))
       (bounce (hash 1
                     (chaperone-vector (vector 1 2 3)
                                       (lambda (v i val) val)
                                       (lambda (v i val) val)))))

(struct posn (x y) #:prefab)

(check (posn (vector 1 2 3) 4)
       (bounce (chaperone-struct (posn (vector 1 2 3) 4)
                                 posn-x (lambda (s val)
                                          (chaperone-vector val
                                                            (lambda (v i val) val)
                                                            (lambda (v i val) val))))))

(check (hash 'a (vector 1 2 3))
       (bounce (chaperone-hash (hash 'a (vector 1 2 3))
                               (lambda (ht k)
                                 (values k
                                         (lambda (ht k val)
                                           (chaperone-vector val
                                                             (lambda (v i val) val)
                                                             (lambda (v i val) val)))))
                               (lambda (ht k val) val)
                               (lambda (ht k) k)
                               (lambda (ht k) k))))
