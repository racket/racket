(require "../client.ss"
         (lib "serialize.ss"))
 
(module m08 "../persistent-interaction.ss"
  (define (id x) x)
  
  (define (gn which)
    (cadr
     (send/suspend
      (lambda (k)
        (let ([ignore (printf "Please send the ~a number.~n" which)])
          k)))))
  
  (let ([ignore (start-interaction car)])
    (letrec ([f (let ([n (gn "first")])
                  (lambda (m) (+ n m)))]
             [g (let ([n (gn "second")])
                  (lambda (m) (+ n (f m))))])
      (let ([result (g (gn "third"))])
        (let ([ignore (printf "The answer is: ~s~n" result)])
          result)))))

(require m08)

;; trace *without* serialization
(define k0 (dispatch-start 'foo))
(define k1 (dispatch (list k0 1)))
(serialize k1)
(define k2 (dispatch (list k1 2)))
(serialize k1)
(= 6 (dispatch (list k2 3)))
(= 9 (dispatch (list k2 6)))
(serialize k2)
(define k1.1 (dispatch (list k0 -1)))
(define k2.1 (dispatch (list k1.1 -2)))
(zero? (dispatch (list k2.1 3)))
(= 6 (dispatch (list k2 3)))
(serialize k2)
(serialize k1)

;; trace *with* serialization
(define k0 (serialize (dispatch-start 'foo)))
(define k1 (serialize (dispatch (list (deserialize k0) 1))))
(define k2 (serialize (dispatch (list (deserialize k1) 2))))
(= 6 (dispatch (list (deserialize k2) 3)))
(= 9 (dispatch (list (deserialize k2) 6)))
k2
(define k1.1 (serialize (dispatch (list (deserialize k0) -1))))
(define k2.1 (serialize (dispatch (list (deserialize k1.1) -2))))
(zero? (dispatch (list (deserialize k2.1) 3)))
(= 6 (dispatch (list (deserialize k2) 3)))
k2