(require "../client.ss")

;; ****************************************
;; ****************************************
;; BASIC TESTS

(module m00 "../interaction.ss"
  (define (id x) x)
  (id (start-interaction id)))

(require m00)
(= 7 (dispatch-start 7))
(= 8 (dispatch-start 8))

(module m01 "../interaction.ss"
  (define (id x) x)
  (+ (* 1 2) (* 3 4) (start-interaction id)))

(require m01)
(= 14 (dispatch-start 0))
(= 20 (dispatch-start 6))

;; start-interaction may be called mutitple times
;; each call overwrites the previous interaction
;; continuation with the latest one.
(module m02 "../interaction.ss"
  (define (id x) x)
  (+ (start-interaction id)
     (start-interaction id)))

(require m02)
(void? (dispatch-start 1))
(= 3 (dispatch-start 2))
(= 0 (dispatch-start -1))

;; ****************************************
;; ****************************************
;; TESTS INVOLVING CALL/CC

(module m03 "../interaction.ss"
  (define (f x)
    (let/cc k
      (+ 2 4 (k 3) 6 8)))
  (f (start-interaction (lambda (x) x))))

(require m03)
(= 3 (dispatch-start 'foo))
(= 3 (dispatch-start 7))

;; in the following test, if you modify
;; resume to print the "stack" you will
;; see that this is not tail recursive
(module m04 "../interaction.ss"
  (define (mult ln)
    (let/cc k
      (cond
        [(null? ln) 1]
        [(zero? (car ln)) (k 0)]
        [else
         (* (car ln)
            (mult (cdr ln)))])))
  
  (mult (start-interaction (lambda (x) x))))

(require m04)
(= 0 (dispatch-start (list 1 2 3 4 5 6 7 0 8 9)))
(= 120 (dispatch-start (list 1 2 3 4 5)))

;; this version captures the continuation
;; outside the recursion and should be tail
;; recursive. A "stack trace" reveals this
;; as expected.
(module m05 "../interaction.ss"
  (provide mult)
  
  (define (mult ln)
    (let/cc escape
      (mult/escape escape ln)))
  
  (define (mult/escape escape ln)
    (cond
      [(null? ln) 1]
      [(zero? (car ln)) (escape 0)]
      [else
       (* (car ln)
          (mult/escape escape (cdr ln)))]))
  
  (mult (start-interaction (lambda (x) x))))

(require m05)
(= 0 (dispatch-start (list 1 2 3 0 4 5 6)))
(= 120 (dispatch-start (list 1 2 3 4 5)))

;; ****************************************
;; ****************************************
;; TESTS INVOLVING send/suspend


(module table01 mzscheme
  (provide store-k
           lookup-k)
  
  (define the-table (make-hash-table))
  
  (define (store-k k)
    (let ([key (string->symbol (symbol->string (gensym 'key)))])
      (hash-table-put! the-table key k)
      key))
  
  (define (lookup-k key-pair)
    (printf "key-pair = ~s~n" key-pair)
    (hash-table-get the-table (car key-pair) (lambda () #f))))

(module m06 "../interaction.ss"
  (require table01)
  
  (define (gn which)
    (cadr
     (send/suspend
      (lambda (k)
        (let ([ignore (printf "Please send the ~a number.~n" which)])
          (store-k k))))))
  
  (let ([ignore (start-interaction lookup-k)])
    (let ([result (+ (gn "first") (gn "second"))])
      (let ([ignore (printf "The answer is: ~s~n" result)])
        result))))

(require m06)

(let* ([first-key (dispatch-start 'foo)]
       [second-key (dispatch `(,first-key 1))]
       [third-key (dispatch `(,first-key -7))])
  (values
   (= 3 (dispatch `(,second-key 2)))
   (= 4 (dispatch `(,second-key 3)))
   (zero? (dispatch `(,second-key -1)))
   (= -7 (dispatch `(,third-key 0)))
   (zero? (dispatch `(,third-key 7)))))
   