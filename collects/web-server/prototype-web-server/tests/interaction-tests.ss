(require "../abort-resume.ss")

(define (id x) x)

;; ****************************************
;; ****************************************
;; BASIC TESTS

(module m00 "../lang.ss"
  (define (id x) x)
  (provide start)
  (define (start i)
    (id i)))

(require (prefix m00: m00))
(run-start (lambda () (start-interaction id)) m00:start)
(= 7 (dispatch-start 7))
(= 8 (dispatch-start 8))

(module m01 "../lang.ss"
  (provide start)
  (define (start i)
    (+ (* 1 2) (* 3 4) i)))

(require (prefix m01: m01))
(run-start (lambda () (start-interaction id)) m01:start)
(= 14 (dispatch-start 0))
(= 20 (dispatch-start 6))

;; start-interaction may be called mutitple times
;; each call overwrites the previous interaction
;; continuation with the latest one.
; XXX Can't do this anymore
#|
(module m02 "../lang.ss"
  (define (id x) x)
  (+ (start-interaction id)
     (start-interaction id)))

(require m02)
(void? (dispatch-start 1))
(= 3 (dispatch-start 2))
(= 0 (dispatch-start -1))
|#

;; ****************************************
;; ****************************************
;; TESTS INVOLVING CALL/CC

(module m03 "../lang.ss"
  (provide start)
  (define (start x)
    (let/cc k
      (+ 2 4 (k 3) 6 8))))

(require (prefix m03: m03))
(run-start (lambda () (start-interaction id)) m03:start)
(= 3 (dispatch-start 'foo))
(= 3 (dispatch-start 7))

;; in the following test, if you modify
;; resume to print the "stack" you will
;; see that this is not tail recursive
(module m04 "../lang.ss"
  (provide start)
  (define (start ln)
    (let/cc k
      (cond
        [(null? ln) 1]
        [(zero? (car ln)) (k 0)]
        [else
         (* (car ln)
            (start (cdr ln)))]))))

(require (prefix m04: m04))
(run-start (lambda () (start-interaction id)) m04:start)
(= 0 (dispatch-start (list 1 2 3 4 5 6 7 0 8 9)))
(= 120 (dispatch-start (list 1 2 3 4 5)))

;; this version captures the continuation
;; outside the recursion and should be tail
;; recursive. A "stack trace" reveals this
;; as expected.
(module m05 "../lang.ss"
  (provide start)
  
  (define (start ln)
    (let/cc escape
      (mult/escape escape ln)))
  
  (define (mult/escape escape ln)
    (cond
      [(null? ln) 1]
      [(zero? (car ln)) (escape 0)]
      [else
       (* (car ln)
          (mult/escape escape (cdr ln)))])))

(require (prefix m05: m05))
(run-start (lambda () (start-interaction id)) m05:start)
(= 0 (dispatch-start (list 1 2 3 0 4 5 6)))
(= 120 (dispatch-start (list 1 2 3 4 5)))

;; ****************************************
;; ****************************************
;; TESTS INVOLVING send/suspend
; XXX Doesn't work
#|
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
|#