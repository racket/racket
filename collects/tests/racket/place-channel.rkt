#lang racket/base
(require racket/flonum
         racket/fixnum
         racket/place
         racket/list
         rackunit
         (for-syntax racket/base))

(provide main)

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(define big (make-string 1025 #\K))

(define (big-sender ch msg) (car (place-channel-send/receive ch (cons msg big))))

(define-syntax (normal-receiver stx)
  (syntax-case stx ()
    [(_ ch x body ...) 
      #'(let ([x (place-channel-receive ch)])
        (place-channel-send ch (begin body ...)))]))

(define-syntax (big-receiver stx)
  (syntax-case stx ()
    [(_ ch x body ...) 
      #'(let ([x (car (place-channel-receive ch))])
        (place-channel-send ch (cons (begin body ...) big)))]))

(define (test expect fun . args)
  (printf "~s ==> " (cons fun args))
  (flush-output)
  (let ([res (if (procedure? fun)
               (apply fun args)
               (car args))])
    (printf "~s\n" res)
    (let ([ok? (equal? expect res)])
      (unless ok?
        (printf "  BUT EXPECTED ~s\n" expect))
      ok?)))

(define (echo ch) (place-channel-send ch (place-channel-receive ch)))
(define (recv/print ch) (displayln (place-channel-receive ch)))

(define-struct building (rooms location) #:prefab)
(define-struct (house building) (occupied ) #:prefab)
(define h1 (make-house 5 'factory 'yes))

(define-syntax (test-place-channel-receive/send stx)
  (syntax-case stx ()
    [(_ ch x body ...) #'(normal-receiver ch x body ...)]))

(define-syntax (test-place-channel-receive/send-* stx)
  (syntax-case stx ()
    [(_ ch x body ...) #'(begin (normal-receiver ch x body) ...)]))


(define-syntax-rule (channel-test-basic-types-worker receiver ch)
  (begin
    (define-syntax-rule (test-place-channel-receive/send-* x body (... ...))
      (begin (receiver ch x body) (... ...)))

    (test-place-channel-receive/send-* x
      (not x)
      (not x)
      (void)
      null
      1/3
      (/ 1 5)
      (* x 2)
      4+9i
      (+ 1 x)
      (string-append x "-ok")
      (cons (car x) 'b)
      (list (car x) 'b (cadr x))
      (vector (vector-ref x 0) 'b (vector-ref x 1))
      #s((abuilding 1 building 2) 6 'utah 'no)
      `(,x))))

(define (channel-test-basic-types-master sender ch)
  (define-syntax-rule (test-place-channel-send-receive sender ch (send expect) ...) 
    (begin (test expect sender ch send) ...))

  (test-place-channel-send-receive sender ch
    (#t #f)
    (#f #t)
    (null (void))
    ((void) null)
    ((/ 1 2) 1/3)
    (1/4 (/ 1 5))
    ((exact->inexact (/ 1 3)) 0.6666666666666666)
    (3+8i 4+9i)
    (1 2)
    ("Hello" "Hello-ok")
    ((cons 'a 'a) (cons 'a 'b))
    ((list 'a 'a) (list 'a 'b 'a))
    (#(a a) #(a b a))
    (h1 #s((abuilding 1 building 2) 6 'utah 'no))
    ('(printf "Hello") '((printf "Hello")))))

(define-place (place-worker ch)
  (channel-test-basic-types-worker normal-receiver ch)
  (channel-test-basic-types-worker big-receiver ch)

  (define pc1 (place-channel-receive ch))
  (test-place-channel-receive/send pc1 x (string-append x "-ok"))

  (define pc3 (first (place-channel-receive ch)))
  (test-place-channel-receive/send pc3 x (string-append x "-ok3"))

  (test-place-channel-receive/send-* ch x
    (begin (flvector-set! x 2 5.0) "Ready1")
    (begin (flvector-set! x 2 6.0) "Ready2")
    (begin (fxvector-set! x 2 5)   "Ready2.1")
    (begin (fxvector-set! x 2 6)   "Ready2.2")
    (begin (bytes-set! x 2 67)     "Ready3")
    (begin (bytes-set! x 2 67)     "Ready4"))

  (define pc5 (place-channel-receive ch))
  (place-channel-send pc5 "Ready5")

  (channel-test-basic-types-worker normal-receiver pc5)
  (channel-test-basic-types-worker big-receiver pc5)

  (for ([i (in-range 3)]) (echo pc5))
  (for ([i (in-range 3)]) (recv/print ch)))


(define (main)
(let ([pl (place-worker)])
  (define flv1 (shared-flvector 0.0 1.0 2.0 3.0))
  (define flv2 (make-shared-flvector 4 3.0))
  (define fxv1 (shared-fxvector 0 1 2 3))
  (define fxv2 (make-shared-fxvector 4 3))
  (define b1 (shared-bytes 66 66 66 66))
  (define b2 (make-shared-bytes 4 65))

  (channel-test-basic-types-master place-channel-send/receive pl)
  (channel-test-basic-types-master big-sender pl)

  (define-values (pc1 pc2) (place-channel))
  (place-channel-send pl pc2)
  (test "Testing-ok" place-channel-send/receive pc1 "Testing")

  (define-values (pc3 pc4) (place-channel))
  (place-channel-send pl (list pc4))
  (test "Testing-ok3" place-channel-send/receive pc3 "Testing")

  (test "Ready1" place-channel-send/receive pl flv1)
  (test 5.0 flvector-ref flv1 2)

  (test "Ready2" place-channel-send/receive pl flv2)
  (test 6.0 flvector-ref flv2 2)

  (test "Ready2.1" place-channel-send/receive pl fxv1)
  (test 5 fxvector-ref fxv1 2)

  (test "Ready2.2" place-channel-send/receive pl fxv2)
  (test 6 fxvector-ref fxv2 2)

  (test "Ready3" place-channel-send/receive pl b1)
  (test 67 bytes-ref b1 2)

  (test "Ready4" place-channel-send/receive pl b2)
  (test 67 bytes-ref b2 2)

  (define-values (pc5 pc6) (place-channel))
  (place-channel-send pl pc5)
  (test "Ready5" sync pc6)
  (channel-test-basic-types-master place-channel-send/receive pc6)
  (channel-test-basic-types-master big-sender pc6)

  (let ([try-graph
         (lambda (s)
           (let ([v (read (open-input-string s))])
             (place-channel-send pc6 v)
             (test v place-channel-receive pc6)))])
    (try-graph "#0=(#0# . #0#)")
    (try-graph "#0=#(#0# 7 #0#)")
    (try-graph "#0=#s(thing 7 #0#)"))

  (check-exn exn:fail? (λ () (place-channel-send pl (open-output-string))))
  (check-not-exn (λ () (place-channel-send pl "Test String")))
  (check-not-exn (λ () (place-channel-send pl (bytes->path #"/tmp/unix" 'unix))))
  (check-not-exn (λ () (place-channel-send pl (bytes->path #"C:\\Windows" 'windows))))

  (place-wait pl))

(let ([p (place/anon ch
           (with-handlers ([exn:break? (lambda (x) (place-channel-send ch "OK"))])
            (place-channel-send ch "ALIVE")
            (sync never-evt)
            (place-channel-send ch "NOK")))])

  (test "ALIVE" place-channel-receive p)
  (place-break p)
  (test "OK" place-channel-receive p)
  (place-wait p)))


;(report-errs)
