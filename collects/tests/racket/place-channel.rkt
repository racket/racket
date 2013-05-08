#lang racket/base
(require racket/flonum
         racket/fixnum
         racket/place
         racket/list
         racket/tcp
         racket/match
         rackunit
         (for-syntax racket/base))

(define-syntax-rule (define-place (name ch) body ...)
  (define name (lambda () (place ch body ...))))

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(define big (make-string 1025 #\K))

(define (big-sender ch msg) (car (place-channel-put/get ch (cons msg big))))

(define-syntax (normal-receiver stx)
  (syntax-case stx ()
    [(_ ch x body ...)
      #'(let ([x (place-channel-get ch)])
          (place-channel-put ch (begin body ...)))]))

(define-syntax (big-receiver stx)
  (syntax-case stx ()
    [(_ ch x body ...)
      #'(let ([x (car (place-channel-get ch))])
          (place-channel-put ch (cons (begin body ...) big)))]))

(define (test expect fun . args)
  (printf "~s ==> " (cons fun args))
  (flush-output)
  (let ([res (if (procedure? fun)
               (apply fun args)
               (car args))])
    (printf "~s\n" res)
    (let ([ok? (equal? expect res)])
      (unless ok?
        (printf "  BUT EXPECTED ~s\n" expect)
        (eprintf "ERROR\n"))
      ok?)))

(define (echo ch) (place-channel-put ch (place-channel-get ch)))
(define (recv/print ch) (displayln (place-channel-get ch)))

(define-struct building (rooms location) #:prefab)
(define-struct (house building) (occupied ) #:prefab)
(define h1 (make-house 5 'factory 'yes))
(define l1 (list (cons 1 2) (cons 'red "green") (cons (hash) 1) (cons (vector 1) #s(blue 2))))
(define l2 (list (cons 1 2) (cons 3 4) (cons #\a 5)))
(define l3 (list (cons 1 2) (cons 3 4) (cons #\a 5) (cons 3.1415 12)))

(define-syntax (test-place-channel-get/put stx)
  (syntax-case stx ()
    [(_ ch x body ...) #'(normal-receiver ch x body ...)]))

(define-syntax (test-place-channel-get/put-* stx)
  (syntax-case stx ()
    [(_ ch x body ...) #'(begin (normal-receiver ch x body) ...)]))


(define-syntax-rule (channel-test-basic-types-worker receiver ch)
  (begin
    (define-syntax-rule (test-place-channel-get/put-* x body (... ...))
      (begin (receiver ch x body) (... ...)))

    (test-place-channel-get/put-* x
      (not x)
      (not x)
      '#:b
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
      `(,x)
      (make-immutable-hash (list (cons 'red 'der)))
      (make-immutable-hash l1)
      (make-immutable-hasheq l2)
      (make-immutable-hasheqv l3)
      (bytes->path x 'unix)
      (bytes->path x 'windows)
      )))

(define (channel-test-basic-types-master sender ch)
  (define-syntax-rule (test-place-channel-put-receive sender ch (send expect) ...) 
    (begin (test expect sender ch send) ...))

  (test-place-channel-put-receive sender ch
    (#t #f)
    (#f #t)
    ('#:a '#:b)
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
    ('(printf "Hello") '((printf "Hello")))
    ((make-hash (list (cons 'red 'der))) (make-immutable-hash (list (cons 'red 'der))))
    ((make-hash l1) (make-immutable-hash l1))
    ((make-hasheq l2) (make-immutable-hasheq l2))
    ((make-hasheqv l3) (make-immutable-hasheqv l3))
    (#"/tmp/unix" (bytes->path #"/tmp/unix" 'unix))
    (#"C:\\Windows" (bytes->path #"C:\\Windows" 'windows))
    ))

(define-place (place-worker ch)
  (channel-test-basic-types-worker normal-receiver ch)
  (channel-test-basic-types-worker big-receiver ch)

  (define pc1 (place-channel-get ch))
  (test-place-channel-get/put pc1 x (string-append x "-ok"))

  (define pc3 (first (place-channel-get ch)))
  (test-place-channel-get/put pc3 x (string-append x "-ok3"))

  (test-place-channel-get/put-* ch x
    (begin (flvector-set! x 2 5.0) "Ready1")
    (begin (flvector-set! x 2 6.0) "Ready2")
    (begin (fxvector-set! x 2 5)   "Ready2.1")
    (begin (fxvector-set! x 2 6)   "Ready2.2")
    (begin (bytes-set! x 2 67)     "Ready3")
    (begin (bytes-set! x 2 67)     "Ready4"))

  (define pc5 (place-channel-get ch))
  (place-channel-put pc5 "Ready5")

  (channel-test-basic-types-worker normal-receiver pc5)
  (channel-test-basic-types-worker big-receiver pc5)

  (for ([i (in-range 3)]) (echo pc5))
  (for ([i (in-range 3)]) (recv/print ch)))

(define len 1000000)

(define intern-num-sym
  (let ([ht (make-hash)])
    (lambda (k)
      (hash-ref ht k 
                (lambda ()
                  (hash-set! ht k (string->symbol (format "~s" k)))
                  (hash-ref ht k))))))

(define-syntax-rule (test-long msg desc)
  (begin
    (define l (build-list len msg))
    (define ll (length l))
    (printf "Master ~a length ~a\n" desc ll)

    (define p (place ch
      (define l (place-channel-get ch))
      (define wl (length l))
      (printf "Worker length ~a\n" wl)
      (when (symbol? (car l))
        (for ([v (in-list l)]
              [x (in-naturals)])
          (unless (and (symbol? v)
                       (eq? v (intern-num-sym (modulo x 1000))))
            (printf "bad ~s\n" v))))
      (place-channel-put ch wl)))


    (place-channel-put p l)
    (define wlen (place-channel-get p))
    (unless (= wlen ll)
      (raise (format "~a master length ~a != worker length ~a\n" desc ll wlen))
    (place-wait p))))

(module+ test
  (main))

(define (main)
  
  ;test breaks in BEGIN_ESCAPABLE during scheme_place_async_try_receive
  (let ()
    (for ([i 25])
      (let ()
        (define-values (ch1 ch2) (place-channel))
        (define mt 
          (thread 
            (lambda ()
              (let loop () (loop)))))
        (define t
          (thread
            (lambda ()
              (for ([i 100000])
                ;(place-channel-put ch1 (list "foo" 1 "bar"))
                (place-channel-put ch1 (make-list 7000 'foo))
                ))))
        (define t2
          (thread
            (lambda ()
              (time (for ([i 100000])
                (sync ch2))))))

        (define ti (/ (+ (random 100) 1) 100))
        (sleep ti)
        (kill-thread mt) 
        (kill-thread t2)
        (kill-thread t)
        ;(displayln (exact->inexact ti))
        (thread-wait t)
        (thread-wait t2))))

  (let ()
    (define flx (make-shared-fxvector 10 0))
    (define flv (make-shared-flvector 10 0.0))
    (define bs (make-shared-bytes 10 60))
    (define-values (in out) (place-channel))

    (define p (place ch 
                     (define a
                       (for/hash ([x (place-channel-get ch)])
                                 (values x x)))
                     (define b
                       (for/hash ([x (place-channel-get ch)])
                                 (values x x)))
                     (test #t eq? a a)
                     (test #t eq? b b)
                     (test #f eq? a b)
                     (test #t equal? a b)
                     (test #t equal? b a)))

    (place-channel-put p (list flx flv bs in out))
    (place-channel-put p (list flx flv bs in out))
    (place-wait p))


  (let ()
    (define p1 (place ch
                      (define in (place-channel-get ch))
                      (test 'val place-channel-get in)))
    (define p2 (place ch
                      (define in (place-channel-get ch))
                      (sleep 1)
                      (define t (thread
                                 (lambda () 
                                   (test 'val place-channel-get in))))
                      (sleep 1)
                      'done))
    (define-values (in out) (place-channel))
    (place-channel-put p1 in)
    (place-channel-put p2 in)
    (sleep 4)
    (place-channel-put out 'val)
    (sleep 4)
    (place-channel-put out 'val)
    (place-wait p1)
    (place-wait p2)
    (test (void) printf "Matthew's example completes"))

  ; test signal-handle vector growing
  (let ()
    (define ps (for/list ([i (in-range 4)])
      (place ch (define in (place-channel-get ch))
             (test #t not (not (member (place-channel-get in) (list 'val1 'val2 'val3 'val4)))))))
    (define-values (in out) (place-channel))
    (for ([p ps]) (place-channel-put p in))
    (sleep 4)
    (for ([x (list 'val1 'val2 'val3 'val4)]) (place-channel-put out x))
    (sleep 4)
    (for ([p ps]) (place-wait p))
    (test (void) printf "signal-handle vector growing completes"))

  ; test signal-handle vector shrinking
  (let ()
    (define ps (for/list ([i (in-range 4)])
      (place ch (define in (place-channel-get ch))
             (test #t not (not (member (place-channel-get in) (list 'val1 'val2 'val3 'val4)))))))
    (define-values (in out) (place-channel))
    (for ([p ps]) (place-channel-put p in))
    (sleep 4)
    (for ([x (list 'val1 'val2 'val3 'val4)]) (place-channel-put out x))
    (sleep 4)
    (for ([p ps]) (place-wait p))

    (define p0 (place ch 
                      (define in (place-channel-get ch))
                      (test 'p0val1 place-channel-get in)
                      (test 'p0val2 place-channel-get in)
                      (test 'p0val3 place-channel-get in)
                      (test 'p0val4 place-channel-get in)))
    (place-channel-put p0 in)
    (for ([x (list 'p0val1 'p0val2 'p0val3)]) (place-channel-put out x))
    (sleep 4)
    (place-channel-put out 'p0val4)
    (for ([p ps]) (place-wait p0))
    (test (void) printf "signal-handle vector growing completes"))


(let ([pl (place-worker)])
  (define flv1 (shared-flvector 0.0 1.0 2.0 3.0))
  (define flv2 (make-shared-flvector 4 3.0))
  (define fxv1 (shared-fxvector 0 1 2 3))
  (define fxv2 (make-shared-fxvector 4 3))
  (define b1 (shared-bytes 66 66 66 66))
  (define b2 (make-shared-bytes 4 65))

  ; test place-channel communication of basic types
  (channel-test-basic-types-master place-channel-put/get pl)
  (channel-test-basic-types-master big-sender pl)

  (define-values (pc1 pc2) (place-channel))
  (place-channel-put pl pc2)
  (test "Testing-ok" place-channel-put/get pc1 "Testing")

  (define-values (pc3 pc4) (place-channel))
  (place-channel-put pl (list pc4))
  (test "Testing-ok3" place-channel-put/get pc3 "Testing")

  (test "Ready1" place-channel-put/get pl flv1)
  (test 5.0 flvector-ref flv1 2)

  (test "Ready2" place-channel-put/get pl flv2)
  (test 6.0 flvector-ref flv2 2)

  (test "Ready2.1" place-channel-put/get pl fxv1)
  (test 5 fxvector-ref fxv1 2)

  (test "Ready2.2" place-channel-put/get pl fxv2)
  (test 6 fxvector-ref fxv2 2)

  (test "Ready3" place-channel-put/get pl b1)
  (test 67 bytes-ref b1 2)

  (test "Ready4" place-channel-put/get pl b2)
  (test 67 bytes-ref b2 2)

  (define-values (pc5 pc6) (place-channel))
  (place-channel-put pl pc5)
  (test "Ready5" sync pc6)
  (channel-test-basic-types-master place-channel-put/get pc6)
  (channel-test-basic-types-master big-sender pc6)

  ; test deep copy of cycles
  (let ([try-graph
         (lambda (s)
           (let ([v (read (open-input-string s))])
             (place-channel-put pc6 v)
             (test v place-channel-get pc6)))])
    (try-graph "#0=(#0# . #0#)")
    (try-graph "#0=#(#0# 7 #0#)")
    (try-graph "#0=#s(thing 7 #0#)"))

  (check-exn exn:fail? (λ () (place-channel-put pl (open-output-string))))
  (check-not-exn (λ () (place-channel-put pl "Test String")))
  (check-not-exn (λ () (place-channel-put pl (bytes->path #"/tmp/unix" 'unix))))
  (check-not-exn (λ () (place-channel-put pl (bytes->path #"C:\\Windows" 'windows))))

    (place-wait pl))

  ; test place-break
  (let ()
    (define (go kind)
      (let ([p (place ch
                 (define kind (place-channel-get ch))
                 (with-handlers ([(case kind
                                    [(hang-up) exn:break:hang-up?]
                                    [(terminate) exn:break:terminate?]
                                    [else exn:break?])
                                  (lambda (x) (place-channel-put ch "OK"))])
                   (place-channel-put ch "ALIVE")
                   (sync never-evt)
                   (place-channel-put ch "NOK")))])
        (place-channel-put p kind)
        (test "ALIVE" place-channel-get p)
        (place-break p kind)
        (test "OK" place-channel-get p)
        (place-wait p)))
    (go #f)
    (go 'hang-up)
    (go 'terminate))

  ; test place-dead-evt
  (define wbs '())
  (for ([i (in-range 0 50)])
    (define p (place ch (void (place-channel-get ch))))
    (set! wbs
          (cons
           (make-weak-box
            (thread
             (λ ()
               (define-values (in out) (place-channel))
               (place-channel-put p in)
               (sync 
                 (handle-evt (place-dead-evt p)
                   (lambda (x) (printf "Place ~a died\n" i) ))
                 out))))
           wbs))
    (collect-garbage)
    (set! wbs (filter weak-box-value wbs))
    (printf "len ~a\n" (length wbs)))

  ; test deep stack handling in places_deep_copy c routine
  (test-long (lambda (x) 3) "Listof ints")
  (test-long (lambda (x) #(1 2)) "Listof vectors")
  (test-long (lambda (x) (intern-num-sym (modulo x 1000))) "Listof symbols")
  (test-long (lambda (x) #s(clown "Binky" "pie")) "Listof prefabs")
  (test-long (lambda (x) (read (open-input-string "#0=(#0# . #0#)"))) "Listof cycles")

  ;; check that a thread blocked on a place channel
  ;; can be GCed if the other end of the channel is
  ;; unreachable --- where a place's channels should
  ;; all count as "unreachable" when the place ends
  (displayln "checking place-channel and thread GC interaction")
  (let ([N 40])
    (define weaks (make-weak-hash))
    (for ([i (in-range N)])
      (define s (make-semaphore))
      (hash-set!
       weaks
       (thread (lambda ()
                 (define-values (i o) (place-channel))
                 (define p (place ch (place-channel-get ch)))
                 (place-channel-put p o)
                 (place-wait p)
                 (semaphore-post s)
                 (sync i)))
       #t)
      (sync s))
    (for ([i 3])
      (sync (system-idle-evt))
      (collect-garbage))
    (unless ((hash-count weaks) . < . (/ N 2))
      (error "thread-gc test failed")))

)
  
;(report-errs)
