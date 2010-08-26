(load-relative "loadtest.rktl")
(Section 'place-channel)
(require racket/flonum)

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(splat
#<<END
(module pct1 scheme
  (provide place-main)
  (require racket/flonum)

  (define-syntax (pcrs stx)
    (syntax-case stx ()
      [(_ ch body) 
       (with-syntax
          [(x (syntax-local-introduce #'x))]
        #'(place-channel-send ch 
          (let ([x (place-channel-recv ch)])
            body)))]))

  (define-syntax-rule (pcrss ch body ...) (begin (pcrs ch body) ...))

  (define (place-main ch)
    (pcrss ch
      (+ 1 x)
      (string-append x "-ok")
      (cons (car x) 'b)
      (list (car x) 'b (cadr x))
      (vector (vector-ref x 0) 'b (vector-ref x 1))
      #s((abuilding 1 building 2) 6 'utah 'no))

    (define pc1 (place-channel-recv ch))
    (pcrss pc1 (string-append x "-ok"))

    (define pc3 (first (place-channel-recv ch)))
    (pcrss pc3 (string-append x "-ok3"))

    (pcrss ch (begin (flvector-set! x 2 5.0) "Ready1"))
    (pcrss ch (begin (flvector-set! x 2 6.0) "Ready2"))
    (pcrss ch (begin (bytes-set! x 2 67) "Ready3"))
    (pcrss ch (begin (bytes-set! x 2 67) "Ready4"))
  )
)
END
"pct1.ss")

(define-syntax-rule (pc-send-recv-test ch (send expect) ...) 
  (begin (test expect place-channel-send/recv ch send) ...))


(define-struct building (rooms location) #:prefab)
(define-struct (house building) (occupied ) #:prefab)
(define h1 (make-house 5 'factory 'yes))

(define flv1 (shared-flvector 0.0 1.0 2.0 3.0))
(define flv2 (make-shared-flvector 4 3.0))

(define b1 (shared-bytes 66 66 66 66))
(define b2 (make-shared-bytes 4 65))

(let ([pl (place "pct1.ss" 'place-main)])
  (pc-send-recv-test pl
    (1 2 )
    ("Hello" "Hello-ok")
    ((cons 'a 'a) (cons 'a 'b))
    ((list 'a 'a) (list 'a 'b 'a))
    (#(a a) #(a b a))
    (h1 #s((abuilding 1 building 2) 6 'utah 'no)))

  (define-values (pc1 pc2) (place-channel))
  (place-channel-send pl pc2)
  (test "Testing-ok" place-channel-send/recv pc1 "Testing")

  (define-values (pc3 pc4) (place-channel))
  (place-channel-send pl (list pc4))
  (test "Testing-ok3" place-channel-send/recv pc3 "Testing")

  (test "Ready1" place-channel-send/recv pl flv1)
  (test 5.0 flvector-ref flv1 2)

  (test "Ready2" place-channel-send/recv pl flv2)
  (test 6.0 flvector-ref flv2 2)

  (test "Ready3" place-channel-send/recv pl b1)
  (test 67 bytes-ref b1 2)

  (test "Ready4" place-channel-send/recv pl b2)
  (test 67 bytes-ref b2 2)


  (place-wait pl)
)
