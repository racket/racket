(load-relative "loadtest.rktl")
(Section 'place-channel)
(require racket/flonum
         rackunit)

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(splat
#<<END
(module pct1 racket/base
  (provide place-main)
  (require racket/flonum
           racket/place
           racket/list
           (for-syntax racket/base))

  (define-syntax (pcrs stx)
    (syntax-case stx ()
      [(_ ch body) 
       (with-syntax
          [(x (syntax-local-introduce #'x))]
        #'(place-channel-send ch 
          (let ([x (place-channel-receive ch)])
            body)))]))

  (define-syntax-rule (pcrss ch body ...) (begin (pcrs ch body) ...))

  (define (place-main ch)
    (pcrss ch
      (+ 1 x)
      (string-append x "-ok")
      (cons (car x) 'b)
      (list (car x) 'b (cadr x))
      (vector (vector-ref x 0) 'b (vector-ref x 1))
      #s((abuilding 1 building 2) 6 'utah 'no)
      `(,x))

    (define pc1 (place-channel-receive ch))
    (pcrss pc1 (string-append x "-ok"))

    (define pc3 (first (place-channel-receive ch)))
    (pcrss pc3 (string-append x "-ok3"))

    (pcrss ch (begin (flvector-set! x 2 5.0) "Ready1"))
    (pcrss ch (begin (flvector-set! x 2 6.0) "Ready2"))
    (pcrss ch (begin (bytes-set! x 2 67) "Ready3"))
    (pcrss ch (begin (bytes-set! x 2 67) "Ready4"))

    (define pc5 (place-channel-receive ch))
    (place-channel-send pc5 "Ready5")
  )
)
END
"pct1.ss")

(define-syntax-rule (pc-send-receive-test ch (send expect) ...) 
  (begin (test expect place-channel-send/receive ch send) ...))


(define-struct building (rooms location) #:prefab)
(define-struct (house building) (occupied ) #:prefab)
(define h1 (make-house 5 'factory 'yes))

(define flv1 (shared-flvector 0.0 1.0 2.0 3.0))
(define flv2 (make-shared-flvector 4 3.0))

(define b1 (shared-bytes 66 66 66 66))
(define b2 (make-shared-bytes 4 65))

(let ([pl (place "pct1.ss" 'place-main)])
  (pc-send-receive-test pl
    (1 2 )
    ("Hello" "Hello-ok")
    ((cons 'a 'a) (cons 'a 'b))
    ((list 'a 'a) (list 'a 'b 'a))
    (#(a a) #(a b a))
    (h1 #s((abuilding 1 building 2) 6 'utah 'no))
    ('(printf "Hello") '((printf "Hello"))))

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

  (test "Ready3" place-channel-send/receive pl b1)
  (test 67 bytes-ref b1 2)

  (test "Ready4" place-channel-send/receive pl b2)
  (test 67 bytes-ref b2 2)

  (define-values (pc5 pc6) (place-channel))
  (place-channel-send pl pc5)
  (test "Ready5" sync pc6)

  (check-exn exn:fail? (λ () (place-channel-send pl (open-output-string))))
  (check-not-exn (λ () (place-channel-send pl "Test String")))
  (check-not-exn (λ () (place-channel-send pl (string->path "C:\\Windows"))))

  (place-wait pl)
)

