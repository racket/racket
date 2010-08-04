(load-relative "loadtest.rktl")
(Section 'place-channel)

(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(splat
#<<END
(module pct1 scheme
  (provide place-main)


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
    (define pc1 (place-channel->receiver-channel (place-channel-recv ch)))
    (pcrss pc1 (string-append x "-ok")))
)
END
"pct1.ss")

(define-syntax-rule (pc-send-recv-test ch (send expect) ...) 
  (begin (test expect place-channel-send/recv ch send) ...))


(define-struct building (rooms location) #:prefab)
(define-struct (house building) (occupied ) #:prefab)
(define h1 (make-house 5 'factory 'yes))


(let ([pl (place "pct1.ss" 'place-main)])
  (pc-send-recv-test pl
    (1 2 )
    ("Hello" "Hello-ok")
    ((cons 'a 'a) (cons 'a 'b))
    ((list 'a 'a) (list 'a 'b 'a))
    (#(a a) #(a b a))
    (h1 #s((abuilding 1 building 2) 6 'utah 'no)))
  (define pc1 (place-channel))
  (place-channel-send pl pc1)
  (test "Testing-ok" place-channel-send/recv pc1 "Testing")
  (place-wait pl)
)

