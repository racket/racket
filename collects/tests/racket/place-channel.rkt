(load-relative "loadtest.rkt")
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

  (define-syntax pcrss
    (syntax-rules ()
      [(_ ch body ...) (begin (pcrs ch body) ...)]))

  (define (place-main ch)
    (pcrss ch
      (+ 1 x)
      (string-append x "-ok")
      (cons (car x) 'b)
      (list (car x) 'b (cadr x))
      (vector (vector-ref x 0) 'b (vector-ref x 1))
      #s((bozo 1 building 2) 6 'gubber 'no)
  ))
)
END
"pct1.ss")

(define (pcsr ch x)
  (place-channel-send ch x)
  (place-channel-recv ch))

(define-syntax pcsrs
    (syntax-rules ()
      [(_ ch (send expect) ...) (begin (test expect pcsr ch send) ...)]))


(define-struct building (rooms location) #:prefab)
(define-struct (house building) (occupied ) #:prefab)
(define h1 (make-house 5 'factory 'no))


(let ([pl (place "pct1.ss" 'place-main)])
  (pcsrs pl
    (1 2 )
    ("Hello" "Hello-ok")
    ((cons 'a 'a) (cons 'a 'b))
    ((list 'a 'a) (list 'a 'b 'a))
    (#(a a) #(a b a))
    (h1 #s((bozo 1 building 2) 6 'gubber 'no))
))

