#lang scheme

(define (err n)
  (lambda x
    (error n "is a stub; see docs for draw.*")))

(define-syntax prov
  (syntax-rules ()
    [(prov name ...) (begin (provide name ...) (define name (err name)) ...)]))

(prov bigBang endOfTime endOfWorld onTick onKeyEvent draw show close
  drawCircle drawDisk drawRect drawLine drawString)

(printf "this is just a stand-in for the plt/collects/htdch/draw/ library\n")
