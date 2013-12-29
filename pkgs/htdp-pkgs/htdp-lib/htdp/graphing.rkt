#lang scheme

(require htdp/error
         lang/posn
         lang/prim
         mzlib/unit
         htdp/draw-sig
         htdp/big-draw)

(provide-signature-elements draw^)

(provide 
 graph-fun
 graph-line
 )

(module test racket/base)

(define-higher-order-primitive graph-line graph-line/proc (f _))
(define-higher-order-primitive graph-fun graph-fun/proc (f _))

;; --------------------------------------------------------------------------

;; make-graph : sym -> void
;; effect: set up pasteboard for drawing functions 
;;   between [0,10] and [0,10] on x/y axis 
(define (make-graph name)
  (start EAST SOUTH)
  (let* ([vp+pm #cs(get-@VP)]
         [vp    (car vp+pm)])
    (draw-solid-line ORIGIN X-AXIS 'blue)
    ((draw-string vp) (make-posn (+ OFFSET 10) (+ OFFSET 10)) "Y-AXIS")
    (draw-solid-line ORIGIN Y-AXIS 'blue)
    ((draw-string vp) (make-posn (- EAST 100) (- SOUTH 15)) "X-AXIS")))

;; (num -> num) symbol -> true
;; effect: draw function graph for x in [0,10] at delta = .1
(define (graph-line/proc f color)
  (check 'graph-line f color)
  (let ((p1 (translate (make-posn 0 (f 0))))
        (p2 (translate (make-posn 10 (f 10)))))
    (draw-solid-line p1 p2 color)))

;; (num -> num) symbol -> true
;; effect: draw function graph for x in [0,10] at delta = .1
(define (graph-fun/proc f color)
  (check 'graph-fun f color)
  (draw-tab (map translate (tabulate f 0 10 DELTA)) color))

;; check : tst tst tst -> void
(define (check tag f color)
  (check-proc tag f 1 '1st "one argument")
  (check-arg tag (symbol? color) 'symbol '2nd color))

;; tabulate : (num -> num) num num num -> (list-of (make-posn num num))
(define (tabulate f left right delta)
  (if (> left right) null
      (cons (make-posn left (f left))
            (tabulate f (+ left delta) right delta))))

;; translate : posn -> posn
(define (translate p)
  (make-posn (+ (* FACT (/ 1 DELTA) (posn-x p)) OFFSET) 
             (- (- SOUTH (* FACT (/ 1 DELTA) (posn-y p))) OFFSET)))

;; draw-tab : (list-of (make-posn num num)) symbol -> true
(define (draw-tab lop color)
  (for-each (lambda (p) (draw-solid-disk p DOT color)) lop)
  (unless (or (null? lop) (null? (cdr lop)))
    (let loop ([f (car lop)][r (cdr lop)])
      (unless (null? r) 
        (let ([next (car r)])
          (draw-solid-line f next color)
          (loop next (cdr r))))))
  #t)

(define EAST  400)
(define SOUTH EAST)
(define FACT  (/ (- EAST 100) 100))
(define OFFSET   10.)
(define ORIGIN (make-posn OFFSET          (- SOUTH OFFSET)))
(define X-AXIS (make-posn OFFSET OFFSET))
(define Y-AXIS (make-posn (- EAST OFFSET) (- SOUTH OFFSET)))
(define GRAPH-COLOR 'red)

(define DELTA .1)
(define DOT   1)

(make-graph 'ok)
