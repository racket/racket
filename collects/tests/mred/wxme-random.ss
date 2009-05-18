#lang scheme/gui

(define seed 704050726 #;(abs (current-milliseconds)))
(random-seed seed)

(define t (new text%))

(define frame
  (new frame% [label "Test"]
       [width 300]
       [height 400]))
(define canvas
  (new editor-canvas% [parent frame] [editor t]))

(send frame show #t)

(send t set-max-undo-history 100)

(define (random-elem v)
  (vector-ref v (random (vector-length v))))

(define (random-string)
  (random-elem '#("a" "x\ny\nz\n" "hello there")))

(define seq 0)

(define actions
  (vector
   (lambda () (send t undo))
   (lambda () (send t redo))
   (lambda () (send t insert (random-string) (random (add1 (send t last-position)))))
   (lambda () 
     (let ([pos (random (add1 (send t last-position)))])
       (send t delete pos (random (max 1 (- (send t last-position) pos))))))
   (lambda ()
     (send t begin-edit-sequence)
     (set! seq (add1 seq)))
   (lambda () 
     (let loop ()
       (when (positive? seq)
         (send t end-edit-sequence)
         (set! seq (sub1 seq))
         (when (zero? (random 2))
           (loop)))))
   (lambda () 
     (let ([pos (random (add1 (send t last-position)))])
       (send t set-position pos (random (max 1 (- (send t last-position) pos))))))
   (lambda () (send t copy))
   (lambda () (send t cut))
   (lambda () (send t paste))
   (lambda () (send t change-style (make-object style-delta% 'change-size (add1 (random 42)))))
   (lambda () (send t insert (make-object editor-snip%)))
   ))
   
(let loop ()
  (let ([act (random-elem actions)])
    (printf "~s: ~s\n" seed act)
    (act)
    (loop)))

   