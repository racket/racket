#lang scheme/gui

(define seed (abs (current-milliseconds)))
(random-seed seed)

(error-print-context-length 100)

(define orig-t (new text%))

(define frame
  (new frame% [label "Test"]
       [width 300]
       [height 400]))
(define canvas
  (new editor-canvas% [parent frame] [editor orig-t]))

(send frame show #t)

(define (init t)
  (send t set-max-undo-history 100))
(init orig-t)

(define (random-elem v)
  (vector-ref v (random (vector-length v))))

(define (random-string)
  (random-elem '#("a" "x\ny\nz\n" "hello there")))

(define seqs (make-hasheq))
(define ts (make-weak-hasheq))

(define actions
  (vector
   (lambda (t) (send t undo))
   (lambda (t) (send t redo))
   (lambda (t) (send t insert (random-string) (random (add1 (send t last-position)))))
   (lambda (t) 
     (let ([pos (random (add1 (send t last-position)))])
       (send t delete pos (random (max 1 (- (send t last-position) pos))))))
   (lambda (t)
     (send t begin-edit-sequence)
     (hash-update! seqs t add1 0))
   (lambda (t) 
     (let loop ()
       (when (positive? (hash-ref seqs t 0))
         (send t end-edit-sequence)
         (hash-update! seqs t sub1)
         (when (zero? (random 2))
           (loop)))))
   (lambda (t) 
     (let ([pos (random (add1 (send t last-position)))])
       (send t set-position pos (random (max 1 (- (send t last-position) pos))))))
   (lambda (t) (send t copy))
   (lambda (t) (send t cut))
   (lambda (t) (send t paste))
   (lambda (t) (send t change-style (make-object style-delta% 'change-size (add1 (random 42)))))
   (lambda (t) 
     (let ([t2 (new text%)])
       (hash-set! ts t2 #t)
       (init t2)
       (send t insert (make-object editor-snip% t2))))
   (lambda (t)
     (send t set-max-width (if (zero? (random 2)) 100.0 'none)))
   ))
   
(let loop ()
  (let ([act (random-elem actions)]
        [t (if (zero? (random 2))
               orig-t
               (for/fold ([t orig-t])
                   ([t (in-hash-keys ts)]
                    [n (in-range (random (add1 (hash-count ts))))])
                 t))])
    (printf "~s: ~s\n" seed act)
    (act t)
    (loop)))
