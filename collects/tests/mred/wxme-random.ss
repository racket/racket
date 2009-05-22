#lang scheme/gui

(define seed (abs (current-milliseconds)))
(random-seed seed)

(error-print-context-length 100)

(define orig-t (new text%))

(define frame
  (new (class frame% 
         (define/augment (on-close) (exit))
         (super-new))
       [label "Test"]
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
  (random-elem '#("a" "x\ny\nz\n" "(define (f x)\n  (+ x x))\n" "hello there")))

(define seqs (make-hasheq))

(define ts-length 64)
(define ts-pos 0)
(define ts (make-vector ts-length orig-t))
(define (add-t! t2)
  (if (= ts-pos ts-length)
      (let ([v ts])
        (set! ts (make-vector ts-length orig-t))
        (set! ts-pos 0)
        (for ([t3 (in-vector v)])
          (when (zero? (random 2))
            (add-t! t3)))
        (add-t! t2))
      (begin
        (vector-set! ts ts-pos t2)
        (set! ts-pos (add1 ts-pos)))))

;; Don't paste before copying, because that interferes with replay
(define copied? #f)
(define (set-copied?! t)
  (unless (= (send t get-start-position)
             (send t get-end-position))
    (set! copied? #t)))

(define actions
  (vector
   (lambda (t) (send t undo))
   (lambda (t) (send t redo))
   (lambda (t) (send t insert (random-string) (random (add1 (send t last-position)))))
   (lambda (t) (send t insert "\t" (random (add1 (send t last-position)))))
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
   (lambda (t) (set-copied?! t) (send t copy))
   (lambda (t) (set-copied?! t) (send t cut))
   (lambda (t) (set-copied?! t) (send t kill))
   (lambda (t) (when copied? 
                 (send t paste)
                 (when (zero? (random 4))
                   (send t paste-next))))
   (lambda (t) (send t change-style (make-object style-delta% 'change-size (add1 (random 42)))))
   (lambda (t) (send t change-style 
                     (send (make-object style-delta%) set-delta-foreground (make-object color%
                                                                                        (random 256)
                                                                                        (random 256)
                                                                                        (random 256)))))
   (lambda (t) 
     (let ([t2 (new text%)])
       (add-t! t2)
       (init t2)
       (send t insert (make-object editor-snip% t2))))
   (lambda (t)
     (send t set-max-width (if (zero? (random 2)) 
                               (+ 50.0 (/ (random 500) 10.0))
                               'none)))
   (lambda (t) (yield (system-idle-evt)))
   ))

(send canvas focus)
   
(let loop ()
  (let ([act (random-elem actions)]
        [t (if (zero? (random 2))
               orig-t
               (random-elem ts))])
    (printf "~s: ~s ~s\n" seed (eq-hash-code t) act)
    (act t)
    (loop)))
