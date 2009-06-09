#lang scheme/gui

(define seed (abs (current-milliseconds)))
(random-seed seed)

(define use-nested? #t)

(error-print-context-length 100)

;; Don't paste before copying, because that interferes with replay
(define copied? #f)
(define copy-len 0)

(define (go pause x-pos)
  (define orig-t (new text%))

  (define frame
    (new (class frame% 
           (define/augment (on-close) (exit))
           (super-new))
         [label "Test"]
         [width 300]
         [height 400]
         [x x-pos]))

  (define canvas
    (new editor-canvas% [parent frame] [editor orig-t]))
  
  (define _1 (send frame show #t))
  
  (define (init t)
    (send t set-max-undo-history 100))
  (define _2 (init orig-t))

  (define (random-elem v)
    (vector-ref v (random (vector-length v))))

  (define (random-string)
    (random-elem '#("a" "x\ny\nz\n" "(define (f x)\n  (+ x x))\n" "hello there" "" "\n")))

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

  (define (set-copied?! t)
    (let ([len (- (send t get-end-position)
                  (send t get-start-position))])
      (if (zero? len)
          #f
          (begin
            (set! copy-len len)
            (set! copied? #t)
            #t))))

  (define (maybe-convert)
    (when (zero? (random 4))
      (let ([data (send the-clipboard get-clipboard-data "WXME" 0)])
        (send the-clipboard set-clipboard-client
              (new (class clipboard-client%
                     (inherit add-type)
                     (super-new)
                     (add-type "WXME")
                     (define/override (get-data format) data)))
              0))))

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
     (lambda (t) (when (set-copied?! t) (send t copy) (maybe-convert)))
     (lambda (t) (when (set-copied?! t) (send t cut) (maybe-convert)))
     (lambda (t) (when copied? 
                   (let ([s (send t get-start-position)]
                         [e (send t get-end-position)]
                         [l (send t last-position)])
                     (send t paste)
                     (when copy-len
                       (unless (= (send t last-position)
                                  (+ (- l (- e s)) copy-len))
                         (error 'paste "length mismatch: [~s, ~s) in ~s + ~s ~s -> ~s"
                                s e l copy-len 
                                (send the-clipboard get-clipboard-data "TEXT" 0)
                                (send t last-position)))))
                   (when (zero? (random 4))
                     (set! copy-len #f)
                     (send t paste-next))))
     (lambda (t) (send t change-style (make-object style-delta% 'change-size (add1 (random 42)))))
     (lambda (t) (send t change-style 
                       (send (make-object style-delta%) set-delta-foreground (make-object color%
                                                                                          (random 256)
                                                                                          (random 256)
                                                                                          (random 256)))))
     (lambda (t) 
       (when use-nested?
         (let ([t2 (new text%)])
           (add-t! t2)
           (init t2)
           (send t insert (make-object editor-snip% t2)))))
     (lambda (t)
       (send t set-max-width (if (zero? (random 2)) 
                                 (+ 50.0 (/ (random 500) 10.0))
                                 'none)))
     (lambda (t) (yield (system-idle-evt)))
     (lambda (t) (pause))
     ))
  
  (send canvas focus)
  
  (let loop ()
    (let ([act (random-elem actions)]
          [t (if (zero? (random 2))
                 orig-t
                 (random-elem ts))])
      (printf "~s: ~s ~s\n" seed (eq-hash-code t) act)
      (act t)
      (loop))))

(define (run-one)
  (go void 50))

(define (run-two-concurrent)
  (define sema-one (make-semaphore))
  (define sema-two (make-semaphore))
  
  (define (make sema-this sema-other x-pos)
    (parameterize ([current-eventspace (make-eventspace)])
      (queue-callback
       (lambda ()
         (semaphore-wait sema-this)
         (go (lambda ()
               (semaphore-post sema-other)
               (semaphore-wait sema-this))
             x-pos)))
      (current-eventspace)))

  (define e1 (make sema-one sema-two 50))
  (define e2 (make sema-two sema-one 350))
  (semaphore-post sema-one)
  (application-quit-handler (lambda args (exit)))
  (yield never-evt))

(define (run-two)
  (define one-box (box #f))
  (define two-box (box #f))
  (define (make box-this box-other x-pos)
    (let/ec esc
      (call-with-continuation-prompt
       (lambda ()
         (begin
           (let/cc k
             (set-box! box-this k)
             (esc))
           (go (lambda ()
                 (let/cc k
                   (set-box! box-this k)
                   ((unbox box-other))))
               x-pos))))))

  (make one-box two-box 50)
  (make two-box one-box 350)
  (call-with-continuation-prompt
   (lambda ()
     ((unbox one-box)))))

(run-two)

