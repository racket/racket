#lang racket/base
(require racket/class
         racket/list
         racket/port
         framework
         ;; FRP requires
         
         frtime/core/frp
         (except-in frtime/lang-ext
                    undefined?)
         (only-in frtime/lang-core
                  any-nested-reactivity? raise-reactivity)
         
         ;; GRacket require
         mred)

(define drs-eventspace #f)

(define (set-eventspace evspc)
  (set! drs-eventspace evspc))

(define value-snip-copy%
  (class string-snip%
    (init-field current parent)
    (inherit get-admin)
    (define/public (set-current c)
      (parameterize ([current-eventspace drs-eventspace])
        (queue-callback
         (lambda ()
           (set! current c)
           (let ([admin (get-admin)])
             (when admin
               (send admin needs-update this 0 0 2000 100)))))))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send current draw dc x y left top right bottom dx dy draw-caret))
    (super-instantiate (" "))))

(define make-snip
  (case-lambda
    [(bhvr)
     (make-object string-snip%
       (let ([tmp (cond
                    [(behavior? bhvr) (value-now bhvr)]
                    [(event? bhvr) (signal-value bhvr)]
                    [else bhvr])])
         (cond
           [(event-set? tmp) (format "#<event (last: ~a@~a)>"
                                     (event-set-events tmp) (event-set-time tmp))]
           [(undefined? tmp) "<undefined>"]
           [else (format "~a" tmp)])))]
    [(bhvr super-render-fun)
     (get-rendering (value-now bhvr) super-render-fun)]))

(define value-snip%
  (class string-snip%
    (init-field bhvr)
    (field [copies empty]
           [loc-bhvr (proc->signal (lambda () (update)) bhvr)]
           [current (make-snip bhvr)])
    
    (define/override (copy)
      (let ([ret (make-object value-snip-copy% current this)])
        (set! copies (cons ret copies))
        ret))
    
    (define/public (update)
      (set! current (make-snip bhvr))
      (for-each (lambda (copy) (send copy set-current current)) copies))
    
    (super-instantiate (" "))))

(define dynamic-snip-copy%
  (class editor-snip%
    (init-field current parent)
    (inherit get-editor)
    (define/public (set-current c)
      (parameterize ([current-eventspace drs-eventspace])
        (queue-callback
         (lambda ()
           (send (get-editor) lock #f)
           (send (get-editor) delete 0 (send (get-editor) last-position))
           (for-each (lambda (thing)
                       (send (get-editor) insert thing
                             (send (get-editor) last-position) (send (get-editor) last-position)))
                     c)
           (send (get-editor) lock #t)))))
    
    (super-new
     [editor (new racket:text%)]
     [with-border? #f]
     [left-margin 0]
     [right-margin 0]
     [top-margin 0]
     [bottom-margin 0])
    (set-current current)))

(define dynamic-snip%
  (class snip%
    (init-field bhvr super-render-fun)
    
    (field [copies empty]
           [loc-bhvr (proc->signal (lambda () (update)) bhvr)]
           [current (make-snip bhvr super-render-fun)])
    
    (define/override (copy)
      (let ([ret (make-object dynamic-snip-copy% current this)])
        (set! copies (cons ret copies))
        ret))
    
    (define/public (update)
      (set! current (make-snip bhvr super-render-fun))
      (for-each (lambda (copy) (send copy set-current current)) copies))
    
    (define/override (size-cache-invalid)
      (for-each
       (lambda (s) (send s size-cache-invalid))
       copies))
    
    (define/override (get-extent dc x y w h descent space lspace rspace)
      (send current get-extent dc x y w h descent space lspace rspace))
    
    (super-new)))

(define (render beh as-snip?)
  (cond
    [as-snip? (watch beh)]
    [(undefined? (value-now beh)) "<undefined>"]
    [(behavior? beh) (format "#<behavior (~a)>" (value-now beh))]
    [(event? beh) (format "#<event (last: ~a)>" (event-set-events (signal-value beh)))]
    [else beh]))

(define (render/dynamic-snip val super-render-fun)
  (if (behavior? val)
      ; interesting case:
      ; create a snip
      ; each time val changes, recompute its rendering via super-render-fun
      (make-object dynamic-snip% val super-render-fun)
      ; easy case
      (super-render-fun val)))

(define (get-rendering val super-render-fun)
  (let-values ([(in out) (make-pipe-with-specials)])
    (thread (lambda () (super-render-fun val out) (close-output-port out)))
    (let loop ([chars empty])
      (let ([c (read-char-or-special in)])
        (if (eof-object? c)
            (reverse (rest chars))
            (loop (cons c chars)))))))

(define (watch beh super-render-fun)
  (cond
    [(undefined? beh)
     (begin
       (make-object string-snip% "<undefined>")
       )
     ]
    [(event? beh)
     (make-object value-snip% beh)]
    [(or (behavior? beh) (any-nested-reactivity? beh))
     (make-object dynamic-snip% (raise-reactivity beh) super-render-fun)]
    [(signal? beh)
     (make-object dynamic-snip% beh super-render-fun)]
    [else beh]))

(provide (all-defined-out))
