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

(define (make-snip bhvr)
  (make-object string-snip%
    (let ([tmp (cond
                 [(behavior? bhvr) (value-now bhvr)]
                 [(event? bhvr) (signal-value bhvr)]
                 [else bhvr])])
      (cond
        [(event-set? tmp) (format "#<event (last: ~a@~a)>"
                                  (event-set-events tmp) (event-set-time tmp))]
        [(undefined? tmp) "<undefined>"]
        [else (format "~a" tmp)]))))

(define value-snip%
  (class string-snip%
    (init-field bhvr [ignore-copy-count 1])
    (field [copies empty]
           [current (make-snip bhvr)]
           [loc-bhvr (proc->signal (lambda () (update)) bhvr)])
    
    (define/override (copy)
      (if (> ignore-copy-count 0)
          (begin
            (set! ignore-copy-count (sub1 ignore-copy-count))
            this)
          (let ([ret (make-object value-snip-copy% current this)])
            (set! copies (cons ret copies))
            ret)))
    
    (define/public (update)
      (set! current (make-snip bhvr))
      (for-each (lambda (copy) (send copy set-current current)) copies))
    
    (super-instantiate (" "))))

(define dynamic-snip-copy%
  (class editor-snip%
    (init-field initial-content parent)
    (inherit get-editor set-editor)
    
    (define/public (update content)
      (parameterize ([current-eventspace drs-eventspace])
        (queue-callback
         (lambda ()
           ;; TODO(ghcooper): Figure out why this doesn't work properly for non-
           ;; textual content. (Image snips don't seem to be deleted from the
           ;; editor.) It doesn't even work if we create a completely new
           ;; racket:text% each time, which suggests it's a bug in the snip
           ;; rather than the editor itself.
           (let ([editor (get-editor)])
             (send editor lock #f)
             (send editor delete 0 (send editor last-position))
             (for-each (lambda (thing)
                         (send editor insert thing
                               (send editor last-position)
                               (send editor last-position)))
                       content)
             (send editor lock #t))))))
    
    (super-new
     [editor (new racket:text%)]
     [with-border? #f]
     [left-margin 0]
     [right-margin 0]
     [top-margin 0]
     [bottom-margin 0])
    (update initial-content)))

;; Class of objects to be given to DrRacket for rendering a signal in the
;; interactions window. However, DrRacket won't actually embed this snip
;; directly into the interactions window; instead it makes a copy, and then a
;; copy of the copy, and the second copy is what's really rendered. This makes
;; life challenging for us, because what we want (I believe) is ultimately an
;; editor-snip% whose contents we can rewrite whenever the signal changes.
;; We can't make this class inherit from editor-snip%, though, because we need
;; custom copy behavior, and editor-snip%'s copy method is final. Instead, this
;; class is designed NOT to be rendered, but just to be copied, to keep track of
;; the copy that's actually displayed, and to make sure the copy gets updated
;; when the signal changes. The displayed "copy" is not, in fact, a copy at all
;; but an instance of the dynamic-snip-copy% class defined above.
;;
;; TODO(ghcooper): This code is very brittle; it breaks whenever DrRacket
;; changes the length of the chain of copies it makes. A better approach might
;; be to have a single class that HAS an editor-snip% (instead of inheriting
;; from editor-snip%), delegates all relevant calls to the editor-snip%, and has
;; a copy method that makes a proper copy of itself and (like this class) keeps
;; track of copies so it can notify them when they need to be redrawn.
(define dynamic-snip%
  (class snip%
    (init-field
     ;; The behavior we want to render dynamically.
     bhvr
     ;; Procedure that generates a rendering of the current value of bhvr.
     super-render-fun
     ;; Number of times the copy method will just return this object. Ick!
     [ignore-copy-count 1])
    
    (field [copies empty]  ; "Copies" of this snip that we need to update.
           [current (get-rendering (value-now bhvr) super-render-fun)]
           [loc-bhvr (proc->signal (lambda () (update)) bhvr)])
    
    (define/override (copy)
      (if (> ignore-copy-count 0)
          (begin
            (set! ignore-copy-count (sub1 ignore-copy-count))
            this)
          (let ([ret (make-object dynamic-snip-copy% current this)])
            (set! copies (cons ret copies))
            ret)))
    
    (define/public (update)
      (set! current (get-rendering (value-now bhvr) super-render-fun))
      (for-each (lambda (copy) (send copy update current)) copies))
    
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

;; get-rendering : any (any port -> void) -> (listof (string U snip%))
;; Applies super-render-fun to val and a port. Returns the sequence of values
;; written to the port.
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
