(module frp-snip mzscheme
  (require (lib "class.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "port.ss")
           
           ;; FRP requires
           
           (lib "frp-core.ss" "frtime")
           (all-except (lib "lang-ext.ss" "frtime") undefined?)
           (only (lib "mzscheme-core.ss" "frtime") any-nested-reactivity? raise-reactivity)
;           (rename (lib "frp-core.ss" "frtime") behavior? behavior?)
;           (rename (lib "lang-ext.ss" "frtime") event? event?)
;           (rename (lib "frp-core.ss" "frtime") signal? signal?)
;           
;           (rename (lib "frp-core.ss" "frtime") econs? econs?)
;           (rename (lib "frp-core.ss" "frtime") efirst efirst)
;
;           (rename (lib "frp-core.ss" "frtime") value-now value-now)
;           (rename (lib "frp-core.ss" "frtime") signal-value signal-value)
;           (rename (lib "lang-ext.ss" "frtime") undefined undefined)
;           (rename (lib "lang-ext.ss" "frtime") undefined? frp:undefined?)
;           
;           (rename (lib "frp-core.ss" "frtime") proc->signal proc->signal)

           ;; MrEd require
           (all-except (lib "mred.ss" "mred") send-event))
  
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
             [(econs? tmp) (format "#<event (last: ~a)>" (efirst tmp))]
             [(undefined? tmp) "<undefined>"]
             [else (expr->string tmp)])))]
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
    (class snip%
      (init-field current parent)
      (inherit get-admin)
      (define/public (set-current c)
        (parameterize ([current-eventspace drs-eventspace])
          (queue-callback
           (lambda ()
             (set! current c)
             (let ([admin (get-admin)])
               (when admin
                 (send admin resized this #t)
                 #;(send admin needs-update this 0 0 2000 100)))))))
      #;(define/override (resize w h)
        (super resize w h)
        (send (get-admin) resized this #t)
        #t)
      (define/override (size-cache-invalid)
        (send current size-cache-invalid))
              
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (send current get-extent dc x y w h descent space lspace rspace))

      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (send current draw dc x y left top right bottom dx dy draw-caret))
      (super-new)))
    
  (define dynamic-snip%
    (class snip%
      (init-field bhvr super-render-fun)
  
      (field [copies empty]
             [loc-bhvr (proc->signal (lambda () (update)) bhvr)]
             [current (make-snip bhvr super-render-fun)])
      
      (define/override (copy)
        (let ([ret (make-object value-snip-copy% current this)])
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
      [(event? beh) (format "#<event (last: ~a)>" (efirst (signal-value beh)))]
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
          ;(fprintf (current-error-port) "read ~a~n" c)
          (cond
            [(eof-object? c) (make-object string-snip% (list->string (reverse (rest chars))))]
            [(char? c) (loop (cons c chars))]
            [else c])))))
  
  (define (watch beh super-render-fun)
    (cond
      [(undefined? beh)
       (begin
         ;(printf "~a was regarded as undefined~n" beh)
         (make-object string-snip% "<undefined>")
         )
         ]
      [(or (behavior? beh) (any-nested-reactivity? beh))
       (make-object dynamic-snip% (raise-reactivity beh) super-render-fun)]
      [(signal? beh)
       (make-object dynamic-snip% beh super-render-fun)]
       #;(let ([pb (new pasteboard%)])
         (send pb insert (make-object dynamic-snip% beh super-render-fun))
         (new editor-snip% [editor (new pasteboard%)]))
      [else beh]))
  
  (provide (all-defined))
  )
