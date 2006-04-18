(module frp-snip mzscheme
  (require (lib "class.ss")
           (lib "string.ss")
           (lib "list.ss")
           
           ;; FRP requires
           
           (lib "frp-core.ss" "frtime")
           (all-except (lib "lang-ext.ss" "frtime") undefined?)
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
  
  (define (make-snip bhvr)
    (make-object string-snip%
      (let ([tmp (cond
                   [(behavior? bhvr) (value-now bhvr)]
                   [(event? bhvr) (signal-value bhvr)]
                   [else bhvr])])
        (cond
          [(econs? tmp) (format "#<event (last: ~a)>" (efirst tmp))]
          [(undefined? tmp) "<undefined>"]
          [else (expr->string tmp)]))))
  
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
  
  (define (render beh as-snip?)
    (cond
      [as-snip? (watch beh)]
      [(undefined? (value-now beh)) "<undefined>"]
      [(behavior? beh) (format "#<behavior (~a)>" (value-now beh))]
      [(event? beh) (format "#<event (last: ~a)>" (efirst (signal-value beh)))]
      [else beh]))
  
  #;(define (get-rendering val super-render-fun)
    (let-values ([(in out) (make-pipe-with-specials)])
      (thread (lambda () (super-render-fun val out) (flush-output out) (close-output-port out)))
      (let loop ([chars empty])
        (let ([c (read-char-or-special in)])
          (cond
            [(eof-object? c) (list->string (reverse chars))]
            [(char? c) (loop (cons c chars))]
            [else c])))))
  
  (define (watch beh)
    (cond
      [(undefined? beh)
       (begin
         ;(printf "~a was regarded as undefined~n" beh)
         (make-object string-snip% "<undefined>")
         )
         ]
      [(signal? beh) (make-object value-snip% beh)]
      [else beh]))
  
  (provide (all-defined))
  )
