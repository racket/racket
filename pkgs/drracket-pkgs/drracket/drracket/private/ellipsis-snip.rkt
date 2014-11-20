#lang racket/base
(require racket/gui/base
         racket/contract
         racket/class
         (prefix-in r: racket/base)
         framework)
(provide ellipsis-snip%)

(define ellipsis-snip%
  (class snip%
    (init-field extra [insertion-done? #f])
    (inherit get-style)
    (define str "...")
    (define/override (get-extent dc x y wb hb db sb lb rb)
      (set-box/f! lb 0)
      (set-box/f! rb 0)
      (define-values (w h d a) (send dc get-text-extent str (send (get-style) get-font)))
      (set-box/f! wb w)
      (set-box/f! hb h)
      (set-box/f! db d)
      (set-box/f! sb a))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text str x y))
    
    (define/override (on-goodbye-event dc x y editorx editory event)
      (handle-event dc x y editorx editory event #t))
    (define/override (on-event dc x y editorx editory event)
      (handle-event dc x y editorx editory event #f))
    (define/private (handle-event dc x y editorx editory event goodbye?)
      (unless insertion-done?
        (define admin (get-admin))
        (when admin
          (define ed (send admin get-editor))
          (define the-cursor-to-use (if goodbye? #f arrow-cursor))
          (when (send event button-up? 'left)
            (unless goodbye?
              (do-insertion)
              (set! insertion-done? #t)
              (set! the-cursor-to-use #f)))
          (send ed set-cursor the-cursor-to-use))))
    
    (define/private (do-insertion)
      (define admin (get-admin))
      (define ed (send admin get-editor))
      (when (is-a? ed text:ports<%>)
        (define pos (send ed get-snip-position this))
        (when pos
          (send ed begin-edit-sequence)
          (define insertion-pos (+ pos 2))
          (let loop ([strs extra])
            (cond
              [(null? strs) (void)]
              [else
               (define str (car strs))
               (send ed insert/io str insertion-pos (send ed get-err-style-delta))
               (send ed insert/io "\n" insertion-pos (send ed get-err-style-delta))
               (loop (cdr strs))]))
          (send ed end-edit-sequence))))
    (define/override (copy) (new ellipsis-snip% 
                                 [extra extra]
                                 [insertion-done? insertion-done?]))
    (define/override (write f)
      (define bp (open-output-bytes))
      (r:write (list insertion-done? extra) bp)
      (define b (get-output-bytes bp))
      (send f put (bytes-length b) b))
    (super-new)
    (inherit set-flags get-flags get-admin set-snipclass)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    (set-snipclass snipclass)))

(define arrow-cursor (make-object cursor% 'arrow))

(define (set-box/f! b v) (when (box? b) (set-box! b v)))

(provide snipclass)
(define valid-data? (list/c boolean? (listof string?)))
(define snipclass
  (new (class snip-class%
         (define/override (read f)
           (define data (r:read (open-input-bytes (send f get-unterminated-bytes))))
           (cond
             [(valid-data? data)
              (new ellipsis-snip% 
                   [insertion-done? (list-ref data 0)]
                   [extra (list-ref data 1)])]
             [else
              (new ellipsis-snip% [extra '()])]))
         (super-new))))
(send snipclass set-version 2)
(send snipclass set-classname 
      (format "~s" '((lib "ellipsis-snip.rkt" "drracket" "private")
                     (lib "ellipsis-snip-wxme.rkt" "drracket" "private"))))
(send (get-the-snip-class-list) add snipclass)

(module+ main
  (define f (new frame% [label ""] [width 100] [height 100]))
  (define t (new text%))
  (send t insert (new ellipsis-snip% [extra '("a" "b" "c")]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send f show #t))
