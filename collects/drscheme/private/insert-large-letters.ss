#lang scheme/base

(require mred scheme/class string-constants framework)

(provide insert-large-letters)

(define (insert-large-letters comment-prefix comment-character edit parent)
  (let ([str (make-large-letters-dialog comment-prefix comment-character #f)])
    (when (and str
               (not (equal? str "")))
      (render-large-letters comment-prefix comment-character (get-chosen-font) str edit)
      (void))))

(preferences:set-default 'drscheme:large-letters-font #f (λ (x) (or (and (pair? x)
                                                                         (string? (car x))
                                                                         (number? (cdr x))
                                                                         (integer? (cdr x))
                                                                         (<= 1 (cdr x) 255))
                                                                    (not x))))

(define (get-default-font)
  (send (send (editor:get-standard-style-list)
              find-named-style
              "Standard")
        get-font))

(define (get-chosen-font)
  (let ([pref-val (preferences:get 'drscheme:large-letters-font)])
    (cond
      [pref-val
       (let ([candidate (send the-font-list find-or-create-font (cdr pref-val) (car pref-val) 'default 'normal 'normal)])
         (if (equal? (send candidate get-face) (car pref-val))
             candidate
             (get-default-font)))]
      [else
       (get-default-font)])))

(define columns-string "~a columns")

;; make-large-letters-dialog : string char top-level-window<%> -> void
(define (make-large-letters-dialog comment-prefix comment-character parent)
  (define dlg (new dialog% 
                   [parent parent] 
                   [width 700]
                   [label (string-constant large-semicolon-letters)]))
  (define text-field (new text-field% 
                          [parent dlg] 
                          [label (string-constant text-to-insert)]
                          [callback (λ (x y) (update-txt (send text-field get-value)))]))
  (define info-bar (new horizontal-panel%
                        [parent dlg]
                        [stretchable-height #f]))
  (define font-choice (new choice%
                           [label (string-constant fonts)]
                           [parent info-bar]
                           [choices (get-face-list)]
                           [callback
                            (λ (x y)
                              (let ([old (preferences:get 'drscheme:large-letters-font)])
                                (preferences:set 'drscheme:large-letters-font
                                                 (cons (send font-choice get-string-selection)
                                                       (if old
                                                           (cdr old)
                                                           (send (get-default-font) get-point-size))))
                                (update-txt (send text-field get-value))))]))
  (define count (new message% [label (format columns-string 1000)] [parent info-bar]))
  (define pane1 (new horizontal-pane% (parent info-bar)))
  (define dark-msg (new bitmap-message% [parent info-bar]))
  (define pane2 (new horizontal-pane% (parent info-bar)))
                                
                                
  (define txt (new scheme:text%))
  (define ec (new editor-canvas% [parent dlg] [editor txt]))
  (define button-panel (new horizontal-panel%
                            [parent dlg]
                            [stretchable-height #f]
                            [alignment '(right center)]))
  (define ok? #f)
  (define-values (ok cancel)
    (gui-utils:ok/cancel-buttons button-panel
                                 (λ (x y) (set! ok? #t) (send dlg show #f))
                                 (λ (x y) (send dlg show #f))))
  (define (update-txt str)
    (send txt begin-edit-sequence)
    (send txt lock #f)
    (send txt delete 0 (send txt last-position))
    (let ([bm (render-large-letters comment-prefix comment-character (get-chosen-font) str txt)])
      (send ec set-line-count (+ 1 (send txt last-paragraph)))
      (send txt lock #t)
      (send txt end-edit-sequence)
      (send count set-label (format columns-string (get-max-line-width txt)))
      (send dark-msg set-bm bm)))
  
  (send font-choice set-string-selection (send (get-chosen-font) get-face))
  
  (send txt auto-wrap #f)
  (update-txt " ")
  (send text-field focus)
  (send dlg show #t)
  (and ok? (send text-field get-value)))

(define (get-max-line-width txt)
  (let loop ([i (+ (send txt last-paragraph) 1)]
             [m 0])
    (cond
      [(zero? i) m]
      [else (loop (- i 1)
                  (max m (- (send txt paragraph-end-position (- i 1))
                            (send txt paragraph-start-position (- i 1)))))])))
      
(define bitmap-message%
  (class canvas%
    (inherit min-width min-height get-dc)
    (define bm #f)
    (define/override (on-paint)
      (when bm
        (let ([dc (get-dc)])
          (send dc draw-bitmap bm 0 0))))
    (define/public (set-bm b)
      (set! bm b)
      (min-width (send bm get-width))
      (min-height (send bm get-height)))
    (super-new (stretchable-width #f)
               (stretchable-height #f))))

(define (render-large-letters comment-prefix comment-character the-font str edit)
  (define bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #t)))
  (define-values (tw raw-th td ta) (send bdc get-text-extent str the-font))
  (define th (let-values ([(_1 h _2 _3) (send bdc get-text-extent "X" the-font)])
               (max raw-th h)))
  (define tmp-color (make-object color%))
  
  (define (get-char x y)
    (send bdc get-pixel x y tmp-color)
    (let ([red (send tmp-color red)])
      (if (= red 0)
          comment-character
          #\space)))
  (define bitmap
    (make-object bitmap% 
      (max 1 (inexact->exact tw))
      (inexact->exact th)
      #t))
  
  (define (fetch-line y)
    (let loop ([x (send bitmap get-width)]
               [chars null])
      (cond
        [(zero? x) (apply string chars)]
        [else (loop (- x 1) (cons (get-char (- x 1) y) chars))])))
  
  (send bdc set-bitmap bitmap)
  (send bdc clear)
  (send bdc set-font the-font)
  (send bdc draw-text str 0 0)
  
  (send edit begin-edit-sequence)
  (let ([start (send edit get-start-position)]
        [end (send edit get-end-position)])
    (send edit delete start end)
    (send edit insert "\n" start start)
    (let loop ([y (send bitmap get-height)])
      (unless (zero? y)
        (send edit insert (fetch-line (- y 1)) start start)
        (send edit insert comment-prefix start start)
        (send edit insert "\n" start start)
        (loop (- y 1)))))
  (send edit end-edit-sequence)
  (send bdc set-bitmap #f)
  bitmap)

;(make-large-letters-dialog ";" #\; #f)
