#lang typed/racket/base

(require typed/mred/mred
         typed/framework/framework
         racket/class
         string-constants/string-constant)


(define-type-alias Bitmap-Message% (Class () 
                                          ([parent (Instance Horizontal-Panel%)])
                                          ([set-bm ((Instance Bitmap%) -> Void)])))


(require/typed "bitmap-message.rkt"
               [bitmap-message% Bitmap-Message%])

(provide insert-large-letters)

(: insert-large-letters (String Char (Instance Text:Basic%) Any -> Void))
(define (insert-large-letters comment-prefix comment-character edit parent)
  (let ([str (make-large-letters-dialog comment-prefix comment-character #f)])
    (when (and str
               (not (equal? str "")))
      (render-large-letters comment-prefix comment-character (get-chosen-font) str edit)
      (void))))

(: get-default-font (-> (Instance Font%)))
(define (get-default-font)
  (send (send (editor:get-standard-style-list)
              find-named-style
              "Standard")
        get-font))

(: get-chosen-font (-> (Instance Font%)))
(define (get-chosen-font)
  (let ([pref-val (preferences:get 'drracket:large-letters-font)])
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
(: make-large-letters-dialog (String Char Any -> (Option String)))
(define (make-large-letters-dialog comment-prefix comment-character parent)
  (define dlg (new dialog% 
                   [parent parent] 
                   [width 700]
                   [label (string-constant large-semicolon-letters)]))
  (define: text-field : (Instance Text-Field%) 
    (new text-field% 
         [parent dlg] 
         [label (string-constant text-to-insert)]
         [callback (λ: ([x : Any] [y : Any]) (update-txt (send (assert text-field defined?) get-value)))]))
  (: info-bar (Instance Horizontal-Panel%))
  (define info-bar (new horizontal-panel%
                        [parent dlg]
                        [stretchable-height #f]))
  (define: font-choice : (Instance Choice%)
    (let ([tmp-bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #f))])
      (new choice%
           [label (string-constant fonts)]
           [parent info-bar]
           [choices (map (λ: ((x : String)) (format "~a~a" x (get-w-size tmp-bdc x))) 
                         (get-face-list))]
           [callback
            (λ: ([x : Any] [y : Any])
                (let ([old (preferences:get 'drracket:large-letters-font)]
                      [choice (send (assert font-choice defined?) get-selection)])
                  (when choice
                    (preferences:set 'drracket:large-letters-font
                                     (cons (list-ref (get-face-list)
                                                     choice)
                                           (if old
                                               (cdr old)
                                               (send (get-default-font) get-point-size))))
                    (update-txt (send (assert text-field defined?) get-value)))))])))
  
  (: count (Instance Message%))
  (define count (new message% [label (format columns-string 1000)] [parent info-bar]))
  (: pane1 (Instance Horizontal-Pane%))
  (define pane1 (new horizontal-pane% (parent info-bar)))
  (: dark-msg (Instance Bitmap-Message%))
  (define dark-msg (new bitmap-message% [parent info-bar]))
  (: pane2 (Instance Horizontal-Pane%))
  (define pane2 (new horizontal-pane% (parent info-bar)))
                                
  (: txt (Instance Text:Basic%))
  (define txt (new racket:text%))
  (: ec (Instance Editor-Canvas%))
  (define ec (new editor-canvas% [parent dlg] [editor txt]))
  (: button-panel (Instance Horizontal-Panel%))
  (define button-panel (new horizontal-panel%
                            [parent dlg]
                            [stretchable-height #f]
                            [alignment '(right center)]))
  (define: ok? : Boolean #f)
  (: ok Any)
  (: cancel Any)
  (define-values (ok cancel)
    (gui-utils:ok/cancel-buttons button-panel
                                 (λ: ([x : Any] [y : Any]) (set! ok? #t) (send dlg show #f))
                                 (λ: ([x : Any] [y : Any]) (send dlg show #f))))
  (: update-txt (String -> Any))
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
  
  
  ;; CHANGE - get-face can return #f
  (let ([face (send (get-chosen-font) get-face)])
    (when face
      (let loop ([i 0]
                 [faces (get-face-list)])
        (cond
          [(null? faces) (void)]
          [else (cond
                  [(equal? face (car faces))
                   (send (assert font-choice defined?) set-selection i)]
                  [else
                   (loop (+ i 1) (cdr faces))])]))))
  
  (send txt auto-wrap #f)
  (update-txt " ")
  (send (assert text-field defined?) focus)
  (send dlg show #t)
  (and ok? (send (assert text-field defined?) get-value)))

(: get-w-size ((Instance Bitmap-DC%) String -> String))
(define (get-w-size dc face-name)
  (let ([font (send the-font-list find-or-create-font 24 face-name 'default 'normal 'normal)])
    (let-values ([(w h a d) (send dc get-text-extent "w" font)])
      (format " (~a)" (floor (inexact->exact w))))))
  

(: get-max-line-width ((Instance Text:Basic%) -> Real))
(define (get-max-line-width txt)
  (let loop ([i (+ (send txt last-paragraph) 1)]
             [#{m : Integer} 0])
    (cond
      [(zero? i) m]
      [else (loop (sub1 i)
                  (max m (- (send txt paragraph-end-position (- i 1))
                            (send txt paragraph-start-position (- i 1)))))])))
      

(: render-large-letters (String Char (Instance Font%) String (Instance Text:Basic%) -> (Instance Bitmap%)))
(define (render-large-letters comment-prefix comment-character the-font str edit)
  (define bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #t)))
  (define-values (tw raw-th td ta) (send bdc get-text-extent str the-font))
  (define th (let-values ([(_1 h _2 _3) (send bdc get-text-extent "X" the-font)])
               (max raw-th h)))
  (define tmp-color (make-object color%))
  
  (: get-char (Real Real -> Char))
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
  
  (: fetch-line (Real -> String))
  (define (fetch-line y)
    (let: loop : String ([x : Real (send bitmap get-width)]
                         [chars : (Listof Char) null])
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
