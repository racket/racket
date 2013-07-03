;; This is a modified version of the original "gui.ss" teachpack:
;;   - Instead of having one window, each call to "create-window"
;;     returns a window
;;   - Message items stretch horizontally to fill the window, 
;;     avoiding the need for long initialization messages
#lang scheme/gui

(require htdp/error
         lang/prim
         mzlib/class
         mzlib/list
         mzlib/etc)

(provide-primitives
 window?       ; any/c -> boolean 
 create-window ; (listof (listof GUI-ITEM)) -> window
 show-window   ; window -> true
 hide-window   ; window -> true
 make-text     ; Str -> GUI-ITEM
 text-contents ; GUI-ITEM[text%] -> Str
 make-choice   ; (listof Str) -> GUI-ITEM
 choice-index  ; GUI-ITEM[choice%] -> Str
 ; make-button ; Str (Event% -> Boolean) -> GUI-ITEM
 make-message  ; Str -> GUI-ITEM
 draw-message  ; GUI-ITEM[message%] Str -> true
 )

(provide-higher-order-primitive make-button (_ call-back))

#| ------------------------------------------------------------------
  Students build a window from a "matrix" list of GUI-ITEMS. 
  To build GUI-ITEMs, they need to use make-text, make-choice, 
  make-button, make-choice, or make-message. A GUI-ITEM can be 
  added to the window only once. 
  
  |#

;; INFRASTRUCTURE OPERATIONS:
;; ------------------------------------------------------------------

(define-struct window (get-frame))

;; show-window : -> true
;; effect: to show the window
(define (show-window w)
  (check-arg 'show-window (window? w) "window" "its" w)
  (send ((window-get-frame w)) show true)
  true)

;; hide-window : X -> true
;; effect: to hide the window 
(define (hide-window w)
  (check-arg 'hide-window (window? w) "window" "its" w)
  (send ((window-get-frame w)) show false)
  true)

;; MAKING ITEMS:
;; ------------------------------------------------------------------

(define-struct gui-item (builder))
;; A gui-item[C < control%] is a structure: (make-gui-item f)
;; where f is a function: (panel% -> C)

;; create-gui-item : ((union panel% #f) -> C[< control%])
;; to create a memoizing gui-item
;; create-window is the only caller that can pass in a panel
;; exceptions: ;; if the gui-item is already "panel'ed", error
;; all other callers must pass in #f
(define (create-gui-item builder)
  (let ([C false])
    (make-gui-item
     (lambda (p)
       (cond
         [(and p C)
          (error 'create-window "item added to window twice")]
         [(and p (not C)) (set! C (builder p)) C]
         [(and (not p) C) C]
         [(and (not p) (not C))
          (error 'gui "gui-items must be added to window before use (see create-window)")])))))

;; create-window : (listof (listof gui-item)) -> true
;; to add gui-items to the window and to show window
(define (create-window loi)
  (check-list-list 'create-window (listoflistof? gui-item? "gui-item" loi) "gui-items" loi)
  
  (let ([the-frame (make-object frame% "GUI" false 10 10)])
    (for-each (lambda (loi)
                (let ((p (make-object horizontal-pane% the-frame)))
                  (send p set-alignment 'center 'center)
                  (for-each (lambda (i) ((gui-item-builder i) p)) loi)))
              loi)
    (let ([w (make-window (lambda () the-frame))])
      (show-window w)
      w)))

;; (_ -> Boolean) String X -> (union String true)
(define (listoflistof? pred? pred given)
  (cond
    [(not (list? given)) (format NONLIST given)]
    [(find-non list? given)
     => (lambda (non-list)
          (format NONLISTLIST non-list))]
    [(ormap identity (map (lambda (ll) (find-non pred? ll)) given))
     => (lambda (non-x)
          (format NONX pred non-x))]
    [else #t]))

(define NONLIST "list expected, given: ~e")
(define NONLISTLIST "list of lists expected, given list with ~e")
(define NONX "list of lists of ~a expected, given list of lists with ~e")


#| Tests ------------------------------------------------------------------
  (listoflistof? number? "number" '((1 2 3) (4 5 6)))
  
  (string=? (format NONX "number" 'a)
            (listoflistof? number? "number" '((1 2 3) (4 a 6))))
  
  (string=? (format NONLISTLIST 1)
            (listoflistof? number? "number" '(1 (2 3) (4 5 6))))
  |#

;; make-text : str -> gui-item
;; to create a text-item with label lbl
(define (make-text lbl)
  (check-arg 'make-text (string? lbl) "string" "first" lbl)
  (create-gui-item
   (lambda (the-panel) 
     (make-object text-field% lbl the-panel void))))

;; make-message : str -> gui-item
;; to create a message-item with current contents txt
(define (make-message txt)
  (check-arg 'make-message (string? txt) "string" "first" txt)
  (create-gui-item 
   (lambda (the-panel)
     (new message% [label txt] [parent the-panel] [stretchable-width #t]))))

;; make-button : str (event% -> boolean) -> gui-item
;; to create a button-item with label and call-back function 
(define (make-button label call-back)
  (check-arg 'make-button (string? label) "string" 'first label)
  (check-proc 'make-button call-back 1 'second "1 argument")
  (create-gui-item 
   (lambda (the-panel)
     (make-object button% label
       the-panel
       (lambda (b e)
         (check-result 'button-callback boolean? "Boolean" (call-back e)))))))

;; make-choice : (listof str) -> gui-item
;; to create a choice-item that permits users to choose from the
;; alternatives on loc
(define (make-choice loc)
  (check-arg 'make-choice (and (list? loc) (andmap string? loc)) "list of strings" "first" loc)
  (create-gui-item 
   (lambda (the-panel)
     (make-object choice% "" loc the-panel void))))

;; DISPLAYING MESSAGES:
;; ------------------------------------------------------------------

;; draw-message : gui-item[message%] str -> true
;; to change the current contents of a message field 
(define (draw-message msg txt)
  (check-arg 'draw-message (gui-item? msg) "gui-item" "first" msg)
  (check-arg 'draw-message (string? txt) "string" "second" txt)   
  (let* ([o ((gui-item-builder msg) #f)])
    (when (<= (send o min-width) (string-length txt))
      (let*-values ;; MF: I couldn't think of a better way of doing this
          ([(m) (new message% [parent (new frame% [label "hello"])][label txt])]
           [(x y) (send m get-graphical-min-size)])
        (send o min-width x)))
    (send o set-label txt)
    true))

;; PROBING ITEMS:
;; ------------------------------------------------------------------

;; text-contents : gui-item[text-field%] -> str
;; to determine the contents of a text-item 
(define (text-contents a-text-gui)
  (check-arg 'text-contents (gui-item? a-text-gui) "gui-item" "first" a-text-gui)
  (send ((gui-item-builder a-text-gui) #f) get-value))

;; choice-index : gui-item[choice%] -> number
;; to determine which choice is currently selected in a choice-item 
(define (choice-index a-choice)
  (check-arg 'choice-index (gui-item? a-choice) "gui-item" "first" a-choice)      
  (send ((gui-item-builder a-choice) #f) get-selection))


