;; GUI widget initialization tests

(require racket/gui)

(define frame
  (new frame%
   [label "label"]
   [parent #f]
   [width 100]
   [height 100]
   [x 0]
   [y 0]
   [style null]
   [enabled #t]
   [border 0]
   [spacing 0]
   [alignment '(center top)]
   [min-width 100]
   [min-height 100]
   [stretchable-width #t]
   [stretchable-height #t]))

(define cb (lambda (b e) (void)))
(define font (make-object font% 1 'system))

;; top levels
(make-object frame%
  "label"       ; label
  #f            ; parent
  100           ; width
  100           ; height
  0             ; x
  0             ; y
  null          ; style
  #t            ; enabled
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object dialog%
  "label"       ; label
  #f            ; parent
  100           ; width
  100           ; height
  0             ; x
  0             ; y
  null          ; style
  #t            ; enabled
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

;; panels
(make-object horizontal-panel%
  frame         ; parent
  null          ; style
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object vertical-panel%
  frame         ; parent
  null          ; style
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object tab-panel%
  '("a" "b")    ; choices
  frame         ; parent
  cb            ; callback
  null          ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object group-box-panel%
  "label"       ; label
  frame         ; parent
  null          ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

;; panes
(make-object horizontal-pane%
  frame         ; parent
  0             ; vert-margin
  0             ; horiz-margin
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object vertical-pane%
  frame         ; parent
  0             ; vert-margin
  0             ; horiz-margin
  0             ; border
  0             ; spacing
  '(center top) ; alignment
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

;; controls
(make-object message%
  "label"       ; label
  frame         ; parent
  null          ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  #f            ; auto-resize
  )

(make-object button%
  "label"       ; label
  frame         ; parent
  cb            ; callback
  null          ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object radio-box%
  "label"       ; label
  '("a" "b")    ; choices
  frame         ; parent
  cb            ; callback
  '(vertical)   ; style
  0             ; selection
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object check-box%
  "label"       ; label
  frame         ; parent
  cb            ; callback
  null          ; style
  #f            ; value
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object slider%
  "label"       ; label
  0             ; min-value
  100           ; max-value
  frame         ; parent
  cb            ; callback
  50            ; init-value
  '(vertical)   ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object gauge%
  "label"       ; label
  100           ; range
  frame         ; parent
  '(vertical)   ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object text-field%
  "label"       ; label
  frame         ; parent
  cb            ; callback
  "foo"         ; init-value
  '(single)     ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object combo-field%
  "label"       ; label
  '("a" "b")    ; choices
  frame         ; parent
  cb            ; callback
  "foo"         ; init-value
  null          ; style
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

;; list controls
(make-object choice%
  "label"       ; label
  '("a" "b")    ; choices
  frame         ; parent
  cb            ; callback
  null          ; style
  0             ; selection
  font          ; font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object list-box%
  "label"       ; label
  '("a" "b")    ; choices
  frame         ; parent
  cb            ; callback
  '(single)     ; style
  0             ; selection
  font          ; font
  font          ; label-font
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  '("Column")   ; columns
  #f            ; column-order
  )

;; canvases
(make-object canvas%
  frame         ; parent
  null          ; style
  cb            ; paint-callback
  "label"       ; label
  #f            ; gl-config
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

(make-object editor-canvas%
  frame         ; parent
  #f            ; editor
  null          ; style
  100           ; scrolls-per-page
  #f            ; label
  3             ; wheel-step
  #f            ; line-count
  5             ; horizontal-inset
  5             ; vertical-inset
  #t            ; enabled
  0             ; vert-margin
  0             ; horiz-margin
  100           ; min-width
  100           ; min-height
  #t            ; stretchable-width
  #t            ; stretchable-height
  )

;; menus
(define menu
  (make-object menu%
    "label"                       ; label
    (make-object menu-bar% frame) ; parent
    #f                            ; help-string
    (lambda (m) (void))           ; demand-callback
    ))

(make-object popup-menu%
  "label"               ; title
  (lambda (p e) (void)) ; popdown-callback
  (lambda (p) (void))   ; demand-callback
  font                  ; font
  )

(make-object menu-bar%
  (new frame% [label ""]) ; parent
  (lambda (p) (void))     ; demand-callback
  )

(make-object menu-item%
  "label"               ; label
  menu                  ; parent
  cb                    ; callback
  'up                   ; shortcut
  "foo"                 ; help-string
  (lambda (p) (void))   ; demand-callback
  '(alt)                ; shortcut-prefix
  )

(make-object checkable-menu-item%
  "label"               ; label
  menu                  ; parent
  cb                    ; callback
  'up                   ; shortcut
  "foo"                 ; help-string
  (lambda (p) (void))   ; demand-callback
  #f                    ; checked
  '(alt)                ; shortcut-prefix
  )

(make-object separator-menu-item%
  menu  ; parent
  )

;; misc
(make-object key-event%
  #\nul ; key-code
  #f    ; shift-down
  #f    ; control-down
  #f    ; meta-down
  #f    ; alt-down
  0     ; x
  0     ; y
  0     ; time-stamp
  #f    ; caps-down
  )

(make-object mouse-event%
  'enter ; event-type
  #f     ; left-down
  #f     ; middle-down
  #f     ; right-down
  0      ; x
  0      ; y
  #f     ; shift-down
  #f     ; control-down
  #f     ; meta-down
  #f     ; alt-down
  0      ; time-stamp
  #f     ; caps-down
  )

(make-object mouse-event%
  'top       ; event-type
  'vertical  ; direction
  0          ; position
  0          ; time-stamp
  )

(make-object mouse-event%
  'button    ; event-type
  0          ; time-stamp
  )
