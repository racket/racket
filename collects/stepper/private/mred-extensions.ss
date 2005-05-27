(module mred-extensions mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (prefix f: (lib "framework.ss" "framework"))
           (lib "pretty.ss")
           "highlight-placeholder.ss"
           "testing-shared.ss"
	   (lib "string-constant.ss" "string-constants"))

  (provide
   stepper-canvas%
   stepper-text%
   snip?
   separator-snip% ;; these last two aren't required, but are useful 
   vertical-separator-snip% ;; for debugging purposes
   stepper-warning%
   finished-text
   stepper-text-test)
   
  (define test-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define reduct-highlight-color (make-object color% 255 255 255))
  (define redex-highlight-color (make-object color% 255 255 255))
  (send test-dc try-color (make-object color% 212 159 245) reduct-highlight-color)
  (send test-dc try-color (make-object color% 193 251 181) redex-highlight-color)
  
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground "RED")
  
  (define snip-delta (make-object style-delta% 'change-alignment 'top))
  

;;;;;; copied from /plt/collects/drscheme/snip.ss :
  
  (define separator-snipclass
    (make-object
        (class snip-class% ()
          (override read)
          
          (define (read s)
            (let ([size-box (box 0)])
              (send s get size-box)
              (make-object separator-snip%)))
          
          (super-instantiate ()))))
  
  (send* separator-snipclass
    (set-version 1)
    (set-classname "drscheme:separator-snip%"))
  
  (send (get-the-snip-class-list) add separator-snipclass)
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define separator-snip%
    (class snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (public reset-width)
      (override write copy get-extent draw)
      
      (define width 800)

      (define (reset-width)
        (let* ([admin (get-admin)]
               [reporting-media (send admin get-editor)]
               [reporting-admin (send reporting-media get-admin)]
               [widthb (box 0)])
          (send reporting-admin get-view #f #f widthb #f)
          (set! width (- (unbox widthb) 2))
          (send admin resized this #t)))
      
      (define (write s)
        (send s put (char->integer #\r)))
      
      (define (copy)
        (let ([s (make-object separator-snip%)])
          (send s set-style (get-style))
          s))
      
      (define height 1)
      (define white-around 2)
      
      (define (get-extent dc x y w-box h-box descent-box space-box lspace-box rspace-box)
        (for-each (lambda (box) (unless (not box) (set-box! box 0)))
                  (list descent-box space-box lspace-box rspace-box))
        (unless (not w-box)
          (set-box! w-box width))
        (unless (not h-box)
          (set-box! h-box (+ (* 2 white-around) height))))
      
      (define body-pen 
        (send the-pen-list find-or-create-pen
              "BLUE" 0 'solid))
      (define body-brush 
        (send the-brush-list find-or-create-brush
              "BLUE" 'solid))
      
      (define (draw dc x y left top right bottom dx dy draw-caret)
            (let ([orig-pen (send dc get-pen)]
                  [orig-brush (send dc get-brush)])
              (send dc set-pen body-pen)
              (send dc set-brush body-brush)
              
              (send dc draw-rectangle (+ x 1)
                    (+ white-around y) width height)
              
              (send dc set-pen orig-pen)
              (send dc set-brush orig-brush)))
      
      (super-instantiate ())
      (set-flags (cons 'hard-newline (get-flags)))
      (set-snipclass separator-snipclass)))

  ;;;; end of copied region

  ;;;; duplicated for vertical-snip
  
  (define red-arrow-bitmap
    (make-object bitmap% (build-path (collection-path "icons") "red-arrow.bmp") 'bmp))

  (unless (send red-arrow-bitmap ok?)
    (error 'red-arrow-bitmap "unable to load red-arrow bitmap"))
  
  (define vertical-separator-snipclass
    (make-object
     (class snip-class% ()
       (override read)
       
       (define (read s)
         (let ([size-box (box 0)])
           (send s get size-box)
           (make-object vertical-separator-snip% 100)))
       
       (super-instantiate ()))))
  
  (send* separator-snipclass
    (set-version 1)
    (set-classname "drscheme:vertical-separator-snip%"))
  
  (send (get-the-snip-class-list) add vertical-separator-snipclass)
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define vertical-separator-snip%
    (class snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (public set-height!)
      (override write copy get-extent draw)
      
      (init-field height)
      

      (define bitmap-width 15.0)
      (define left-white 0.0)
      (define right-white 3.0)
      (define bitmap-height 10.0)
      
      (define (set-height! x) 
        (set! height (max x bitmap-height)))
      
      (define (write s) 
        (send s put (char->integer #\r)))        ; this can't be right...?
      
      (define (copy)
        (let ([s (make-object vertical-separator-snip% height)])
          (send s set-style (get-style))
          s))
      
      (define (get-extent dc x y w-box h-box descent-box space-box lspace-box rspace-box)
        (for-each (lambda (box) (unless (not box) (set-box! box 0)))
                  (list descent-box space-box lspace-box rspace-box))
        (unless (not w-box)
          (set-box! w-box (+ left-white right-white bitmap-width)))
        (unless (not h-box)
          (set-box! h-box height)))
      
      (define (draw dc x y left top right bottom dx dy draw-caret)
        (let ([y-offset (round (/ (- height bitmap-height) 2))]
              [x-offset left-white])
          (send dc draw-bitmap red-arrow-bitmap (+ x x-offset) (+ y y-offset))))
      
      (super-instantiate ())
      (set-snipclass vertical-separator-snipclass)))
  
  
  ;;;; end of vertical snip-stuff
  
  (define stepper-editor-snip%
    (class editor-snip% ()
      (inherit get-editor set-min-width set-max-width)
      (public*
        [set-new-width
         (lambda (width canvas)
           (set-min-width width)
           (set-max-width width)
           (let ([editor (get-editor)])
             (when editor
               (send editor reset-pretty-print-width width canvas))))])
      (super-instantiate (#f #f 0 0 0 0 0 0 0 0))))
                                                                                                              
                                                                  ;                                    ;;    ;
       ;                                                          ;              ;                 ;  ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;         ;;;  ;   ;  ; ;;;         ;;;;  ;;;  ;    ; ;;;;;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;     ;   ;  ;;   ;         ;   ;   ;  ;  ;   ;   ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;     ;   ;  ;    ;         ;   ;   ;   ;;    ;     ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;   ;;   ;   ;  ;    ;  ;;;;;  ;   ;;;;;   ;;    ;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;              ;  ;   ;  ;    ;         ;   ;       ;;    ;    ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;              ;  ;  ;;  ;;   ;         ;   ;      ;  ;   ;   ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;           ;;;    ;; ;  ; ;;;           ;;  ;;;; ;    ;   ;;;    ;; 
                  ;       ;                                                                                   
                  ;       ;                                                                                   
  
  ; the stepper-sub-text% class is used to hold an individual list of sexps, with one or more highlights.
  ; there are four of them in the stepper window.
  
  (define stepper-sub-text%
    (class f:text:standard-style-list% ()
      
      (init-field exps highlight-color)

      (inherit insert get-style-list set-style-list change-style highlight-range last-position lock erase
               begin-edit-sequence end-edit-sequence get-start-position select-all clear)
      
      (public* [reset-pretty-print-width
                (lambda (inner-width canvas)
                  (begin-edit-sequence)
                  (let* ([style (send (get-style-list) find-named-style "Standard")]
                         [char-width (send style get-text-width (send canvas get-dc))]
                         [width (floor (/ (- inner-width 18) char-width))])
                    (reformat-sexp width)
                    (end-edit-sequence)))])
      
      (define highlight-table (make-hash-table 'weak))
      
      (define stripped-exps
        (map (lambda (exp) (strip-to-sexp exp highlight-table)) exps))
      
      (define pretty-printed-width #f)
      (define clear-highlight-thunks null)
      (define/private (reset-style)
        (change-style (send (get-style-list) find-named-style "Standard")))
      (define/private (set-last-style)
        (change-style (send (get-style-list) find-named-style "Standard")
		      (sub1 (last-position))
		      (last-position)))
               
      (define/private (reformat-sexp width)
        (when (not (eq? pretty-printed-width width))
          (set! pretty-printed-width width)
          (format-whole-step)))
               
      (define highlight-begin #f)
               
      (inherit get-dc)
      
      (define/private (format-sexp sexp)
        (define text-port (open-output-text-editor this))

        (parameterize ([pretty-print-columns pretty-printed-width]
                       
                       ; the pretty-print-size-hook decides whether this object should be printed by the new pretty-print-hook
                       [pretty-print-size-hook
                        (lambda (value display? port)
                          (let ([looked-up (hash-table-get highlight-table value (lambda () #f))])
                            (cond
                              [(is-a? value snip%) 
			       ;; Calculate the effective width of the snip, so that
			       ;;  too-long lines (as a result of large snips) are broken
			       ;;  correctly. When the snip is actusally inserted, its width
			       ;;  will be determined by `(send snip get-count)', but the number
			       ;;  returned here triggers line breaking in the pretty printer.
			       (let ([dc (get-dc)]
                                     [wbox (box 0)])
                                 (send value get-extent dc 0 0 wbox #f #f #f #f #f)
                                 (let-values ([(xw dc dc2 dc3) (send dc get-text-extent "x")])
                                   (max 1 (inexact->exact (ceiling (/ (unbox wbox) xw))))))]
                              [(and looked-up (not (eq? looked-up 'non-confusable)))
                               (string-length (format "~s" (car looked-up)))]
                              [else #f])))]
                       
                       [pretty-print-print-hook
                        ; this print-hook is called for confusable highlights and for images.
                        (lambda (value display? port)
                          (let ([to-display (cond 
					     [(hash-table-get highlight-table value (lambda () #f)) => car]
					     [else value])])
                            (cond 
			     [(is-a? to-display snip%) 
			      (write-special (send to-display copy) port) (set-last-style)]
			     [else 
			      (write-string (format "~s" to-display) port)])))]
                       [pretty-print-print-line
                        (lambda (number port old-length dest-columns)
                          (when (and number (not (eq? number 0)))
                            (newline port))
                          0)]
                       [pretty-print-pre-print-hook
                        (lambda (value p)
                          (when (hash-table-get highlight-table value (lambda () #f))
                            (set! highlight-begin (get-start-position))))]
                       [pretty-print-post-print-hook
                        (lambda (value p)
                          (when (hash-table-get highlight-table value (lambda () #f))
                            (let ([highlight-end (get-start-position)])
                              (unless highlight-begin
                                (error 'format-whole-step "no highlight-begin to match highlight-end"))
                              (set! clear-highlight-thunks
                                    (cons (highlight-range highlight-begin highlight-end highlight-color #f #f)
                                          clear-highlight-thunks))
                              (set! highlight-begin #f))))]
                       ;; mflatt: MAJOR HACK - this setting needs to come from the language
                       ;;  somehow
                       [read-case-sensitive #t])
          (pretty-print sexp text-port)))
             
      (define/public (format-whole-step)
        (lock #f)
        (begin-edit-sequence)
        (for-each (lambda (fun) (fun)) clear-highlight-thunks)
        (set! clear-highlight-thunks null)
        (select-all)
        (clear)
        (reset-style)
        (let loop ([remaining stripped-exps] [first #t])
          (unless (null? remaining)
            (unless first (insert #\newline))
            (format-sexp (car remaining))
            (loop (cdr remaining) #f)))
        (end-edit-sequence)
        (lock #t))
      
      (super-instantiate ())))
  
                                                   
                                                                  ;                                                                        ;;    ;
       ;                                                          ;                                                  ;                 ;  ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;         ;;;  ;   ;  ; ;;;           ;;;   ; ;; ; ;;  ;;;   ; ;;       ;;;;  ;;;  ;    ; ;;;;;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;     ;   ;  ;;   ;         ;   ;  ;;   ;;   ;   ;  ;;          ;   ;   ;  ;  ;   ;   ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;     ;   ;  ;    ;         ;   ;  ;    ;    ;   ;  ;           ;   ;   ;   ;;    ;     ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;   ;;   ;   ;  ;    ;  ;;;;;  ;;;;;  ;    ;    ;   ;  ;    ;;;;;  ;   ;;;;;   ;;    ;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;              ;  ;   ;  ;    ;         ;      ;    ;    ;   ;  ;           ;   ;       ;;    ;    ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;              ;  ;  ;;  ;;   ;         ;      ;    ;    ;   ;  ;           ;   ;      ;  ;   ;   ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;           ;;;    ;; ;  ; ;;;           ;;;;  ;    ;     ;;;   ;            ;;  ;;;; ;    ;   ;;;    ;; 
                  ;       ;                                                                                                                       
                  ;       ;                                                                                                                       
  
  ; the stepper-sub-error-text%, like stepper-sub-text%, fits in one of the four stepper "text" spots.
  ; it is used for error messages.
  
  (define stepper-sub-error-text%
    (class f:text:standard-style-list% ()
  
      (init-field error-msg)
      
      (inherit get-style-list last-position set-style-list insert change-style auto-wrap
               set-max-width)
      
      (public* [reset-pretty-print-width 
                (lambda (inner-width canvas)
                  (set-max-width inner-width))])
      
      (super-instantiate ())
      (let ([before-error-msg (last-position)])
        (change-style (send (get-style-list) find-named-style "Standard"))
        (auto-wrap #t)
        (insert error-msg)
        (change-style error-delta before-error-msg (last-position)))))
                                                                                                   
                                                                                            ;;    ;
       ;                                                                                   ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;         ;;;   ;;;   ; ;;  ;   ;   ;;;    ;;; ;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;     ;   ;  ;;  ; ;   ;  ;   ;  ;     ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;         ;  ;   ;  ; ;       ;  ;       ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;  ;      ;;;;  ;   ;  ; ;    ;;;;   ;;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;           ;     ;   ;  ;   ;  ; ;   ;   ;     ;   ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;           ;     ;   ;  ;   ;  ;;    ;   ;     ;  ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;            ;;;   ;;;;; ;   ;   ;     ;;;;; ;;;  ;    ;; 
                  ;       ;                                                                        
                  ;       ;                                                                        
                                                                                                   
  (define stepper-canvas%
    (class editor-canvas% ()
      (inherit get-editor)
      (override*
        [on-size
         (lambda (width height)
           (super on-size width height)
           (let ([editor (get-editor)])
             (when editor
               (send editor reset-width this))))])
 
      (super-instantiate ())))
                                                                                  
                                                                           ;;    ;
       ;                                             ;                 ;  ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;       ;;;;  ;;;  ;    ; ;;;;;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;   ;   ;  ;  ;   ;   ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;   ;   ;   ;;    ;     ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;  ;   ;;;;;   ;;    ;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;           ;   ;       ;;    ;    ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;           ;   ;      ;  ;   ;   ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;            ;;  ;;;; ;    ;   ;;;    ;; 
                  ;       ;                                                       
                  ;       ;                                                       
    
  ; the stepper-text% is the principal inhabitant of the stepper window. It keeps
  ; track of all of the sexps & error messages in a given step, reformatting as necessary.
  
  ; constructor : ((listof sexp) (listof sexp) (listof sexp) (union string #f) (listof sexp) -> )
  
  (define stepper-text%
    (class f:text:standard-style-list% ()
      
      (init-field finished-exprs exps post-exps error-msg after-exprs)

      (inherit find-snip insert change-style highlight-range last-position lock erase auto-wrap
               begin-edit-sequence end-edit-sequence get-start-position get-style-list set-style-list
               get-admin get-snip-location get-dc needs-update hide-caret)
      (public* [reset-width 
                (lambda (canvas)
                  (lock #f)
                  (begin-edit-sequence)
                  (let* ([width-box (box 0)]
                         [canvas-width (begin (send (get-admin) get-view #f #f width-box #f) (unbox width-box))]
                         [dc (send canvas get-dc)])
                    (unless (and old-width (= canvas-width old-width))
                      (set! old-width canvas-width)
                      (let* ([minus-cursor-margin (- canvas-width 2)]
                             [vert-separator-width-box (box 0)]
                             [_ (send vert-separator get-extent dc 0 0 vert-separator-width-box
                                      #f #f #f #f #f)]
                             [vert-separator-width (unbox vert-separator-width-box)]
                             [minus-center-bar (- minus-cursor-margin vert-separator-width)]
                             [l-r-box-widths (floor (/ minus-center-bar 2))])
                        (send top-defs-snip set-new-width minus-cursor-margin canvas)
                        (send before-snip set-new-width l-r-box-widths canvas)
                        (send after-snip set-new-width l-r-box-widths canvas)
                        (send bottom-defs-snip set-new-width minus-cursor-margin canvas)
                        (coordinate-snip-sizes)
                        (send horiz-separator-1 reset-width)
                        (send horiz-separator-2 reset-width)))
                    (end-edit-sequence)
                    (lock #t)))])

      (define old-width #f)
      (define top-defs-snip (make-object stepper-editor-snip%))
      (define horiz-separator-1 (make-object separator-snip%))
      (define before-snip (make-object stepper-editor-snip%))
      (define vert-separator (make-object vertical-separator-snip% 10))
      (define after-snip (make-object stepper-editor-snip%))
      (define horiz-separator-2 (make-object separator-snip%))
      (define bottom-defs-snip (make-object stepper-editor-snip%))
      (define/private (release-snip-sizes)
        (for-each (lambda (snip)
                    (send snip set-min-height 0.0)
                    (send snip set-max-height 0.0)
                    (send snip set-max-height 'none))
                  (list before-snip after-snip)))
      (define/private (coordinate-snip-sizes)
        (let* ([get-snip-height
                (lambda (snip)
                  (let* ([top-box (box 0)]
                         [bottom-box (box 0)])
                    (get-snip-location snip #f top-box #f)
                    (get-snip-location snip #f bottom-box #t)
                    (- (unbox bottom-box) (unbox top-box))))]
               [max-height (apply max (map get-snip-height (list before-snip after-snip)))])
          (send vert-separator set-height! (- max-height 4))
          (let ([w-box (box 0)]
                [h-box (box 0)])
            (send vert-separator get-extent #f 0 0 w-box h-box #f #f #f #f)
            (send (send vert-separator get-admin) resized vert-separator #t))))
      
      

      (super-instantiate ())
      (hide-caret #t)
      (let ([before-position (last-position)])
        (for-each (lambda (x) (insert x)) (list top-defs-snip (string #\newline) horiz-separator-1
                               before-snip vert-separator 
                               after-snip (string #\newline) 
                               horiz-separator-2 bottom-defs-snip))
        (change-style snip-delta before-position (last-position)))
      (send top-defs-snip set-editor 
            (make-object stepper-sub-text% finished-exprs #f))
      (send before-snip set-editor
            (make-object stepper-sub-text% exps redex-highlight-color))
      (if (eq? error-msg #f)
          (send after-snip set-editor
                (make-object stepper-sub-text% post-exps reduct-highlight-color))
          (send after-snip set-editor
                (make-object stepper-sub-error-text% error-msg)))
      (send bottom-defs-snip set-editor
            (make-object stepper-sub-text% after-exprs #f))
      (lock #t)))
  
  (define finished-text
    (instantiate (class text% ()
                   (define/public (reset-width arg)
                     (void))
                   
                   (super-instantiate ())
                   
                   (inherit insert lock)
                   (insert "All of the definitions have been successfully evaluated.")
                   (lock #t)) 
      ()))
  
  
  (define (snip? val)
    (is-a? val snip%))
  
  (define warning-color "yellow")
  (define warning-font (send the-font-list find-or-create-font 18 'decorative 'normal 'bold #f))
  
  (define stepper-warning%
    (class canvas%
      
      (init-field warning-str)
       
      (inherit get-dc get-client-size)
      (define/override (on-paint)
        (let ([dc (get-dc)])
          (send dc set-font warning-font) 
          (let-values ([(cw ch) (get-client-size)]
                       [(tw th dont-care dont-care2) (send dc get-text-extent warning-str)])
            (send dc set-pen (send the-pen-list find-or-create-pen warning-color 1 'solid))
            (send dc set-brush (send the-brush-list find-or-create-brush warning-color 'solid))
            (send dc draw-rectangle 0 0 cw ch)
            (send dc draw-text 
                  warning-str
                  (- (/ cw 2) (/ tw 2))
                  (- (/ ch 2) (/ th 2))))))
      
      (super-instantiate ())
      (inherit min-width min-height stretchable-height)
      (let-values ([(tw th dc dc2) (send (get-dc) get-text-extent warning-str warning-font)])
        (min-width (+ 2 (inexact->exact (ceiling tw))))
        (min-height (+ 2 (inexact->exact (ceiling th)))))
      
      (stretchable-height #f)))

  
  ;                                                                                          
  ;                                                                                          
  ;                                                                                          
  ;                    ;                                                                     
  ;                                                                                          
  ;          ;                             ;                                                 
  ;    ;;;  ;;;;  ; ;  ;   ; ;;           ;;;;   ;;;             ;;;    ;;;  ;     ;  ; ;;   
  ;   ;      ;    ;;   ;   ;;  ;           ;    ;   ;           ;      ;   ;  ;   ;   ;;  ;  
  ;   ;;     ;    ;    ;   ;    ;          ;   ;     ;          ;;    ;    ;   ; ;    ;    ; 
  ;    ;;    ;    ;    ;   ;    ;  ;;;;;;  ;   ;     ;  ;;;;;;   ;;   ;;;;;;    ;     ;    ; 
  ;      ;   ;    ;    ;   ;    ;          ;   ;     ;             ;  ;        ; ;    ;    ; 
  ;      ;   ;    ;    ;   ;;  ;           ;    ;   ;              ;   ;      ;   ;   ;;  ;  
  ;   ;;;     ;;  ;    ;   ; ;;             ;;   ;;;            ;;;     ;;;; ;     ;  ; ;;   
  ;                        ;                                                          ;      
  ;                        ;                                                          ;      
  ;                        ;                                                          ;      
  
  
  ; strip-to-sexp transforms a syntax-object to an s-expression.  The reason we can't
  ; just use syntax-object->datum for this is that certain syntax-objects must be
  ; represented by a gensym'ed pointer into a table.
                           
  (define (strip-to-sexp stx highlight-table)
    (define (strip-regular stx)
      (let* ([it (if (and (syntax? stx)
                          (eq? (syntax-property stx 'stepper-hint) 'from-xml))
                     (strip-xml stx)
                     stx)]
             [it
              (cond [(pair? it)
                     (cons (strip-regular (car it))
                           (strip-regular (cdr it)))]
                    [(syntax? it)
                     (strip-regular (syntax-e it))]
                    [else it])]
             [it
              (if (and (syntax? stx)
                       (syntax-property stx 'stepper-highlight))
                  (if (pair? it) 
                      (begin
                        (hash-table-put! highlight-table it 'non-confusable)
                        it)
                      (let ([new-sym (gensym "-placeholder")])
                        (hash-table-put! highlight-table new-sym (list it))
                        new-sym))
                  it)])
        it))
    
    ; strip-xml attempts to undo the expansion of quasiquote.  
    (define (strip-xml stx)
      ; we'll work on this later.
      "<xml box here>"
      ;(instantiate xml-snip% () [eliminate-whitespace-in-tags? #t])
      )
    
    (strip-regular stx))
                                                                     

  
  (define (stepper-text-test . args)
    (let* ([new-frame (make-object frame% "test-frame")]
           [new-text (apply make-object stepper-text% args)]
           [new-canvas (make-object stepper-canvas% new-frame new-text)])
      (send new-canvas min-width 800)
      (send new-canvas min-height 200)
      (send new-frame show #t)
      (send new-text reset-width new-canvas)
      new-canvas))
  
  
;  (define a
;    (stepper-text-test (build-stx-with-highlight `((define x 3) 14 15 #f 1))
;                       (build-stx-with-highlight `((* 13 (hilite (* 15 16)))))
;                       (build-stx-with-highlight `((hilite (+ 3 4)) (define y 4) 13 14 (+  (hilite 13) (hilite #f)) 13 
;                                                   298 1 1 (+ (x 398 (hilite (+ x 398))) (hilite (x 398 (+ x 398)))) (hilite #f)))
;                       #f
;                       (build-stx-with-highlight `((define y (+ 13 14)) 80))))
  
  
  
;  (stepper-text-test `() `(uninteresting but long series of lines) `() "This is an error message" `((define x 3 4 5)))
  
; (stepper-text-test  `() `() `() "This is another error message" `(poomp))

  )

