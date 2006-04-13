;; originally by Dan Grossman
;; 6/30/95

(module scheme mzscheme
  (require "collapsed-snipclass-helpers.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred")
           (lib "mred.ss" "mred")
	   (lib "list.ss")
	   (lib "thread.ss")
           (lib "etc.ss")
           (lib "surrogate.ss")
           (lib "scheme-lexer.ss" "syntax-color"))
  
  (provide scheme@)

  (define (scheme-paren:get-paren-pairs)
    '(("(" . ")")
      ("[" . "]")
      ("{" . "}")))
  
  
  (define scheme@
    (unit/sig framework:scheme^
      (import mred^
              [preferences : framework:preferences^]
              [icon : framework:icon^]
              [keymap : framework:keymap^]
              [text : framework:text^]
              [editor : framework:editor^]
              [frame : framework:frame^]
              [comment-box : framework:comment-box^]
              [mode : framework:mode^]
              [color : framework:color^]
              [color-prefs : framework:color-prefs^])
      
      (rename [-text-mode<%> text-mode<%>]
              [-text<%> text<%>]
              [-text% text%])
      
      (define text-balanced? 
        (opt-lambda (text [start 0] [in-end #f])
          (let* ([end (or in-end (send text last-position))]
                 [port (open-input-text-editor text start end)])
            (with-handlers ([exn:fail:read:eof? (λ (x) #f)]
                            [exn:fail:read? (λ (x) #t)])
              (let loop ()
                (let ([s (read port)])
                  (or (eof-object? s)
                      (loop))))))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                                  ;;
      ;;                           Sexp Snip                              ;;
      ;;                                                                  ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (set-box/f! b v) (when (box? b) (set-box! b v)))

      (define sexp-snip<%>
        (interface ()
          get-saved-snips))

      (define sexp-snip%
        (class* snip% (sexp-snip<%> readable-snip<%>)
          (init-field left-bracket right-bracket saved-snips)
          (define/public (get-saved-snips) saved-snips)
          (field [sizing-text (format "~a   ~a" left-bracket right-bracket)])

          (define/public (read-special file line col pos)
            (let ([text (make-object text:basic%)])
              (for-each
               (λ (s) (send text insert (send s copy)
                                 (send text last-position)
                                 (send text last-position)))
               saved-snips)
	      (datum->syntax-object
	       #f
	       (read (open-input-text-editor text))
	       (list file line col pos 1))))
          
          (define/override get-text
            (opt-lambda (offset num [flattened? #f])
              (if flattened?
                  (apply string-append
                         (map (λ (snip)
                                (send snip get-text 0 (send snip get-count) flattened?))
                              saved-snips))
                  (super get-text offset num flattened?))))
              
          (define/override (copy)
            (instantiate sexp-snip% ()
              (left-bracket left-bracket)
              (right-bracket right-bracket)
              (saved-snips saved-snips)))
          
          (define/override (write stream-out)
            (send stream-out put (bytes (char->integer left-bracket)))
            (send stream-out put (bytes (char->integer right-bracket)))
            (send stream-out put (length saved-snips))
            (let loop ([snips saved-snips])
              (cond
                [(null? snips) (void)]
                [else
                 (let* ([snip (car snips)]
                        [snipclass (send snip get-snipclass)])
                   (send stream-out put (string->bytes/utf-8 (send snipclass get-classname)))
                   (send snip write stream-out))
                 (loop (cdr snips))])))
          
          (define/override (draw dc x y left top right bottom dx dy draw-caret)
            (send dc draw-text sizing-text x y)
            (let-values ([(lpw lph lpa lpd) (send dc get-text-extent (string left-bracket))]
                         [(rpw rph rpa rpd) (send dc get-text-extent (string right-bracket))]
                         [(sw sh sa sd) (send dc get-text-extent sizing-text)])
              (let* ([dtw (- sw lpw rpw)]
                     [dot-start (+ x lpw)]
                     [dt1x (+ dot-start (* dtw 1/5))]
                     [dt2x (+ dot-start (* dtw 1/2))]
                     [dt3x (+ dot-start (* dtw 4/5))]
                     [dty (+ y (/ sh 2))])
                (send dc draw-rectangle dt1x dty 2 2)
                (send dc draw-rectangle dt2x dty 2 2)
                (send dc draw-rectangle dt3x dty 2 2))))

          (inherit get-style)
          (define/override (get-extent dc x y wb hb descentb spaceb lspaceb rspaceb)
            (let-values ([(w h d a) (send dc get-text-extent sizing-text (send (get-style) get-font))])
              (set-box/f! wb w)
              (set-box/f! hb h)
              (set-box/f! descentb d)
              (set-box/f! spaceb a)
              (set-box/f! lspaceb 0)
              (set-box/f! rspaceb 0)))
          (super-instantiate ())
          (inherit set-snipclass)
          (set-snipclass lib-snip-class)))

      (define sexp-snipclass% (make-sexp-snipclass% sexp-snip%))
      
      ;; old snips (from old versions of drscheme) use this snipclass
      (define lib-snip-class (make-object sexp-snipclass%))
      (send lib-snip-class set-classname (format "~s" '(lib "collapsed-snipclass.ss" "framework")))
      (send lib-snip-class set-version 0)
      (send (get-the-snip-class-list) add lib-snip-class)

      ;; new snips use this snipclass
      (define old-snip-class (make-object sexp-snipclass%))
      (send old-snip-class set-classname "drscheme:sexp-snip")
      (send old-snip-class set-version 0)
      (send (get-the-snip-class-list) add old-snip-class)

      (keymap:add-to-right-button-menu
       (let ([old (keymap:add-to-right-button-menu)])
         (λ (menu text event)
           (old menu text event)
           (split/collapse-text menu text event)
	   (void))))
      
      ;; split/collapse-text : (instanceof menu%) (instanceof editor<%>) (instanceof mouse-event%) -> void
      (define (split/collapse-text menu text event)
        (when (and (is-a? text -text<%>)
                   (not (send text is-frozen?))
                   (not (send text is-stopped?)))
          (let* ([on-it-box (box #f)]
                 [click-pos 
                  (call-with-values
                   (λ ()
                     (send text dc-location-to-editor-location
                           (send event get-x)
                           (send event get-y)))
                   (λ (x y)
                     (send text find-position x y #f on-it-box)))]
                 [snip (send text find-snip click-pos 'after)]
                 [char (send text get-character click-pos)]
                 [left? (memq char '(#\( #\{ #\[))]
                 [right? (memq char '(#\) #\} #\]))])
            (cond
              [(and snip (is-a? snip sexp-snip<%>))
               (make-expand-item text snip menu)]
              [(not (unbox on-it-box))
               ;; clicking in nowhere land, just ignore
               (void)]
              [(or left? right?)
               ;; clicking on left or right paren
               (let* ([pos (if left?
                               click-pos
                               (+ click-pos 1))]
                      [other-pos (if left?
                                     (send text get-forward-sexp pos)
                                     (send text get-backward-sexp pos))])
                 (when other-pos
                   (let ([left-pos (min pos other-pos)]
                         [right-pos (max pos other-pos)])
                     (make-collapse-item text left-pos right-pos menu))))]
              [else 
               ;; clicking on some other text -> collapse containing sexp
               (let ([up-sexp (send text find-up-sexp click-pos)])
                 (when up-sexp 
                   (let ([fwd (send text get-forward-sexp up-sexp)])
                     (make-collapse-item text up-sexp fwd menu))))]))))
      
      ;; make-expand-item : (instanceof text%) (instanceof sexp-snip<%>) (instanceof menu%) -> void
      (define (make-expand-item text snip menu)
        (instantiate separator-menu-item% ()
          (parent menu))
        (instantiate menu-item% ()
          (parent menu)
          (label (string-constant expand-sexp))
          (callback (λ (item evt) (expand-from text snip)))))
      
      ;; expand-from : (instanceof text%) (instanceof sexp-snip<%>) -> void
      (define (expand-from text snip)
        (let ([snips (send snip get-saved-snips)])
          (send text begin-edit-sequence)
          (let ([pos (send text get-snip-position snip)])
            (send text delete pos (+ pos 1))
            (let loop ([snips (reverse snips)])
              (cond
                [(null? snips) (void)]
                [else (send text insert (send (car snips) copy) pos pos)
                      (loop (cdr snips))])))
          (send text end-edit-sequence)))

      ;; make-collapse-item : (instanceof text%) number number (instanceof menu%) -> void
      ;; adds a collapse menu item to the menu
      (define (make-collapse-item text left-pos right-pos menu)
        (instantiate separator-menu-item% ()
          (parent menu))
        (instantiate menu-item% ()
          (parent menu)
          (label (string-constant collapse-sexp))
          (callback (λ (item evt)
                      (collapse-from text left-pos right-pos)))))
      
      (define (collapse-from text left-pos right-pos)
        (let ([left-bracket (send text get-character left-pos)]
              [right-bracket (send text get-character (- right-pos 1))])
          (send text begin-edit-sequence)
          (send text split-snip left-pos)
          (send text split-snip right-pos)
          (let ([snips (let loop ([snip (send text find-snip left-pos 'after)])
                         (cond
                           [(not snip) null]
                           [((send text get-snip-position snip) . >= . right-pos)
                            null]
                           [else (cons (send snip copy) (loop (send snip next)))]))])
            (send text delete left-pos right-pos)
            (send text insert (instantiate sexp-snip% () 
                                (left-bracket left-bracket)
                                (right-bracket right-bracket)
                                (saved-snips snips))
                  left-pos left-pos)
            (send text end-edit-sequence))))
      

                                                                             
              ;;                                                             
               ;                                   ;                    ;    
               ;                                   ;                    ;    
  ;;;    ;;;   ; ;;    ;;;  ;;; ;    ;;;          ;;;;;   ;;;  ;;; ;;; ;;;;; 
 ;   ;  ;   ;  ;;  ;  ;   ;  ; ; ;  ;   ;    ;     ;     ;   ;   ; ;    ;    
  ;;;   ;      ;   ;  ;;;;;  ; ; ;  ;;;;;          ;     ;;;;;    ;     ;    
     ;  ;      ;   ;  ;      ; ; ;  ;              ;     ;       ; ;    ;    
 ;   ;  ;   ;  ;   ;  ;   ;  ; ; ;  ;   ;          ;   ; ;   ;  ;   ;   ;   ;
  ;;;    ;;;  ;;; ;;;  ;;;  ;; ; ;;  ;;;     ;      ;;;   ;;;  ;;   ;;   ;;; 
                                                                             
 
      (define color-prefs-table
        (let ([constant-green (make-object color% 41 128 38)]
              [symbol-blue (make-object color% 38 38 128)])
          `((symbol ,symbol-blue ,(string-constant scheme-mode-color-symbol))
            (keyword ,symbol-blue ,(string-constant scheme-mode-color-keyword))
            (comment ,(make-object color% 194 116 31) ,(string-constant scheme-mode-color-comment))
            (string ,constant-green ,(string-constant scheme-mode-color-string))
            (constant ,constant-green ,(string-constant scheme-mode-color-constant))
            (parenthesis ,(make-object color% "brown") ,(string-constant scheme-mode-color-parenthesis))
            (error ,(make-object color% "red") ,(string-constant scheme-mode-color-error))
            (other ,(make-object color% "black") ,(string-constant scheme-mode-color-other)))))
      (define (get-color-prefs-table) color-prefs-table)
      
      (define (short-sym->pref-name sym) (string->symbol (short-sym->style-name sym)))
      (define sn-hash (make-hash-table))
      (define (short-sym->style-name sym)
	(hash-table-get sn-hash sym
			(λ ()
			  (let ([s (format "framework:syntax-coloring:scheme:~a" sym)])
			    (hash-table-put! sn-hash sym s)
			    s))))
      
      (define (add-coloring-preferences-panel)
        (color-prefs:add-to-preferences-panel
         "Scheme"
         (λ (parent)
           (for-each
            (λ (line)
              (let ([sym (car line)])
                (color-prefs:build-color-selection-panel 
                 parent
                 (short-sym->pref-name sym)
                 (short-sym->style-name sym)
                 (format "~a" sym))))
            color-prefs-table))))
      
      (define-struct string/pos (string pos))
      
      (define -text<%>
        (interface (text:basic<%> mode:host-text<%> color:text<%>)
          get-limit
          balance-parens
          tabify-on-return?
          tabify
          tabify-selection
          tabify-all
          insert-return
          box-comment-out-selection
          comment-out-selection
          uncomment-selection
          get-forward-sexp
          remove-sexp
          forward-sexp
          flash-forward-sexp
          get-backward-sexp
          flash-backward-sexp
          backward-sexp
          find-up-sexp
          up-sexp
          find-down-sexp
          down-sexp
          remove-parens-forward
          
          select-forward-sexp
          select-backward-sexp
          select-up-sexp
          select-down-sexp
          transpose-sexp
          mark-matching-parenthesis
          get-tab-size
          set-tab-size
          
          introduce-let-ans
          move-sexp-out))
      
      (define init-wordbreak-map
        (λ (map)
          (let ([v (send map get-map #\-)])
            (send map set-map 
                  #\-
                  '(line)))))
      (define wordbreak-map (make-object editor-wordbreak-map%))
      (define (get-wordbreak-map) wordbreak-map)
      (init-wordbreak-map wordbreak-map)
      
      (define matching-parenthesis-style 
        (let ([matching-parenthesis-delta (make-object style-delta% 'change-bold)]
              [style-list (editor:get-standard-style-list)])
          (send matching-parenthesis-delta set-delta-foreground "forest green")
          (send style-list new-named-style "Matching Parenthesis Style"
                (send style-list find-or-create-style
                      (send style-list find-named-style "Standard")
                      matching-parenthesis-delta))
          (send style-list find-named-style "Matching Parenthesis Style")))

      (define text-mixin
        (mixin (text:basic<%> mode:host-text<%> color:text<%>) (-text<%>)
          (inherit begin-edit-sequence
                   delete
                   end-edit-sequence
                   local-edit-sequence?
                   find-string
                   get-character
                   get-keymap
                   get-text
                   get-start-position
                   get-style-list
                   get-end-position
                   flash-on
                   insert
                   kill
                   last-position
                   paragraph-start-position
                   paragraph-end-position
                   position-paragraph
                   set-keymap
                   set-load-overwrites-styles
                   set-position
                   set-wordbreak-map
                   set-tabs
                   set-style-list
                   set-styles-fixed
                   change-style
                   get-snip-position
                   backward-match
                   backward-containing-sexp
                   forward-match
                   skip-whitespace
                   insert-close-paren
		   classify-position)
          
          (inherit get-styles-fixed)
          (inherit has-focus? find-snip split-snip)
          
          (public get-limit tabify-on-return? tabify
                  tabify-all insert-return calc-last-para 
                  box-comment-out-selection comment-out-selection uncomment-selection
                  get-forward-sexp remove-sexp forward-sexp flash-forward-sexp get-backward-sexp
                  flash-backward-sexp backward-sexp find-up-sexp up-sexp find-down-sexp down-sexp
                  remove-parens-forward)
          (define (get-limit pos) 0)
          
          (define/public (balance-parens key-event)
            (insert-close-paren (get-start-position) 
                                (send key-event get-key-code)
                                (preferences:get 'framework:paren-match)
                                (preferences:get 'framework:fixup-parens)))
          
          (define (tabify-on-return?) #t)
          (define tabify    
            (opt-lambda ([pos (get-start-position)])
              (let* ([last-pos (last-position)]
                     [para (position-paragraph pos)]
		     [is-tabbable? (and (> para 0)
					(not (memq (classify-position (sub1 (paragraph-start-position para)))
						   '(comment string error))))]
                     [okay (and is-tabbable? (> para 0))]
                     [end (if okay (paragraph-start-position para) 0)]
                     [limit (get-limit pos)]
		     ;; "contains" is the start of the initial sub-S-exp
		     ;;  in the S-exp that contains "pos". If pos is outside
		     ;;  all S-exps, this will be the start of the initial
		     ;;  S-exp
                     [contains 
                      (if okay
                          (backward-containing-sexp end limit)
                          #f)]
                     [contain-para (and contains
                                        (position-paragraph contains))]
		     ;; "last" is the start of the S-exp just before "pos"
                     [last 
                      (if contains
                          (backward-match end limit)
                          #f)]
                     [last-para (and last
                                     (position-paragraph last))])
                (letrec	
                    ([find-offset
                      (λ (pos)
                        (let loop ([p pos][o 0])
                          (let ([c (get-character p)])
                            (cond
                              [(char=? c #\tab)
                               (loop (add1 p) (+ o (- 8 (modulo o 8))))]
                              [(char=? c #\newline)
                               (cons o p)]
                              [(char-whitespace? c)
                               (loop (add1 p) (add1 o))]
                              [else
                               (cons o p)]))))]
                     [visual-offset
                      (λ (pos)
                        (let loop ([p (sub1 pos)])
                          (if (= p -1)
                              0
                              (let ([c (get-character p)])
                                (cond
                                  [(char=? c #\null) 0]
                                  [(char=? c #\tab)
                                   (let ([o (loop (sub1 p))])
                                     (+ o (- 8 (modulo o 8))))]
                                  [(char=? c #\newline) 0]
                                  [else (add1 (loop (sub1 p)))])))))]
                     [do-indent
                      (λ (amt)
                        (let* ([pos-start end]
                               [curr-offset (find-offset pos-start)])
                          (unless (= amt (car curr-offset))
                            (delete pos-start (cdr curr-offset))
                            (insert
                             (make-string amt #\space)
                             pos-start))))]
                     [get-proc
                      (λ ()
			(let ([id-end (forward-match contains (last-position))])
			  (if (and id-end (> id-end contains))
			      (let* ([text (get-text contains id-end)])
                                (or (get-keyword-type text)
                                    'other)))))]
                     [procedure-indent
                      (λ ()
                        (case (get-proc)
                          [(define) 1]
                          [(begin) 1]
                          [(lambda) 3]
                          [else 0]))]
                     [special-check
                      (λ ()
                        (let* ([proc-name (get-proc)])
                          (or (eq? proc-name 'define)
                              (eq? proc-name 'lambda))))]
                     [indent-first-arg
                      (λ (start)
                        (car (find-offset start)))])
                  (when (and okay
                             (not (char=? (get-character (sub1 end))
                                          #\newline)))
                    (insert #\newline (paragraph-start-position para)))
                  (cond
		    [(not is-tabbable?) (void)]
                    [(let ([real-start (cdr (find-offset end))]) 
                       (and (<= (+ 3 real-start) (last-position))
                            (string=? ";;;"
                                      (get-text real-start
                                                (+ 2 real-start)))))
                     (void)]
                    [(= para 0) (do-indent 0)]
                    [(not contains)
		     ;; Something went wrong matching. Should we get here?
                     (do-indent 0)]
                    [(not last) 
		     ;; We can't find a match backward from pos,
		     ;;  but we seem to be inside an S-exp, so 
		     ;;  go "up" an S-exp, and move forward past
		     ;;  the associated paren
                     (let ([enclosing (find-up-sexp pos)])
                       (do-indent (if enclosing
                                      (+ (visual-offset enclosing) 1)
                                      0)))]
                    [(= contains last)
		     ;; There's only one S-expr in the S-expr
		     ;;  containing "pos"
                     (do-indent (+ (visual-offset contains)
                                   (procedure-indent)))]
                    [(special-check)
		     ;; In case of "define", etc., ignore the position of last 
		     ;;  and just indent under the "define"
                     (do-indent (add1 (visual-offset contains)))]
                    [(= contain-para last-para)
		     ;; So far, the S-exp containing "pos" was all on
		     ;;  one line (possibly not counting the opening paren),
		     ;;  so indent to follow the first S-exp's end
                     (let ([name-length (let ([id-end (forward-match contains (last-position))])
					  (if id-end
					      (- id-end contains)
					      0))])
                       (do-indent (+ (visual-offset contains)
                                     name-length
                                     (indent-first-arg (+ contains 
                                                          name-length)))))]
                    [else
		     ;; No particular special case, so indent to match first 
		     ;; S-expr that start on the previous line
		     (let loop ([last last][last-para last-para])
		       (let* ([next-to-last (backward-match last limit)]
			      [next-to-last-para (and next-to-last
						      (position-paragraph next-to-last))])
			 (if (equal? last-para next-to-last-para)
			     (loop next-to-last next-to-last-para)
			     (do-indent (visual-offset last)))))])))))
          
          (define/public tabify-selection
            (opt-lambda ([start-pos (get-start-position)]
                         [end-pos (get-end-position)])
              (let ([first-para (position-paragraph start-pos)]
                    [end-para (position-paragraph end-pos)])
                (with-handlers ([exn:break?
                                 (λ (x) #t)])
                  (dynamic-wind
                   (λ () 
                     (when (< first-para end-para)
                       (begin-busy-cursor))
                     (begin-edit-sequence))
                   (λ ()
                     (let loop ([para first-para])
                       (when (<= para end-para)
                         (tabify (paragraph-start-position para))
			 (parameterize-break #t (void))
                         (loop (add1 para))))
                     (when (and (>= (position-paragraph start-pos) end-para)
                                (<= (skip-whitespace (get-start-position) 'backward #f)
                                    (paragraph-start-position first-para)))
                       (set-position 
                        (let loop ([new-pos (get-start-position)])
                          (if (let ([next (get-character new-pos)])
                                (and (char-whitespace? next)
                                     (not (char=? next #\newline))))
                              (loop (add1 new-pos))
                              new-pos)))))
                   (λ ()
                     (end-edit-sequence)
                     (when (< first-para end-para)
                       (end-busy-cursor))))))))
          
          (define (tabify-all) (tabify-selection 0 (last-position)))
          (define (insert-return)
            (if (tabify-on-return?)
                (begin 
                  (begin-edit-sequence)
                  (insert #\newline)
                  (tabify (get-start-position))
                  (set-position 
                   (let loop ([new-pos (get-start-position)])
                     (if (let ([next (get-character new-pos)])
                           (and (char-whitespace? next)
                                (not (char=? next #\newline))))
                         (loop (add1 new-pos))
                         new-pos)))
                  (end-edit-sequence))
                (insert #\newline)))
          
          (define (calc-last-para last-pos)
            (let ([last-para (position-paragraph last-pos #t)])
              (if (and (> last-pos 0)
                       (> last-para 0))
                  (begin (split-snip last-pos)
                         (let ([snip (find-snip last-pos 'before)])
                           (if (member 'hard-newline (send snip get-flags))
                               (- last-para 1)
                               last-para)))
                  last-para)))
          
          (define comment-out-selection 
            (opt-lambda ([start-pos (get-start-position)] 
                         [end-pos (get-end-position)]) 
              (begin-edit-sequence) 
              (let ([first-pos-is-first-para-pos? 
                     (= (paragraph-start-position (position-paragraph start-pos)) 
                        start-pos)]) 
                (let* ([first-para (position-paragraph start-pos)] 
                       [last-para (calc-last-para end-pos)]) 
                  (let para-loop ([curr-para first-para]) 
                    (if (<= curr-para last-para) 
                        (let ([first-on-para (paragraph-start-position curr-para)]) 
                          (insert #\; first-on-para) 
                          (para-loop (add1 curr-para)))))) 
                (when first-pos-is-first-para-pos? 
                  (set-position 
                   (paragraph-start-position (position-paragraph (get-start-position))) 
                   (get-end-position)))) 
              (end-edit-sequence) 
              #t)) 
          
          (define box-comment-out-selection
            (opt-lambda ([_start-pos 'start]
                         [_end-pos 'end])
	      (let ([start-pos (if (eq? _start-pos 'start)
				   (get-start-position)
				   _start-pos)]
		    [end-pos (if (eq? _end-pos 'end)
				 (get-end-position)
				 _end-pos)])
		(begin-edit-sequence)
		(split-snip start-pos)
		(split-snip end-pos)
		(let* ([cb (instantiate comment-box:snip% ())]
		       [text (send cb get-editor)])
		  (let loop ([snip (find-snip start-pos 'after-or-none)])
		    (cond
		      [(not snip) (void)]
		      [((get-snip-position snip) . >= . end-pos) (void)]
		      [else
		       (send text insert (send snip copy)
			     (send text last-position)
			     (send text last-position))
		       (loop (send snip next))]))
		  (delete start-pos end-pos)
		  (insert cb start-pos)
		  (set-position start-pos start-pos))
		(end-edit-sequence)
		#t)))
          
          ;; uncomment-box/selection : -> void
          ;; uncomments a comment box, if the focus is inside one.
          ;; otherwise, calls uncomment selection to uncomment
          ;; something else.
          (inherit get-focus-snip)
          (define/public (uncomment-box/selection)
            (begin-edit-sequence)
            (let ([focus-snip (get-focus-snip)])
              (cond
                [(not focus-snip) (uncomment-selection)]
                [(is-a? focus-snip comment-box:snip%) 
                 (extract-contents 
                  (get-snip-position focus-snip)
                  focus-snip)]
                [else (uncomment-selection)]))
            (end-edit-sequence)
            #t)
          
          (define uncomment-selection
            (opt-lambda ([start-pos (get-start-position)]
                         [end-pos (get-end-position)])
              (let ([snip-before (find-snip start-pos 'before-or-none)]
                    [snip-after (find-snip start-pos 'after-or-none)])
                
                (begin-edit-sequence)
                (cond
                  [(and (= start-pos end-pos)
                        snip-before
                        (is-a? snip-before comment-box:snip%))
                   (extract-contents start-pos snip-before)]
                  [(and (= start-pos end-pos)
                        snip-after
                        (is-a? snip-after comment-box:snip%))
                   (extract-contents start-pos snip-after)]
                  [(and (= (+ start-pos 1) end-pos)
                        snip-after
                        (is-a? snip-after comment-box:snip%))
                   (extract-contents start-pos snip-after)]
                  [else
                   (let* ([last-pos (last-position)]
                          [first-para (position-paragraph start-pos)]
                          [last-para (calc-last-para end-pos)])
                     (let para-loop ([curr-para first-para])
                       (when (<= curr-para last-para)
                         (let ([first-on-para
                                (skip-whitespace  (paragraph-start-position curr-para)
                                                  'forward
                                                  #f)])
                           (split-snip first-on-para)
                           (when (and (< first-on-para last-pos)
                                      (char=? #\; (get-character first-on-para))
                                      (is-a? (find-snip first-on-para 'after-or-none) string-snip%))
                             (delete first-on-para (+ first-on-para 1)))
                           (para-loop (add1 curr-para))))))])
                (end-edit-sequence))
              #t))
          
          ;; extract-contents : number (is-a?/c comment-box:snip%) -> void
          ;; copies the contents of the comment-box-snip out of the snip
          ;; and into this editor as `pos'. Deletes the comment box snip
          (define/private (extract-contents pos snip)
            (let ([editor (send snip get-editor)])
              (let loop ([snip (send editor find-snip (send editor last-position) 'before-or-none)])
                (cond
                  [snip
                   (insert (send snip copy) pos)
                   (loop (send snip previous))]
                  [else (void)]))
              (let ([snip-pos (get-snip-position snip)])
                (delete snip-pos (+ snip-pos 1)))
              (set-position pos pos)))
          
          [define get-forward-sexp
            (λ (start-pos)
              (forward-match start-pos (last-position)))]
          [define remove-sexp
            (λ (start-pos)
              (let ([end-pos (get-forward-sexp start-pos)])
                (if end-pos 
                    (kill 0 start-pos end-pos)
                    (bell)))
              #t)]
          [define forward-sexp
            (λ (start-pos)
              (let ([end-pos (get-forward-sexp start-pos)])
                (if end-pos 
                    (set-position end-pos)
                    (bell))
                #t))]
          [define flash-forward-sexp
            (λ (start-pos)
              (let ([end-pos (get-forward-sexp start-pos)])
                (if end-pos 
                    (flash-on end-pos (add1 end-pos))
                    (bell)) 
                #t))]	    
          [define get-backward-sexp
            (λ (start-pos)
              (let* ([limit (get-limit start-pos)]
                     [end-pos
                      (backward-match start-pos limit)]
                     [min-pos
                      (backward-containing-sexp start-pos limit)]
                     [ans
                      (if (and end-pos 
                               (or (not min-pos)
                                   (>= end-pos min-pos)))
                          end-pos
                          #f)])
                ans))]
          [define flash-backward-sexp
            (λ (start-pos)
              (let ([end-pos (get-backward-sexp start-pos)])
                (if end-pos
                    (flash-on end-pos (add1 end-pos))
                    (bell))
                #t))]
          [define backward-sexp
            (λ (start-pos)
              (let ([end-pos (get-backward-sexp start-pos)])
                (if end-pos
                    (set-position end-pos)
                    (bell))
                #t))]
          [define find-up-sexp
            (λ (start-pos)
              (let* ([limit-pos (get-limit start-pos)]
		     [exp-pos
                      (backward-containing-sexp start-pos limit-pos)])
                
                (if (and exp-pos (> exp-pos limit-pos))
		    (let* ([in-start-pos (skip-whitespace exp-pos 'backward #t)]
			   [paren-pos
			    (λ (paren-pair)
			      (find-string
			       (car paren-pair)
			       'backward
			       in-start-pos
			       limit-pos))])
		      (let ([poss (let loop ([parens (scheme-paren:get-paren-pairs)])
				    (cond
				     [(null? parens) null]
				     [else 
				      (let ([pos (paren-pos (car parens))])
					(if pos
					    (cons pos (loop (cdr parens)))
					    (loop (cdr parens))))]))])
			(if (null? poss) ;; all finds failed
			    #f
			    (- (apply max poss) 1)))) ;; subtract one to move outside the paren
                    #f)))]
          [define up-sexp
            (λ (start-pos)
              (let ([exp-pos (find-up-sexp start-pos)])
                (if exp-pos
                    (set-position exp-pos)
                    (bell))
                #t))]
          [define find-down-sexp
            (λ (start-pos)
              (let ([last (last-position)])
                (let loop ([pos start-pos])
                  (let ([next-pos (forward-match pos last)])
                    (if (and next-pos (> next-pos pos))
                        (let ([back-pos
                               (backward-containing-sexp (sub1 next-pos) pos)])
                          (if (and back-pos
                                   (> back-pos pos))
                              back-pos
                              (loop next-pos)))
                        #f)))))]
          [define down-sexp
            (λ (start-pos)
              (let ([pos (find-down-sexp start-pos)])
                (if pos
                    (set-position pos)
                    (bell))
                #t))]
          [define remove-parens-forward
            (λ (start-pos)
              (let* ([pos (skip-whitespace start-pos 'forward #f)]
                     [first-char (get-character pos)]
                     [paren? (or (char=? first-char #\( )
                                 (char=? first-char #\[ ))]
                     [closer (if paren? 
                                 (forward-match pos (last-position)))])
                (if (and paren? closer)
                    (begin (begin-edit-sequence)
                           (delete pos (add1 pos))
                           (delete (-  closer 2) (- closer 1))
                           (end-edit-sequence))
                    (bell))
                #t))]
          
          (define/private (select-text f forward?)
            (let* ([start-pos (get-start-position)]
                   [end-pos (get-end-position)])
              (let-values ([(new-start new-end)
                            (if forward?
                                (values start-pos (f end-pos))
                                (values (f start-pos) end-pos))])
                (if (and new-start new-end) 
                    (set-position new-start new-end)
                    (bell))
                #t)))
          (public select-forward-sexp select-backward-sexp select-up-sexp select-down-sexp)
          [define select-forward-sexp (λ () (select-text (λ (x) (get-forward-sexp x)) #t))]
          [define select-backward-sexp (λ () (select-text (λ (x) (get-backward-sexp x)) #f))]
          [define select-up-sexp (λ () (select-text (λ (x) (find-up-sexp x)) #f))]
          [define select-down-sexp (λ () (select-text (λ (x) (find-down-sexp x)) #t))]
          
          (define/public (introduce-let-ans pos)
            (dynamic-wind
             (λ () (begin-edit-sequence))
             (λ ()
               (let ([before-text "(let ([ans "]
                     [after-text "])\n"]
                     [after-text2 "(printf \"~s\\n\" ans)\nans)"]
                     [end-l (get-forward-sexp pos)])
                 (cond
                   [end-l
                    (insert after-text2 end-l end-l)
                    (insert after-text end-l end-l)
                    (insert before-text pos pos)
                    (let ([blank-line-pos (+ end-l (string-length after-text) (string-length before-text))])
                      (set-position blank-line-pos blank-line-pos))
                    (tabify-selection 
                     pos
                     (+ end-l 
                        (string-length before-text)
                        (string-length after-text)
                        (string-length after-text2)))]
                   [else
                    (bell)])))
             (λ ()
               (end-edit-sequence))))
          
          (define/public (move-sexp-out begin-inner)
            (begin-edit-sequence)
            (let ([end-inner (get-forward-sexp begin-inner)]
                  [begin-outer (find-up-sexp begin-inner)])
              (cond
                [(and end-inner begin-outer)
                 (let ([end-outer (get-forward-sexp begin-outer)])
                   (cond
                     [end-outer
                      (delete end-inner end-outer)
                      (delete begin-outer begin-inner)
                      (tabify-selection begin-outer (+ begin-outer (- end-inner begin-inner)))]
                     [else (bell)]))]
                [else (bell)]))
            (end-edit-sequence))
          
	  (inherit get-fixed-style)
          (define/public (mark-matching-parenthesis pos)
            (let ([open-parens (map car (scheme-paren:get-paren-pairs))]
                  [close-parens (map cdr (scheme-paren:get-paren-pairs))])
              (when (member (string (get-character pos)) open-parens)
                (let ([end (get-forward-sexp pos)])
                  (when (and end
                             (member (string (get-character (- end 1))) close-parens))
                    (let ([start-style (send (find-snip pos 'after) get-style)]
                          [end-style (send (find-snip end 'before) get-style)])
                      (cond
                        [(and (eq? matching-parenthesis-style start-style)
                              (eq? matching-parenthesis-style end-style))
                         (let ([fixed-style (get-fixed-style)])
                           (change-style fixed-style pos (+ pos 1))
                           (change-style fixed-style (- end 1) end))]
                        [else
                         (change-style matching-parenthesis-style pos (+ pos 1))
                         (change-style matching-parenthesis-style (- end 1) end)])))))))
          
          (define/public (transpose-sexp pos)
            (let ([start-1 (get-backward-sexp pos)])
              (if (not start-1)
                  (bell)
                  (let ([end-1 (get-forward-sexp start-1)])
                    (if (not end-1)
                        (bell)
                        (let ([end-2 (get-forward-sexp end-1)])
                          (if (not end-2)
                              (bell)
                              (let ([start-2 (get-backward-sexp end-2)])
                                (if (or (not start-2)
                                        (< start-2 end-1))
                                    (bell)
                                    (let ([text-1 
                                           (get-text start-1 end-1)]
                                          [text-2 
                                           (get-text start-2 end-2)])
                                      (begin-edit-sequence)
                                      (insert text-1 start-2 end-2)
                                      (insert text-2 start-1 end-1)
                                      (set-position end-2)
                                      (end-edit-sequence)))))))))))
          [define tab-size 8]
          (public get-tab-size set-tab-size)
          [define get-tab-size (λ () tab-size)]
          [define set-tab-size (λ (s) (set! tab-size s))]
                    
          (inherit is-frozen? is-stopped?)
          (define/public (rewrite-square-paren)
            (cond
              [(or (not (preferences:get 'framework:fixup-parens))
                   (is-frozen?)
                   (is-stopped?))
               (insert #\[
                       (get-start-position)
                       (get-end-position))]
              [else 
               (insert-paren this)]))
          
          (super-new)))

      (define -text-mode<%>
        (interface ()
          ))
      
      (define text-mode-mixin
        (mixin (color:text-mode<%> mode:surrogate-text<%>) (-text-mode<%>)

          (define/override (on-disable-surrogate text)
            (keymap:remove-chained-keymap text keymap)
            (super on-disable-surrogate text))
          
          (define/override (on-enable-surrogate text)
	    (send text begin-edit-sequence)
	    (super on-enable-surrogate text)
	    (send (send text get-keymap) chain-to-keymap keymap #t)
            
            ;; I don't know about these editor flag settings.
            ;; maybe they belong in drscheme?
            (send text set-load-overwrites-styles #f)
            (send text set-wordbreak-map wordbreak-map)
	    (let ([bw (box 0)]
		  [bu (box #f)]
		  [tab-size (send text get-tab-size)])
	      (unless (and (null? (send text get-tabs #f bw bu))
			   (= tab-size (unbox bw))
			   (not (unbox bu)))
                	(send text set-tabs null (send text get-tab-size) #f)))
            (send text set-styles-fixed #t)
	    (send text end-edit-sequence))
          
          (super-new (get-token scheme-lexer-wrapper)
                     (token-sym->style short-sym->style-name)
                     (matches '((|(| |)|)
                                (|[| |]|)
                                (|{| |}|))))))
      
      (define (scheme-lexer-wrapper in)
        (let-values (((lexeme type paren start end) (scheme-lexer in)))
          (cond
            ((and (eq? type 'symbol)
                  (get-keyword-type lexeme))
             (values lexeme 'keyword paren start end))
            (else
             (values lexeme type paren start end)))))
      
      ;; get-keyword-type : string -> (union #f 'lambda 'define 'begin)
      (define (get-keyword-type text)
        (let* ([pref (preferences:get 'framework:tabify)]
               [ht (car pref)]
               [beg-reg (cadr pref)]
               [def-reg (caddr pref)]
               [lam-reg (cadddr pref)])
          (hash-table-get 
           ht
           (string->symbol text)
           (λ () 
             (cond
               [(and beg-reg (regexp-match beg-reg text)) 'begin]
               [(and def-reg (regexp-match def-reg text)) 'define]
               [(and lam-reg (regexp-match lam-reg text)) 'lambda]
               [else #f])))))
      
      (define set-mode-mixin
        (mixin (-text<%> mode:host-text<%>) ()
	  (super-new)
	  (inherit set-surrogate)
	  (set-surrogate (new text-mode%))))

      (define -text% (set-mode-mixin
		      (text-mixin
		       (mode:host-text-mixin
			color:text%))))

      (define text-mode% (text-mode-mixin color:text-mode%))
    
                                                                                           
              ;;                                 ;;                                        
               ;                                  ;                                        
               ;                                  ;                                        
  ;;;    ;;;   ; ;;    ;;;  ;;; ;    ;;;          ;  ;;   ;;;  ;;; ;;;;;; ;   ;;;;  ; ;;;  
 ;   ;  ;   ;  ;;  ;  ;   ;  ; ; ;  ;   ;         ; ;    ;   ;  ;   ;  ; ; ;      ;  ;   ; 
  ;;;   ;      ;   ;  ;;;;;  ; ; ;  ;;;;;         ;;     ;;;;;  ;   ;  ; ; ;   ;;;;  ;   ; 
     ;  ;      ;   ;  ;      ; ; ;  ;             ; ;    ;       ; ;   ; ; ;  ;   ;  ;   ; 
 ;   ;  ;   ;  ;   ;  ;   ;  ; ; ;  ;   ;         ;  ;   ;   ;   ;;;   ; ; ;  ;   ;  ;   ; 
  ;;;    ;;;  ;;; ;;;  ;;;  ;; ; ;;  ;;;         ;;   ;;  ;;;     ;   ;; ; ;;  ;;; ; ;;;;  
                                                                  ;                  ;     
                                                                  ;                  ;     
                                                                ;;                  ;;;    
      (define (setup-keymap keymap)
        (let ([add-pos-function
               (λ (name call-method)
                 (send keymap add-function name
                       (λ (edit event)
                         (call-method
                          edit
                          (send edit get-start-position)))))])
          (add-pos-function "remove-sexp" (λ (e p) (send e remove-sexp p)))
          (add-pos-function "forward-sexp" (λ (e p) (send e forward-sexp p)))
          (add-pos-function "backward-sexp" (λ (e p) (send e backward-sexp p)))
          (add-pos-function "up-sexp" (λ (e p) (send e up-sexp p)))
          (add-pos-function "down-sexp" (λ (e p) (send e down-sexp p)))
          (add-pos-function "flash-backward-sexp" (λ (e p) (send e flash-backward-sexp p)))
          (add-pos-function "flash-forward-sexp" (λ (e p) (send e flash-forward-sexp p)))
          (add-pos-function "remove-parens-forward" (λ (e p) (send e remove-parens-forward p)))
          (add-pos-function "transpose-sexp" (λ (e p) (send e transpose-sexp p)))
          (add-pos-function "mark-matching-parenthesis"
                            (λ (e p) (send e mark-matching-parenthesis p)))
          (add-pos-function "introduce-let-ans"
                            (λ (e p) (send e introduce-let-ans p)))
          (add-pos-function "move-sexp-out"
                            (λ (e p) (send e move-sexp-out p)))
          
          (let ([add-edit-function
                 (λ (name call-method)
                   (send keymap add-function name
                         (λ (edit event)
                           (call-method edit))))])
            (add-edit-function "select-forward-sexp" 
                               (λ (x) (send x select-forward-sexp)))
            (add-edit-function "select-backward-sexp"  
                               (λ (x) (send x select-backward-sexp)))
            (add-edit-function "select-down-sexp"  
                               (λ (x) (send x select-down-sexp)))
            (add-edit-function "select-up-sexp"  
                               (λ (x) (send x select-up-sexp)))
            (add-edit-function "tabify-at-caret"  
                               (λ (x) (send x tabify-selection)))
            (add-edit-function "do-return"  
                               (λ (x) 
                                 (send x insert-return)))
            (add-edit-function "comment-out"  
                               (λ (x) (send x comment-out-selection)))
            (add-edit-function "box-comment-out"  
                               (λ (x) (send x box-comment-out-selection)))
            (add-edit-function "uncomment"  
                               (λ (x) (send x uncomment-selection)))
            (add-edit-function "rewrite-square-paren"  
                               (λ (x) (send x rewrite-square-paren)))
            
            (let ([add/map-non-clever
                   (λ (name keystroke char)
                     (add-edit-function 
                      name
                      (λ (e) (send e insert char (send e get-start-position) (send e get-end-position))))
                     (send keymap map-function keystroke name))])
              (add/map-non-clever "non-clever-open-square-bracket" "c:[" #\[)
              (add/map-non-clever "non-clever-close-square-bracket" "c:]" #\])
              (add/map-non-clever "non-clever-close-curley-bracket" "c:}" #\})
              (add/map-non-clever "non-clever-close-round-paren" "c:)" #\))))
          
          (send keymap add-function "balance-parens"
                (λ (edit event)
                  (send edit balance-parens event)))
          
          (send keymap map-function "TAB" "tabify-at-caret")
          
          (send keymap map-function "return" "do-return")
          (send keymap map-function "s:return" "do-return")
          (send keymap map-function "s:c:return" "do-return")
          (send keymap map-function "a:return" "do-return")
          (send keymap map-function "s:a:return" "do-return")
          (send keymap map-function "c:a:return" "do-return")
          (send keymap map-function "c:s:a:return" "do-return")
          (send keymap map-function "c:return" "do-return")
          (send keymap map-function "d:return" "do-return")
          
          (send keymap map-function ")" "balance-parens")
          (send keymap map-function "]" "balance-parens")
          (send keymap map-function "}" "balance-parens")
          
          (send keymap map-function "[" "rewrite-square-paren")
          
          (let ([map-meta
                 (λ (key func)
                   (keymap:send-map-function-meta keymap key func))]
                [map
                 (λ (key func)
                   (send keymap map-function key func))])
            
            (map-meta "up" "up-sexp")
            (map-meta "c:u" "up-sexp")
            (map "a:up" "up-sexp")
            (map-meta "s:up" "select-up-sexp")
            (map "a:s:up" "select-up-sexp")
            (map-meta "s:c:u" "select-up-sexp")
            
            (map-meta "down" "down-sexp")
            (map "a:down" "down-sexp")
            (map-meta "s:down" "select-down-sexp")
            (map "a:s:down" "select-down-sexp")
            (map-meta "s:c:down" "select-down-sexp")
            
            (map-meta "right" "forward-sexp")
            (map "a:right" "forward-sexp")
            (map-meta "s:right" "select-forward-sexp")
            (map "a:s:right" "select-forward-sexp")
            
            (map-meta "left" "backward-sexp")
            (map "a:left" "backward-sexp")
            (map-meta "s:left" "select-backward-sexp")
            (map "a:s:left" "select-backward-sexp")
            
            (map-meta "return" "do-return")
            (map-meta "s:return" "do-return")
            (map-meta "s:c:return" "do-return")
            (map-meta "a:return" "do-return")
            (map-meta "s:a:return" "do-return")
            (map-meta "c:a:return" "do-return")
            (map-meta "c:s:a:return" "do-return")
            (map-meta "c:return" "do-return")
            
            (map-meta "c:semicolon" "comment-out")
            (map-meta "c:=" "uncomment")
            (map-meta "c:k" "remove-sexp")
            
            (map-meta "c:f" "forward-sexp")
            (map-meta "s:c:f" "select-forward-sexp")
            
            (map-meta "c:b" "backward-sexp")
            (map-meta "s:c:b" "select-backward-sexp")
            
            (map-meta "c:p" "flash-backward-sexp")
            (map-meta "s:c:n" "flash-forward-sexp")
            
            (map-meta "c:space" "select-forward-sexp")
            (map-meta "c:t" "transpose-sexp")
            
            ;(map-meta "c:m" "mark-matching-parenthesis")
            ; this keybinding doesn't interact with the paren colorer
            )
          (send keymap map-function "c:c;c:b" "remove-parens-forward")
          (send keymap map-function "c:c;c:l" "introduce-let-ans")
          (send keymap map-function "c:c;c:o" "move-sexp-out")))
      
      (define keymap (make-object keymap:aug-keymap%))
      (setup-keymap keymap)
      (define (get-keymap) keymap)
      
      ;; choose-paren : scheme-text number -> character
      ;; returns the character to replace a #\[ with, based
      ;; on the context where it is typed in.
      (define (insert-paren text)
        (let* ([pos (send text get-start-position)]
               [change-to
                (λ (c)
                  (send text insert c pos (+ pos 1)))])
          (send text begin-edit-sequence)
          (send text insert #\[ pos (send text get-end-position))
          (when (eq? (send text classify-position pos) 'parenthesis)
            (let* ([before-whitespace-pos (send text skip-whitespace pos 'backward #t)]
                   [backward-match (send text backward-match before-whitespace-pos 0)])
              (let ([b-m-char (and (number? backward-match) (send text get-character backward-match))])
                (cond
                  [backward-match
                   ;; there is an expression before this, at this layer
                   (let* ([before-whitespace-pos2 (send text skip-whitespace backward-match 'backward #t)]
                          [backward-match2 (send text backward-match before-whitespace-pos2 0)])
                     
                     (cond
                       ;; we found a new expression, two steps back, so we don't use the sibling
                       ;; check here -- we just go with square brackets.
                       [(and backward-match2
                             (text-between-equal? "new"
                                                  text
                                                  backward-match2
                                                  before-whitespace-pos2))
                        (void)]
                       [(member b-m-char '(#\( #\[ #\{))
                        ;; found a "sibling" parenthesized sequence. use the parens it uses.
                        (change-to b-m-char)]
                       [else
                        ;; there is a sexp before this, but it isn't parenthesized.
                        ;; if it is the `cond' keyword, we get a square bracket. otherwise not.
                        (unless (and (beginning-of-sequence? text backward-match)
                                     (ormap
                                      (λ (x)
                                        (text-between-equal? x text backward-match before-whitespace-pos))
                                      '("cond" "provide/contract")))
                          (change-to #\())]))]
                  [(not (zero? before-whitespace-pos))
                   ;; this is the first thing in the sequence
                   ;; pop out one layer and look for a keyword.
                   (let ([b-w-p-char (send text get-character (- before-whitespace-pos 1))])
                     (cond
                       [(equal? b-w-p-char #\()
                        (let* ([second-before-whitespace-pos (send text skip-whitespace 
                                                                   (- before-whitespace-pos 1)
                                                                   'backward
                                                                   #t)]
                               [second-backwards-match (send text backward-match
                                                             second-before-whitespace-pos
                                                             0)])
                          (cond
                            [(not second-backwards-match)
                             (change-to #\()]
                            [(and (beginning-of-sequence? text second-backwards-match)
                                  (ormap (λ (x) (text-between-equal? x
                                                                     text
                                                                     second-backwards-match
                                                                     second-before-whitespace-pos))
                                         '("let" 
                                           "let*" "let-values" "let-syntax" "let-struct" "let-syntaxes"
                                           "letrec"
                                           "letrec-syntaxes" "letrec-syntaxes+values" "letrec-values")))
                             ;; we found a let<mumble> keyword, so we get a square bracket
                             (void)]
                            [else
                             ;; go back one more sexp in the same row, looking for `let loop' pattern
                             (let* ([second-before-whitespace-pos2 (send text skip-whitespace 
                                                                         second-backwards-match
                                                                         'backward
                                                                         #t)]
                                    [second-backwards-match2 (send text backward-match
                                                                   second-before-whitespace-pos2
                                                                   0)])
                               (cond
                                 [(and second-backwards-match2
                                       (text-between-equal? "let" 
                                                            text
                                                            second-backwards-match2
                                                            second-before-whitespace-pos2))
                                  ;; found the `(let loop (' so we keep the [
                                  (void)]
                                 [else
                                  ;; otherwise, round.
                                  (change-to #\()]))]))]
                       [else 
                        (change-to #\()]))]
                  [else 
                   (change-to #\()]))))
          (send text end-edit-sequence)))
      
      ;; beginning-of-sequence? : text number -> boolean 
      ;; determines if this position is at the beginning of a sequence
      ;; that begins with a parenthesis.
      (define (beginning-of-sequence? text start)
        (let ([before-space (send text skip-whitespace start 'backward #t)])
          (cond
            [(zero? before-space) #t]
            [else
             (equal? (send text get-character (- before-space 1)) 
                     #\()])))
      
      (define (text-between-equal? str text start end)
        (and (= (string-length str) (- end start))
             (let loop ([i (string-length str)])
               (cond
                 [(= i 0) #t]
                 [else
                  (and (char=? (string-ref str (- i 1))
                               (send text get-character (+ i start -1)))
                       (loop (- i 1)))]))))
      
                                                                             
                        ;;;                                            ;;;   
                       ;                                                 ;   
                       ;                                                 ;   
; ;;;   ; ;;;   ;;;   ;;;;;   ;;;         ; ;;;   ;;;;  ; ;;;    ;;;     ;   
 ;   ;   ;     ;   ;   ;     ;   ;         ;   ;      ;  ;;  ;  ;   ;    ;   
 ;   ;   ;     ;;;;;   ;      ;;;          ;   ;   ;;;;  ;   ;  ;;;;;    ;   
 ;   ;   ;     ;       ;         ;         ;   ;  ;   ;  ;   ;  ;        ;   
 ;   ;   ;     ;   ;   ;     ;   ;         ;   ;  ;   ;  ;   ;  ;   ;    ;   
 ;;;;   ;;;;    ;;;   ;;;;    ;;;          ;;;;    ;;; ;;;;  ;;  ;;;   ;;;;;;
 ;                                         ;                                 
 ;                                         ;                                 
;;;                                       ;;;                                

      
      (define (add-preferences-panel)
        (preferences:add-panel
         (list (string-constant editor-prefs-panel-label) 
               (string-constant indenting-prefs-panel-label))
         (λ (p)
           (define get-keywords
             (λ (hash-table)
               (letrec ([all-keywords (hash-table-map hash-table list)]
                        [pick-out (λ (wanted in out)
                                    (cond
                                      [(null? in) (sort out string<=?)]
                                      [else (if (eq? wanted (cadr (car in))) 
                                                (pick-out wanted (cdr in) (cons (symbol->string (car (car in))) out))
                                                (pick-out wanted (cdr in) out))]))])
                 (values  (pick-out 'begin all-keywords null)
                          (pick-out 'define all-keywords null)
                          (pick-out 'lambda all-keywords null)))))
           (define-values (begin-keywords define-keywords lambda-keywords)
             (get-keywords (car (preferences:get 'framework:tabify))))
           (define add-button-callback
             (λ (keyword-type keyword-symbol list-box)
               (λ (button command)
                 (let ([new-one
                        (keymap:call/text-keymap-initializer
                         (λ ()
                           (get-text-from-user
                            (format (string-constant enter-new-keyword) keyword-type)
                            (format (string-constant x-keyword) keyword-type))))])
                   (when new-one
                     (let ([parsed (with-handlers ((exn:fail:read? (λ (x) #f)))
                                     (read (open-input-string new-one)))])
                       (cond
                         [(and (symbol? parsed)
                               (hash-table-get (car (preferences:get 'framework:tabify))
                                               parsed
                                               (λ () #f)))
                          (message-box (string-constant error)
                                       (format (string-constant already-used-keyword) parsed))]
                         [(symbol? parsed)
                          (let ([ht (car (preferences:get 'framework:tabify))])
                            (hash-table-put! ht parsed keyword-symbol)
                            (update-list-boxes ht))]
                         [else (message-box 
                                (string-constant error)
                                (format (string-constant expected-a-symbol) new-one))])))))))
           (define delete-callback
             (λ (list-box)
               (λ (button command)
                 (let* ([selections (send list-box get-selections)]
                        [symbols (map (λ (x) (string->symbol (send list-box get-string x))) selections)])
                   (for-each (λ (x) (send list-box delete x)) (reverse selections))
                   (let ([ht (car (preferences:get 'framework:tabify))])
                     (for-each (λ (x) (hash-table-remove! ht x)) symbols))))))
           (define main-panel (make-object horizontal-panel% p))
           (define make-column
             (λ (string symbol keywords bang-regexp)
               (let* ([vert (make-object vertical-panel% main-panel)]
                      [_ (make-object message% (format (string-constant x-like-keywords) string) vert)]
                      [box (make-object list-box% #f keywords vert void '(multiple))]
                      [button-panel (make-object horizontal-panel% vert)]
                      [text (new text-field% 
                                 (label (string-constant indenting-prefs-extra-regexp))
                                 (callback (λ (tf evt) 
                                             (let ([str (send tf get-value)])
                                               (cond
                                                 [(equal? str "") 
                                                  (bang-regexp #f)]
                                                 [else
                                                  (with-handlers ([exn:fail?
                                                                   (λ (x)
                                                                     (color-yellow (send tf get-editor)))])
                                                    (bang-regexp (regexp str))
                                                    (clear-color (send tf get-editor)))]))))
                                 (parent vert))]
                      [add-button (make-object button% (string-constant add-keyword)
                                    button-panel (add-button-callback string symbol box))]
                      [delete-button (make-object button% (string-constant remove-keyword)
                                       button-panel (delete-callback box))])
                 (send* button-panel 
                   (set-alignment 'center 'center)
                   (stretchable-height #f))
                 (send add-button min-width (send delete-button get-width))
                 (values box text))))
           (define (color-yellow text)
             (let ([sd (make-object style-delta%)])
               (send sd set-delta-background "yellow")
               (send text change-style sd 0 (send text last-position))))
           (define (clear-color text)
             (let ([sd (make-object style-delta%)])
               (send sd set-delta-background "white")
               (send text change-style sd 0 (send text last-position))))
           (define-values (begin-list-box begin-regexp-text) 
             (make-column "Begin"
                          'begin
                          begin-keywords
                          (λ (x) (set-car! (cdr (preferences:get 'framework:tabify)) x))))
           (define-values (define-list-box define-regexp-text) 
             (make-column "Define" 
                          'define 
                          define-keywords
                          (λ (x) (set-car! (cddr (preferences:get 'framework:tabify)) x))))
           (define-values (lambda-list-box lambda-regexp-text)
             (make-column "Lambda"
                          'lambda
                          lambda-keywords
                          (λ (x) (set-car! (cdddr (preferences:get 'framework:tabify)) x))))
           (define update-list-boxes
             (λ (hash-table)
               (let-values ([(begin-keywords define-keywords lambda-keywords) (get-keywords hash-table)]
                            [(reset) (λ (list-box keywords)
                                       (send list-box clear)
                                       (for-each (λ (x) (send list-box append x)) keywords))])
                 (reset begin-list-box begin-keywords)
                 (reset define-list-box define-keywords)
                 (reset lambda-list-box lambda-keywords)
                 #t)))
           (define update-gui
             (λ (pref)
               (update-list-boxes (car pref))
               (send begin-regexp-text set-value (or (object-name (cadr pref)) ""))
               (send define-regexp-text set-value (or (object-name (caddr pref)) ""))
               (send lambda-regexp-text set-value (or (object-name (cadddr pref)) ""))))
           (preferences:add-callback 'framework:tabify (λ (p v) (update-gui v)))
           main-panel))))))
