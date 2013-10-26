#lang racket/unit

;; originally by Dan Grossman
;; 6/30/95

(require string-constants
         racket/class
         racket/string
         mred/mred-sig
         syntax-color/module-lexer
         "collapsed-snipclass-helpers.rkt"
         "sig.rkt"
         "../gui-utils.rkt"
         "../preferences.rkt"
         racket/match
         unstable/options)

(import mred^
        [prefix preferences: framework:preferences^]
        [prefix icon: framework:icon^]
        [prefix keymap: framework:keymap^]
        [prefix text: framework:text^]
        [prefix editor: framework:editor^]
        [prefix frame: framework:frame^]
        [prefix comment-box: framework:comment-box^]
        [prefix mode: framework:mode^]
        [prefix color: framework:color^]
        [prefix color-prefs: framework:color-prefs^]
        [prefix finder: framework:finder^])

(export (rename framework:racket^
                [-text-mode<%> text-mode<%>]
                [-text<%> text<%>]
                [-text% text%]))

(init-depend mred^ framework:keymap^ framework:color^ framework:mode^
             framework:text^ framework:editor^)

(define-local-member-name 
  stick-to-next-sexp?
  get-private-racket-container-keymap)

(define (racket-paren:get-paren-pairs)
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")))

(define text-balanced? 
  (lambda (text [start 0] [in-end #f])
    (let* ([end (or in-end (send text last-position))]
           [port (open-input-text-editor text start end)])
      (with-handlers ([exn:fail:read:eof? (λ (x) #f)]
                      [exn:fail:read? (λ (x) #t)])
        (let ([first (read port)])
          (cond
            [(eof-object? first) #f]
            [else
             (let loop ()
               (let ([s (read port)])
                 (cond
                   [(eof-object? s) #t]
                   [else (loop)])))]))))))

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
        (datum->syntax
         #f
         (read (open-input-text-editor text))
         (list file line col pos 1))))
    
    (define/override get-text
      (lambda (offset num [flattened? #f])
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
    (super-new)
    (inherit set-snipclass)
    (set-snipclass 2lib-snip-class)))

(define sexp-snipclass% (make-sexp-snipclass% sexp-snip%))

;; old snips (from old versions of drracket) use this snipclass
(define 2lib-snip-class (make-object sexp-snipclass%))
(send 2lib-snip-class set-classname (format "~s" '((lib "collapsed-snipclass.ss" "framework")
                                                   (lib "collapsed-snipclass-wxme.ss" "framework"))))
(send 2lib-snip-class set-version 0)
(send (get-the-snip-class-list) add 2lib-snip-class)

;; old snips (from old versions of drracket) use this snipclass
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
               (when fwd
                 (make-collapse-item text up-sexp fwd menu)))))]))))

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


;                                            
;                                            
;                                            
;               ;  ;;;;                      
;              ;;  ;;;;                      
;    ;;;;    ;;;;; ;;;; ;;;    ;;;   ;;; ;;; 
;   ;;;;;;  ;;;;;; ;;;;;;;;;  ;;;;;  ;;;;;;; 
;  ;;;;;;;;  ;;;;  ;;;; ;;;; ;;;; ;; ;;;; ;; 
;  ;;;; ;;;  ;;;;  ;;;; ;;;; ;;;;;;; ;;;;    
;  ;;;;;;;;  ;;;;; ;;;; ;;;; ;;;;;   ;;;;    
;   ;;;;;;   ;;;;; ;;;; ;;;;  ;;;;;; ;;;;    
;    ;;;;     ;;;; ;;;; ;;;;   ;;;;  ;;;;    
;                                            
;                                            
;                                            


(define color-prefs-table
  (let ([constant-green (make-object color% 41 128 38)]
        [symbol-blue (make-object color% 38 38 128)])
    `((symbol ,symbol-blue ,(string-constant scheme-mode-color-symbol))
      (keyword ,symbol-blue ,(string-constant scheme-mode-color-keyword))
      (comment ,(make-object color% 194 116 31) ,(string-constant scheme-mode-color-comment))
      (string ,constant-green ,(string-constant scheme-mode-color-string))
      (text ,constant-green ,(string-constant scheme-mode-color-text))
      (constant ,constant-green ,(string-constant scheme-mode-color-constant))
      (hash-colon-keyword ,(make-object color% "brown")
                          ,(string-constant scheme-mode-color-hash-colon-keyword))
      (parenthesis ,(make-object color% "brown") ,(string-constant scheme-mode-color-parenthesis))
      (error ,(make-object color% "red") ,(string-constant scheme-mode-color-error))
      (other ,(make-object color% "black") ,(string-constant scheme-mode-color-other)))))

(define white-on-black-color-prefs-table
  (let* ([sym/kwd (make-object color% 102 102 255)]
         [new-entries
          `((symbol ,sym/kwd)
            (keyword ,sym/kwd)
            (comment ,(make-object color% 249 148 40))
            (string ,(make-object color% 51 174 51))
            (text ,(make-object color% 51 174 51))
            (constant ,(make-object color% 60 194 57))
            (hash-colon-keyword ,(make-object color% 151 69 43))
            (parenthesis ,(make-object color% 151 69 43))
            (other ,(make-object color% "white")))])
    (map 
     (λ (line)
       (let ([new (assoc (car line) new-entries)])
         (if new
             (list* (car line)
                    (cadr new)
                    (cddr line))
             line)))
     color-prefs-table)))

(define (get-color-prefs-table) color-prefs-table)
(define (get-white-on-black-color-prefs-table) white-on-black-color-prefs-table)

(define (short-sym->pref-name sym) (string->symbol (short-sym->style-name sym)))
(define (xlate-sym-style sym) (case sym
                                [(sexp-comment) 'comment]
                                [else sym]))
(define sn-hash (make-hasheq))
(define (short-sym->style-name _sym)
  (define sym (if (eq? _sym 'white-space)
                  'parenthesis
                  _sym))
  (hash-ref sn-hash sym
            (λ ()
              (let ([s (format "framework:syntax-color:scheme:~a"
                               (xlate-sym-style sym))])
                (hash-set! sn-hash sym s)
                s))))

(define (add-coloring-preferences-panel)
  (color-prefs:add-to-preferences-panel
   "Racket"
   (λ (parent)
     (for-each
      (λ (line)
        (let ([sym (car line)])
          (color-prefs:build-color-selection-panel 
           parent
           (short-sym->pref-name sym)
           (short-sym->style-name sym)
           (caddr line))))
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
    move-sexp-out
    kill-enclosing-parens
    toggle-round-square-parens))

(define init-wordbreak-map
  (λ (map)
    (send map set-map  #\< '(line selection))  ; interfaces e.g.the canvas<%> interface
    (send map set-map  #\> '(line selection))  ; interfaces, casts e.g. string->path
    (send map set-map  #\% '(line selection))  ; intefraces, classes
    (send map set-map  #\? '(line selection))  ; predicates
    (send map set-map  #\' '(line selection))  ; literal symbols
    (send map set-map  #\! '(line selection))  ; assignments e.g. set
    (send map set-map  #\- '(line selection))  ; hyphens
    (send map set-map  #\: '(line selection)))); valid identifiers with colons

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
  (mixin (text:basic<%> mode:host-text<%> color:text<%> text:autocomplete<%> editor:keymap<%>)
         (-text<%>)
    (inherit begin-edit-sequence
             delete
             end-edit-sequence
             local-edit-sequence?
             find-string
             extend-position
             get-character
             get-extend-end-position
             get-extend-start-position
             get-keymap
             get-text
             get-start-position
             get-style-list
             get-end-position
             flash-on
             insert
             is-stopped?
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
    (inherit has-focus? find-snip split-snip
             position-location get-dc)
    
    (define private-racket-container-keymap (new keymap:aug-keymap%))
    (define/public (get-private-racket-container-keymap) private-racket-container-keymap) 
    
    (define/override (get-keymaps)
      (editor:add-after-user-keymap private-racket-container-keymap 
                                    (super get-keymaps)))
    
    (define/override (get-word-at current-pos)
      (let ([no-word ""])
        (cond
          [(is-stopped?)
           no-word]
          [else
           (let ([type (classify-position (max 0 (- current-pos 1)))])
             (cond
               [(memq type '(symbol keyword)) 
                (get-text (look-for-non-symbol/non-kwd (max 0 (- current-pos 1)))
                          current-pos)]
               [else no-word]))])))
    
    (define/private (look-for-non-symbol/non-kwd start)
      (let loop ([i start])
        (cond
          [(< i 0) 
           0]
          [(memq (classify-position i) '(symbol keyword))
           (loop (- i 1))]
          [else
           (+ i 1)])))
    
    (public tabify-all insert-return calc-last-para 
            box-comment-out-selection comment-out-selection uncomment-selection
            flash-forward-sexp 
            flash-backward-sexp backward-sexp find-up-sexp up-sexp find-down-sexp down-sexp
            remove-parens-forward)
    
    (define/public (get-limit pos) 0)
    
    (define/public (balance-parens key-event [smart-skip #f])
      (insert-close-paren (get-start-position) 
                          (send key-event get-key-code)
                          (preferences:get 'framework:paren-match)
                          (preferences:get 'framework:fixup-parens)
                          (or smart-skip
                              (and (preferences:get 'framework:automatic-parens)
                                   (not (in-string/comment? this))
                                   'adjacent))))
    
    (define/public (tabify-on-return?) #t)
    (define/public (tabify [pos (get-start-position)])
      (unless (is-stopped?)
        (let* ([tabify-prefs (preferences:get 'framework:tabify)]
               [last-pos (last-position)]
               [para (position-paragraph pos)]
               [is-tabbable? (and (> para 0)
                                  (not (memq (classify-position (- (paragraph-start-position para) 1))
                                             '(comment string error))))]
               [end (if is-tabbable? (paragraph-start-position para) 0)]
               [limit (get-limit pos)]
               ;; "contains" is the start of the initial sub-S-exp
               ;;  in the S-exp that contains "pos". If pos is outside
               ;;  all S-exps, this will be the start of the initial
               ;;  S-exp
               [contains 
                (if is-tabbable?
                    (backward-containing-sexp end limit)
                    #f)]
               [contain-para (and contains
                                  (position-paragraph contains))]
               ;; "last" is the start of the S-exp just before "pos"
               [last 
                (if contains
                    (let ([p (get-backward-sexp end)])
                      (if (and p (p . >= . limit))
                          p
                          (backward-match end limit)))
                    #f)]
               [last-para (and last
                               (position-paragraph last))])
          (define sizing-dc (or (get-dc) (make-object bitmap-dc% (make-bitmap 1 1))))
          (letrec
              ([find-offset
                (λ (start-pos)
                  (define tab-char? #f)
                  (define end-pos
                    (let loop ([p start-pos])
                      (let ([c (get-character p)])
                        (cond
                          [(char=? c #\tab)
                           (set! tab-char? #t)
                           (loop (add1 p))]
                          [(char=? c #\newline)
                           p]
                          [(char-whitespace? c)
                           (loop (add1 p))]
                          [else
                           p]))))
                  (define start-x (box 0))
                  (define end-x (box 0))
                  (position-location start-pos start-x #f #t #t)
                  (position-location end-pos end-x #f #t #t)
                  (define-values (w _1 _2 _3)
                    (send sizing-dc get-text-extent "x" 
                          (send (send (get-style-list)
                                      find-named-style "Standard")
                                get-font)))
                  (values (inexact->exact (floor (/ (- (unbox end-x) (unbox start-x)) w)))
                          end-pos
                          tab-char?))]
               
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
                  (define pos-start end)
                  (define-values (gwidth curr-offset tab-char?) (find-offset pos-start))
                  (unless (and (not tab-char?) (= amt (- curr-offset pos-start)))
                    (delete pos-start curr-offset)
                    (insert (make-string amt #\space) pos-start)))]
               [get-proc
                (λ ()
                  (let ([id-end (get-forward-sexp contains)])
                    (and (and id-end (> id-end contains))
                         (let* ([text (get-text contains id-end)])
                           (or (get-keyword-type text tabify-prefs)
                               'other)))))]
               [procedure-indent 
                (λ ()
                  (case (get-proc)
                    [(begin define) 1]
                    [(lambda) 3]
                    [else 0]))]
               [special-check
                (λ ()
                  (let* ([proc-name (get-proc)])
                    (or (eq? proc-name 'define)
                        (eq? proc-name 'lambda))))]
               [curley-brace-sexp?
                (λ ()
                  (define up-p (find-up-sexp pos))
                  (and up-p
                       (equal? #\{ (get-character up-p))))]
               
               [indent-first-arg (λ (start)
                                   (define-values (gwidth curr-offset tab-char?) (find-offset start))
                                   gwidth)])
            (when (and is-tabbable?
                       (not (char=? (get-character (sub1 end))
                                    #\newline)))
              (insert #\newline (paragraph-start-position para)))
            (cond
              [(not is-tabbable?) 
               (when (= para 0)
                 (do-indent 0))]
              [(let-values ([(gwidth real-start tab-char?) (find-offset end)]) 
                 (and (<= (+ 3 real-start) (last-position))
                      (string=? ";;;"
                                (get-text real-start
                                          (+ 2 real-start)))))
               (void)]
              [(not contains)
               ;; Something went wrong matching. Should we get here?
               (do-indent 0)]
              ;; disable this to accommodate PLAI programs; 
              ;; return to this when a #lang capability is set up.
              #; 
              [(curley-brace-sexp?)
               ;; when we are directly inside an sexp that uses {}s,
               ;; we indent in a more C-like fashion (to help Scribble)
               (define first-curley (find-up-sexp pos))
               (define containing-curleys
                 (let loop ([pos first-curley])
                   (let ([next (find-up-sexp pos)])
                     (if (and next
                              (equal? (get-character next) #\{))
                         (+ (loop next) 1)
                         1))))
               (define close-first-curley (get-forward-sexp first-curley))
               (define para (position-paragraph pos))
               (when (and close-first-curley
                          (<= (paragraph-start-position para) 
                              close-first-curley 
                              (paragraph-end-position para)))
                 (set! containing-curleys (max 0 (- containing-curleys 1))))
               (do-indent (* containing-curleys 2))]
              [(not last) 
               ;; We can't find a match backward from pos,
               ;;  but we seem to be inside an S-exp, so 
               ;;  go "up" an S-exp, and move forward past
               ;;  the associated paren
               (let ([enclosing (find-up-sexp pos)])
                 (if enclosing
                     (do-indent (+ (visual-offset enclosing) 1))
                     (do-indent 0)))]
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
               ;;  unless there are just two sexps and the second is an ellipsis.
               ;;  in that case, we just ignore the ellipsis
               (let ([name-length (let ([id-end (get-forward-sexp contains)])
                                    (if id-end
                                        (- id-end contains)
                                        0))])
                 (cond
                   [(second-sexp-is-ellipsis? contains)
                    (do-indent (visual-offset contains))]
                   [(not (find-up-sexp pos))
                    (do-indent (visual-offset contains))]
                   [else
                    (do-indent (+ (visual-offset contains)
                                  name-length
                                  (indent-first-arg (+ contains 
                                                       name-length))))]))]
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
    
    ;; returns #t if `contains' is at a position on a line with an sexp, an ellipsis and nothing else.
    ;; otherwise, returns #f
    (define/private (second-sexp-is-ellipsis? contains)
      (let ([fst-end (get-forward-sexp contains)])
        (and fst-end
             (let ([snd-end (get-forward-sexp fst-end)])
               (and snd-end
                    (let ([snd-start (get-backward-sexp snd-end)])
                      (and snd-start
                           (equal? (get-text snd-start snd-end)
                                   "...")
                           (let ([thrd-start (get-forward-sexp snd-end)])
                             (and (or (not thrd-start)
                                      (not (= (position-paragraph thrd-start)
                                              (position-paragraph snd-start)))))))))))))
    
    (define/public (tabify-selection [start-pos (get-start-position)]
                                     [end-pos (get-end-position)])
      (unless (is-stopped?) 
        (define first-para (position-paragraph start-pos))
        (define end-para (position-paragraph end-pos))
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
               (end-busy-cursor)))))))
    
    (define (tabify-all) (tabify-selection 0 (last-position)))
    (define (insert-return)
      (if (tabify-on-return?)
          (begin 
            (begin-edit-sequence #t #f)
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
      (lambda ([start-pos (get-start-position)] 
               [end-pos (get-end-position)]) 
        (begin-edit-sequence) 
        (let ([first-pos-is-first-para-pos? 
               (= (paragraph-start-position (position-paragraph start-pos)) 
                  start-pos)]) 
          (let* ([first-para (position-paragraph start-pos)] 
                 [last-para (calc-last-para end-pos)]) 
            (let para-loop ([curr-para first-para]) 
              (when (<= curr-para last-para) 
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
      (lambda ([_start-pos 'start]
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
      (lambda ([start-pos (get-start-position)]
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


    ;; stick-to-next-sexp?: natural -> boolean
    (define stick-to-patterns
      '("'" "," ",@" "`" "#'" "#," "#`" "#,@"
        "#&" "#;" "#hash" "#hasheq" "#ci" "#cs"))
    (define stick-to-patterns-union
      (regexp (string-append 
               "^("
               (string-join (map regexp-quote stick-to-patterns) "|")
               ")")))
    (define stick-to-patterns-union-anchored
      (regexp (string-append 
               "^("
               (string-join (map regexp-quote stick-to-patterns) "|")
               ")$")))
    (define stick-to-max-pattern-length 
      (apply max (map string-length stick-to-patterns)))
    (define/public (stick-to-next-sexp? start-pos)
      ;; Optimization: speculatively check whether the string will
      ;; match the patterns; at time of writing, forward-match can be
      ;; really expensive.
      (define snippet 
        (get-text start-pos 
                  (min (last-position) 
                       (+ start-pos stick-to-max-pattern-length))))
      (and (regexp-match stick-to-patterns-union snippet)
           (let ([end-pos (forward-match start-pos (last-position))])
             (and end-pos
                  (regexp-match stick-to-patterns-union-anchored
                                (get-text start-pos end-pos))
                  #t))))
      
    (define/public (get-forward-sexp start-pos) 
      ;; loop to work properly with quote, etc.
      (let loop ([one-forward (forward-match start-pos (last-position))])
        (cond
          [(and one-forward (not (= 0 one-forward)))
           (let ([bw (backward-match one-forward 0)])
             (cond
               [(and bw 
                     (stick-to-next-sexp? bw))
                (let ([two-forward (forward-match one-forward (last-position))])
                  (if two-forward
                      (loop two-forward)
                      one-forward))]
               [else
                one-forward]))]
          [else one-forward])))
    
    (define/public (remove-sexp start-pos)
      (let ([end-pos (get-forward-sexp start-pos)])
        (if end-pos 
            (kill 0 start-pos end-pos)
            (bell)))
      #t)
    (define/public (forward-sexp start-pos)
      (let ([end-pos (get-forward-sexp start-pos)])
        (if end-pos 
            (set-position end-pos)
            (bell))
        #t))
    [define flash-forward-sexp
      (λ (start-pos)
        (let ([end-pos (get-forward-sexp start-pos)])
          (if end-pos 
              (flash-on end-pos (add1 end-pos))
              (bell)) 
          #t))]
    (define/public (get-backward-sexp start-pos)
      (let* ([limit (get-limit start-pos)]
             [end-pos (backward-match start-pos limit)]
             [min-pos (backward-containing-sexp start-pos limit)])
        (if (and end-pos 
                 (or (not min-pos)
                     (end-pos . >= . min-pos)))
            ;; Can go backward, but check for preceding quote, unquote, etc.
            (let loop ([end-pos end-pos])
              (let ([next-end-pos (backward-match end-pos limit)])
                (if (and next-end-pos
                         (or (not min-pos)
                             (end-pos . >= . min-pos))
                         (stick-to-next-sexp? next-end-pos))
                    (loop next-end-pos)
                    end-pos)))
            ;; can't go backward at all:
            #f)))
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
                (let ([poss (let loop ([parens (racket-paren:get-paren-pairs)])
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
        (let loop ([pos start-pos])
          (let ([next-pos (get-forward-sexp pos)])
            (if (and next-pos (> next-pos pos))
                (let ([back-pos
                       (backward-containing-sexp (sub1 next-pos) pos)])
                  (if (and back-pos
                           (> back-pos pos))
                      back-pos
                      (loop next-pos)))
                #f))))]
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
               [paren? (or (char=? first-char #\()
                           (char=? first-char #\[)
                           (char=? first-char #\{))]
               [closer (and paren? 
                            (get-forward-sexp pos))])
          (if (and paren? closer)
              (begin (begin-edit-sequence #t #f)
                     (delete pos (add1 pos))
                     (delete (-  closer 2) (- closer 1))
                     (end-edit-sequence))
              (bell))
          #t))]
    
    (define/private (select-text f forward?)
      (define start-pos (get-start-position))
      (define end-pos (get-end-position))
      (define new-pos
        (if forward?
            (if (= (get-extend-start-position) start-pos)
                (f end-pos)
                (f start-pos))
            (if (= (get-extend-end-position) end-pos)
                (f start-pos)
                (f end-pos))))
      (if new-pos 
          (extend-position new-pos)
          (bell))
          #t)
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
      (begin-edit-sequence #t #f)
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
    
    (define/public (kill-enclosing-parens begin-inner)
      (begin-edit-sequence #t #f)
      (define begin-outer (find-up-sexp begin-inner))
      (cond
        [begin-outer
          (define end-outer (get-forward-sexp begin-outer))
          (cond
            [(and end-outer (>= (- end-outer begin-outer) 2))
             (delete (- end-outer 1) end-outer)
             (delete begin-outer (+ begin-outer 1))
             (tabify-selection begin-outer (- end-outer 2))]
            [else (bell)])]
        [else (bell)])
      (end-edit-sequence))
    
    ;; change the parens following the cursor from () to [] or vice versa
    (define/public (toggle-round-square-parens start-pos)
      (begin-edit-sequence #t #f)
      (let* ([sexp-begin (skip-whitespace start-pos 'forward #f)]
             [sexp-end (get-forward-sexp sexp-begin)])
        (cond [(and sexp-end
                    (< (+ 1 sexp-begin) sexp-end))
               ;; positions known to exist: start-pos <= x < sexp-end
               (match* ((get-character sexp-begin) (get-character (- sexp-end 1)))
                       [(#\( #\)) (replace-char-at-posn sexp-begin "[")
                                  (replace-char-at-posn (- sexp-end 1) "]")]
                       [(#\[ #\]) (replace-char-at-posn sexp-begin "(")
                                  (replace-char-at-posn (- sexp-end 1) ")")]
                       [(_ _) (bell)])]
              [else (bell)]))
      (end-edit-sequence))
    
    ;; replace-char-at-posn: natural-number string -> 
    ;;   replace the char at the given posn with the given string.
    ;;
    ;; this abstraction exists because the duplicated code in toggle-round-square-parens was 
    ;; just a little too much for comfort
    (define (replace-char-at-posn posn str)
      ;; insertions are performed before deletions in order to preserve the location of the cursor
      (insert str (+ posn 1) (+ posn 1))
      (delete posn (+ posn 1)))
    
    (inherit get-fixed-style)
    (define/public (mark-matching-parenthesis pos)
      (let ([open-parens (map car (racket-paren:get-paren-pairs))]
            [close-parens (map cdr (racket-paren:get-paren-pairs))])
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
    
    ;; get-snips/rev: start end -> (listof snip)
    ;; Returns a list of the snips in reverse order between
    ;; start and end.
    (define/private (get-snips/rev start end)
      (split-snip start)
      (split-snip end)
      (let loop ([snips/rev '()]
                 [a-snip (find-snip start 'after-or-none)])
        (cond
          [(or (not a-snip)
               (>= (get-snip-position a-snip)
                   end))
           snips/rev]
          [else
           (loop (cons (send a-snip copy) snips/rev)
                 (send a-snip next))])))
    
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
                              (let ([snips-1/rev (get-snips/rev start-1 end-1)]
                                    [snips-2/rev (get-snips/rev start-2 end-2)])
                                (begin-edit-sequence)
                                (delete start-2 end-2)
                                (for-each (λ (s) (insert s start-2)) snips-1/rev)
                                (delete start-1 end-1)
                                (for-each (λ (s) (insert s start-1)) snips-2/rev)
                                (set-position end-2)
                                (end-edit-sequence)))))))))))
    [define tab-size 8]
    (public get-tab-size set-tab-size)
    [define get-tab-size (λ () tab-size)]
    [define set-tab-size (λ (s) (set! tab-size s))]
    
    (define/override (get-start-of-line pos)
      (define para (position-paragraph pos))
      (define para-start (paragraph-start-position para))
      (define para-end (paragraph-end-position para))
      (define first-non-whitespace 
        (let loop ([i para-start])
          (cond
            [(= i para-end) #f]
            [(char-whitespace? (get-character i))
             (loop (+ i 1))]
            [else i])))
      (define new-pos 
        (cond 
          [(not first-non-whitespace) para-start]
          [(= pos para-start) first-non-whitespace]
          [(<= pos first-non-whitespace) para-start]
          [else first-non-whitespace]))
      new-pos)

    (super-new)))

(define -text-mode<%>
  (interface ()
    ))

(define module-lexer/waived (waive-option module-lexer))

(define text-mode-mixin
  (mixin (color:text-mode<%> mode:surrogate-text<%>) (-text-mode<%>)
    
    (define saved-wordbreak-map #f)
    
    (define/override (on-disable-surrogate text)
      (keymap:remove-chained-keymap text keymap)
      (send text set-wordbreak-map saved-wordbreak-map)
      (super on-disable-surrogate text))
    
    (define/override (on-enable-surrogate text)
      (send text begin-edit-sequence)
      (super on-enable-surrogate text)
      (send (send text get-private-racket-container-keymap) chain-to-keymap keymap #f)
      
      (set! saved-wordbreak-map (send text get-wordbreak-map))
      
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
    
    (define tabify-pref (preferences:get 'framework:tabify))
    (preferences:add-callback
     'framework:tabify
     (lambda (k v) (set! tabify-pref v)))
    (define/private (racket-lexer-wrapper in offset mode)
      (define-values (lexeme type paren start end backup-delta new-mode) 
        (module-lexer/waived in offset mode))
      (cond
        [(and (eq? type 'symbol)
              (get-keyword-type lexeme tabify-pref))
         (values lexeme 'keyword paren start end backup-delta new-mode)]
        [else
         (values lexeme type paren start end backup-delta new-mode)]))
    
    (define/override (put-file text sup directory default-name)
      (parameterize ([finder:default-extension "rkt"]
                     #; ; no need for the following, since it's the default
                     [finder:default-filters '(["Racket Sources" "*.rkt;*.scrbl;*.ss;*.scm"]
                                               ["Any" "*.*"])])
        ;; don't call the surrogate's super, since it sets the default extension
        (sup directory default-name)))
    
    (super-new (get-token (lambda (in offset mode) (racket-lexer-wrapper in offset mode)))
               (token-sym->style short-sym->style-name)
               (matches '((|(| |)|)
                          (|[| |]|)
                          (|{| |}|))))))

;; get-keyword-type : string (list ht regexp regexp regexp)
;;                 -> (union #f 'lambda 'define 'begin)
(define (get-keyword-type text pref)
  (let* ([ht (car pref)]
         [beg-reg (cadr pref)]
         [def-reg (caddr pref)]
         [lam-reg (cadddr pref)])
    (hash-ref
     ht
     (with-handlers ((exn:fail:read? (λ (x) #f)))
       (read (open-input-string text)))
     (λ () 
       (cond
         [(and beg-reg (regexp-match beg-reg text)) 'begin]
         [(and def-reg (regexp-match def-reg text)) 'define]
         [(and lam-reg (regexp-match lam-reg text)) 'lambda]
         [else #f])))))


;; in-position? : text (list symbol) -> boolean
;; determines if the cursor is currently sitting in a particular
;; position. To make detection of whether the cursor is in
;; a string or comment more robust, check also the position
;; right before the cursor to make sure it matches. This handles
;; the situation ... |"blah blah"  where | indicates cursor; in
;; this case, the cursor is _not_ in the string (although
;; classify-position characterizes it so).
(define (in-position? text sym-list)
  (define selection-start (send text get-start-position))
  (define first-type (send text classify-position selection-start))
  (define final-type
    (if (and (member first-type '(string comment))
             (or (= selection-start 0)
                 (not (eq? (send text classify-position (- selection-start 1))
                           first-type))))
        'white-space
        first-type))
  (and (member final-type sym-list) #t))

;; determines if the cursor is currently sitting in a string
;; literal or a comment. 
(define (in-string/comment? text)
  (in-position? text '(comment string)))

;; produces the 1 character string immediately following
;; the cursor, if there is one and if there is not a current
;; selection, in which case produces #f
(define (immediately-following-cursor text)
  (define selection-start (send text get-start-position))
  (and (= selection-start (send text get-end-position))   ; nothing selected
       (< selection-start (send text last-position))
       (send text get-text selection-start (+ selection-start 1))))


(define set-mode-mixin
  (mixin (-text<%> mode:host-text<%>) ()
    (super-new)
    (inherit set-surrogate)
    (set-surrogate (new text-mode%))))

(define -text% (set-mode-mixin
                (text-mixin
                 (text:autocomplete-mixin
                  (mode:host-text-mixin
                   color:text%)))))

(define text-mode% (text-mode-mixin color:text-mode%))

(define (setup-keymap keymap)
  (define (add-edit-function name f)
    (send keymap add-function name (λ (edit event) (f edit))))
  (define (add-pos-function name f)
    (send keymap add-function name
          (λ (edit event)
            (f edit (send edit get-start-position)))))
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
  (add-pos-function "kill-enclosing-parens"
                    (lambda (e p) (send e kill-enclosing-parens p)))
  (add-pos-function "toggle-round-square-parens"
                    (lambda (e p) (send e toggle-round-square-parens p)))
  
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
                     (λ (x) (send x insert-return)))
  (add-edit-function "comment-out"  
                     (λ (x) (send x comment-out-selection)))
  (add-edit-function "box-comment-out"  
                     (λ (x) (send x box-comment-out-selection)))
  (add-edit-function "uncomment"  
                     (λ (x) (send x uncomment-selection)))
  
  (send keymap add-function "paren-double-select"
        (λ (text event)
          (keymap:region-click
           text event 
           (λ (click-pos eol?)
             (define (word-based)
               (define start-box (box click-pos))
               (define end-box (box click-pos))
               (send text find-wordbreak start-box end-box 'selection)
               (values (unbox start-box) (unbox end-box)))
             (define token (send text classify-position click-pos))
             (define-values (start end)
               (cond
                 [(memq token '(string comment)) (word-based)]
                 [(and (equal? token 'other)
                       (let-values ([(start end) (send text get-token-range click-pos)])
                         (and start
                              end
                              (let ([str (send text get-text start end)])
                                (or (regexp-match? #rx"^#lang" str)
                                    (regexp-match? #rx"^#!" str))))))
                  (word-based)]
                 [(and (equal? token 'parenthesis)
                       (ormap (λ (pr) (equal? (cdr pr) (string (send text get-character click-pos))))
                              (racket-paren:get-paren-pairs)))
                  (define start (send text get-backward-sexp (+ click-pos 1)))
                  (if start 
                      (values start (+ click-pos 1))
                      (word-based))]
                 [else
                  (let ([end (send text get-forward-sexp click-pos)])
                    (if end 
                        (let ([beginning (send text get-backward-sexp end)])
                          (if beginning
                              (values beginning end)
                              (word-based)))
                        (word-based)))]))
             (send text set-position start end)))))

  (let ([add/map-non-clever
         (λ (name keystroke char)
           (add-edit-function 
            name
            (λ (e) (send e insert char (send e get-start-position) (send e get-end-position))))
           (send keymap map-function keystroke name))])
    (add/map-non-clever "non-clever-open-square-bracket" "c:[" #\[)
    (add/map-non-clever "non-clever-close-square-bracket" "c:]" #\])
    (add/map-non-clever "non-clever-close-curley-bracket" "c:}" #\})
    (add/map-non-clever "non-clever-close-round-paren" "c:)" #\)))
  
  (send keymap add-function "balance-parens"
        (λ (edit event)
          (send edit balance-parens event)))
  (send keymap add-function "balance-parens-forward"
        (λ (edit event)
          (send edit balance-parens event 'forward)))
  
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
  
  (send keymap map-function "leftbuttondouble" "paren-double-select")
  

  ;(define (insert-brace-pair text open-brace close-brace [space-between? #f])
  ;  (insert/check/balance text open-brace close-brace #f space-between?))
    #|
    (define selection-start (send text get-start-position))
    (define hash-before?  ; tweak to detect and correctly close block comments #| ... |#
      (and (< 0 selection-start)
           (string=? "#" (send text get-text (- selection-start 1) selection-start))))
    (send text begin-edit-sequence)
    (send text set-position (send text get-end-position))
    (when space-between? (send text insert " "))
    (send text insert close-brace)
    (when (and (char? open-brace) (char=? #\| open-brace) hash-before?) 
      (send text insert #\#))
    (send text set-position selection-start)
    (send text insert open-brace)
    (when space-between?
      (send text set-position (+ (send text get-start-position) 1)))
    (send text end-edit-sequence))|#

  ;; Inserts the open parens character and, if the resulting token
  ;; type satisfies checkp, then go ahead and insert the close parens
  ;; and set the cursor between them.
  ;; When space-between?, adds a space between the braces and places
  ;; the cursor after the space.
  ;; checkp: (or/c #f symbol (symbol -> boolean))
  ;;  When checkp is #f, always inserts both open and close braces
  ;;  When checkp is a symbol, only inserts the closing brace if 
  ;;    the tokenizer identifies open-brace as that type of token
  ;;    having inserted it
  ;;  When checkp is a predicate, only inserts the closing brace if
  ;;    the token type of the inserted open-brace satisfies it
  (define (insert-brace-pair text open-brace close-brace [checkp #f] [space-between? #f])
    (define selection-start (send text get-start-position))
    (define selection-end (send text get-end-position))
    (define open-len (if (string? open-brace) (string-length open-brace) 1))
    (send text begin-edit-sequence #t #f)
    (send text insert open-brace selection-start)
    (define tok-type (send text classify-position selection-start))
    (when (or (not checkp)
              (and (symbol? checkp) (eq? checkp tok-type))
              (and (procedure? checkp) (checkp tok-type)))
      (define hash-before?  ; tweak to detect and correctly close block comments #| ... |#
        (and (< 0 selection-start)
             (string=? "#" (send text get-text (- selection-start 1) selection-start))))
      (send text set-position (+ selection-end open-len))
      (when space-between? (send text insert " "))
      (send text insert close-brace)
      (when (and (char? open-brace) (char=? #\| open-brace) hash-before?) 
        (send text insert #\#))
      (send text set-position (+ selection-start open-len (if space-between? 1 0))))
    (send text end-edit-sequence))
  

  ;; only insert a pair if automatic-parens preference is on, depending
  ;; on other analyses of the state of the text (e.g. auto-parens shouldn't
  ;; affect typing literal characters inside a string constant, etc.)
  (define (maybe-insert-brace-pair text open-brace close-brace)
    (define open-parens 
      (for/list ([x (racket-paren:get-paren-pairs)]) (string-ref (car x) 0)))
    (cond
      [(not (preferences:get 'framework:automatic-parens))
       (define startpos (send text get-start-position))
       (if (and (send text get-overwrite-mode) 
                (= startpos (send text get-end-position)))
           (send text insert open-brace startpos (add1 startpos))
           (send text insert open-brace))]
    
      [else  ; automatic-parens is enabled
       (define c (immediately-following-cursor text))
       (define cur-token 
         (send text classify-position (send text get-start-position)))
       (cond
         ; insert paren pair if it results valid parenthesis token...
         [(member open-brace open-parens) 
          (insert-brace-pair text open-brace close-brace 'parenthesis)]
         
         ; ASSUME: from here on, open-brace is either "  or  |
         [else
          ;(printf "tok ~a~n" cur-token)
          (match cur-token
            [(or 'error #f) (insert-brace-pair text open-brace close-brace 'error)]
            ['constant (insert-brace-pair text open-brace close-brace 
                                       (λ(t) (not (eq? t 'constant))))]
            [(or 'symbol 'comment)
             (cond
               [(and c (char=? #\| open-brace) (string=? c "|"))   ;; smart skip
                   (send text set-position (+ 1 (send text get-end-position)))
                   (define d (immediately-following-cursor text))
                   (when (and d (string=? d "#"))   ; a block comment?
                     (send text set-position (+ 1 (send text get-end-position))))]
               [(eq? cur-token 'comment) (send text insert open-brace)]
               [else (insert-brace-pair text open-brace close-brace)])]
            ['string
             (cond
               [(not (char=? #\" open-brace)) (send text insert open-brace)]
               [else 
                (define start-position (send text get-start-position))
                (define end-position (send text get-end-position))
                (cond
                  ; smart skip a " if it is the immediately following character (c)
                  [(and c (string=? c "\"")) 
                   (send text set-position (+ 1 end-position))]
                  
                  ; there is no current selection - split the string in two
                  [(= start-position end-position)
                   (insert-brace-pair text #\" #\" #f #t)]
                  
                  ; there is a selection - split the selected text off as a
                  ; separate string from the surrounding in an intelligent way
                  ; and retain selection of the split-out string
                  [else (define selection-length (- end-position start-position))
                        (insert-brace-pair text "\" \"" "\" \"")
                        (define cur-position (send text get-start-position))
                        (send text set-position 
                              (- cur-position 1)
                              (+ cur-position selection-length 1))])])]
            [_  (insert-brace-pair text open-brace close-brace)]) ])]))
         

      
  
  (add-edit-function "insert-()-pair" (λ (text) (insert-brace-pair text #\( #\))))
  (add-edit-function "insert-[]-pair" (λ (text) (insert-brace-pair text #\[ #\])))
  (add-edit-function "insert-{}-pair" (λ (text) (insert-brace-pair text #\{ #\})))
  (add-edit-function "insert-\"\"-pair" (λ (text) (insert-brace-pair text #\" #\")))
  (add-edit-function "insert-||-pair" (λ (text) (insert-brace-pair text #\| #\|)))
  
  (add-edit-function "maybe-insert-()-pair" (λ (text) (maybe-insert-brace-pair text #\( #\))))
  (add-edit-function "maybe-insert-[]-pair" (λ (text) (maybe-insert-brace-pair text #\[ #\])))
  (add-edit-function "maybe-insert-{}-pair" (λ (text) (maybe-insert-brace-pair text #\{ #\})))
  (add-edit-function "maybe-insert-\"\"-pair" (λ (text) (maybe-insert-brace-pair text #\" #\")))
  (add-edit-function "maybe-insert-||-pair" (λ (text) (maybe-insert-brace-pair text #\| #\|)))
  
  (add-edit-function "maybe-insert-[]-pair-maybe-fixup-[]"
                     (λ (text)
                       (cond
                         [(or (not (preferences:get 'framework:fixup-open-parens))
                              (send text is-stopped?))
                          (maybe-insert-brace-pair text #\[ #\])]
                         [else 
                          (insert-paren text)])))
  
  (define (insert-lambda-template edit)
    (send edit begin-edit-sequence)
    (let ([selection-start (send edit get-start-position)])
      (send edit set-position (send edit get-end-position))
      (send edit insert ")")
      (send edit set-position selection-start)
      (send edit insert ") ")
      (send edit set-position selection-start)
      (send edit insert "(λ ("))
    (send edit end-edit-sequence))
  
  (add-edit-function "insert-lambda-template" insert-lambda-template)
  
  (define (map-meta key func) (keymap:send-map-function-meta keymap key func))
  (define (map key func) (send keymap map-function key func))
  
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
  (map "m:right" "forward-sexp")
  (map-meta "s:right" "select-forward-sexp")
  (map "a:s:right" "select-forward-sexp")
  (map "m:s:right" "select-forward-sexp")
  
  (map-meta "left" "backward-sexp")
  (map "a:left" "backward-sexp")
  (map "m:left" "backward-sexp")
  (map-meta "s:left" "select-backward-sexp")
  (map "a:s:left" "select-backward-sexp")
  (map "m:s:left" "select-backward-sexp")
  
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

  (map-meta ")" "balance-parens-forward")
  (map-meta "]" "balance-parens-forward")
  (map-meta "}" "balance-parens-forward")
  
  (map-meta "(" "insert-()-pair")
  (map-meta "[" "insert-[]-pair")
  (map-meta "{" "insert-{}-pair")
  (map-meta "\"" "insert-\"\"-pair")
  (map-meta "|" "insert-||-pair")

  (map "(" "maybe-insert-()-pair")
  (map "[" "maybe-insert-[]-pair-maybe-fixup-[]")
  (map "{" "maybe-insert-{}-pair")
  (map "\"" "maybe-insert-\"\"-pair")
  (map "|" "maybe-insert-||-pair")

  (map-meta "s:l" "insert-lambda-template")

  (map "c:c;c:b" "remove-parens-forward")
  (map "c:c;c:l" "introduce-let-ans")
  (map "c:c;c:o" "move-sexp-out")
  (map "c:c;c:e" "kill-enclosing-parens")
  (map "c:c;c:[" "toggle-round-square-parens"))

(define keymap (make-object keymap:aug-keymap%))
(setup-keymap keymap)
(define (get-keymap) keymap)

;; choose-paren : racket-text number -> character
;; returns the character to replace a #\[ with, based
;; on the context where it is typed in.
(define (insert-paren text)
  (let* ([pos (send text get-start-position)]
         [real-char #\[]
         [change-to (λ (i c) 
                      ;(printf "change-to, case ~a\n" i)
                      (set! real-char c))]
         [start-pos (send text get-start-position)]
         [end-pos (send text get-end-position)]
         [letrec-like-forms (preferences:get 'framework:square-bracket:letrec)])
    (send text begin-edit-sequence #f #f)
    (if (and (send text get-overwrite-mode) (= start-pos end-pos))
        (send text insert "[" start-pos (add1 start-pos) #f)
        (send text insert "[" start-pos 'same #f))
    (when (eq? (send text classify-position pos) 'parenthesis)
      (let* ([before-whitespace-pos (send text skip-whitespace pos 'backward #t)]
             [keyword/distance (find-keyword-and-distance before-whitespace-pos text)])
        (cond
          [(and keyword/distance
                (member keyword/distance
                        (preferences:get 'framework:square-bracket:cond/offset)))
           ;; just leave the square backet in, in this case
           (void)]
          [(and keyword/distance
                (member (car keyword/distance)
                        (preferences:get 'framework:square-bracket:local)))
           (unless (= (cadr keyword/distance) 0)
             (change-to 7 #\())]
          [else
           (let* ([backward-match (send text backward-match before-whitespace-pos 0)]
                  [b-m-char (and (number? backward-match) (send text get-character backward-match))])
             (cond
               [backward-match
                ;; there is an expression before this, at this layer
                (define before-whitespace-pos2 
                  (send text skip-whitespace backward-match 'backward #t))
                (define backward-match2 (send text backward-match before-whitespace-pos2 0))
                (cond
                  [(member b-m-char '(#\( #\[ #\{))
                   ;; found a "sibling" parenthesized sequence. use the parens it uses.
                   (change-to 1 b-m-char)]
                  [else
                   ;; otherwise, we switch to (
                   (change-to 2 #\()])]
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
                          (change-to 3 #\()]
                         [(and (beginning-of-sequence? text second-backwards-match)
                               (ormap (λ (x) (text-between-equal? x
                                                                  text
                                                                  second-backwards-match
                                                                  second-before-whitespace-pos))
                                      letrec-like-forms))
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
                                    (eq? (send text classify-position second-backwards-match)
                                         ;;; otherwise, this isn't a `let loop', it is a regular let!
                                         'symbol)
                                    (member "let" letrec-like-forms)
                                    (text-between-equal? "let"
                                                         text
                                                         second-backwards-match2
                                                         second-before-whitespace-pos2))
                               ;; found the `(let loop (' so we keep the [
                               (void)]
                              [else
                               ;; otherwise, round.
                               (change-to 4 #\()]))]))]
                    [else 
                     (change-to 5 #\()]))]
               [else 
                (change-to 6 #\()]))])))
    (send text delete pos (+ pos 1) #f)
    (send text end-edit-sequence)
    (cond
      [(and (preferences:get 'framework:automatic-parens)
            (not (in-string/comment? text)))
       (send text insert real-char start-pos start-pos)
       (when (eq? (send text classify-position start-pos) 'parenthesis)
         (send text insert (case real-char
                           [(#\() #\)]
                           [(#\[) #\]]
                           [(#\{) #\}])
             (+ end-pos 1) (+ end-pos 1))
         (send text set-position (+ start-pos 1)))]
      [else
       (send text insert real-char start-pos end-pos)])))

;; find-keyword-and-distance : -> (union #f (cons string number))
(define (find-keyword-and-distance before-whitespace-pos text)
  ;; searches backwards for the keyword in the sequence at this level.
  ;; if found, it counts how many sexps back it was 
  (let loop ([pos before-whitespace-pos]
             [n 0])
    (let ([backward-match (send text backward-match pos 0)])
      (cond
        [backward-match
         (let ([before-whitespace-pos (send text skip-whitespace backward-match 'backward #t)])
           (loop before-whitespace-pos
                 (if (send text stick-to-next-sexp? backward-match)
                     n
                     (+ n 1))))]
        [else
         (let* ([afterwards (send text get-forward-sexp pos)]
                [keyword
                 (and afterwards
                      (send text get-text pos afterwards))])
           (and keyword
                (list keyword (- n 1))))]))))

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


;                       ;;;                                            ;;;   
;                      ;                                                 ;   
;                      ;                                                 ;   
; ;;;   ; ;;;   ;;;   ;;;;;   ;;;         ; ;;;   ;;;;  ; ;;;    ;;;     ;   
;;   ;   ;     ;   ;   ;     ;   ;         ;   ;      ;  ;;  ;  ;   ;    ;   
;;   ;   ;     ;;;;;   ;      ;;;          ;   ;   ;;;;  ;   ;  ;;;;;    ;   
;;   ;   ;     ;       ;         ;         ;   ;  ;   ;  ;   ;  ;        ;   
;;   ;   ;     ;   ;   ;     ;   ;         ;   ;  ;   ;  ;   ;  ;   ;    ;   
;;;;;   ;;;;    ;;;   ;;;;    ;;;          ;;;;    ;;; ;;;;  ;;  ;;;   ;;;;;;
;;                                         ;                                 
;;                                         ;                                 
;;;;                                       ;;;                                


(define (add-preferences-panel)
  (preferences:add-panel
   (list (string-constant editor-prefs-panel-label) 
         (string-constant indenting-prefs-panel-label))
   make-indenting-prefs-panel)
  (preferences:add-panel
   (list (string-constant editor-prefs-panel-label) 
         (string-constant square-bracket-prefs-panel-label))
   make-square-bracket-prefs-panel))

(define (make-square-bracket-prefs-panel p)
  (define main-panel (make-object vertical-panel% p))
  (define boxes-panel (new horizontal-panel% [parent main-panel]))
  
  (define (mk-list-box sym keyword-type pref->string get-new-one)
    (letrec ([vp (new vertical-panel% [parent boxes-panel])]
             [_ (new message% 
                     [label (format (string-constant x-like-keywords) keyword-type)]
                     [parent vp])]
             [lb
              (new list-box% 
                   [label #f]
                   [parent vp]
                   [choices (map pref->string (preferences:get sym))]
                   [callback
                    (λ (lb evt)
                      (send remove-button enable (pair? (send lb get-selections))))])]
             [bp (new horizontal-panel% [parent vp] [stretchable-height #f])]
             [add
              (new button%
                   [label (string-constant add-keyword)]
                   [parent bp]
                   [callback 
                    (λ (x y)
                      (let ([new-one (get-new-one)])
                        (when new-one
                          (preferences:set sym (append (preferences:get sym) 
                                                       (list new-one))))))])]
             [remove-button
              (new button%
                   [label (string-constant remove-keyword)]
                   [parent bp]
                   [callback 
                    (λ (x y)
                      (let ([n (send lb get-selections)])
                        (when (pair? n)
                          (preferences:set 
                           sym 
                           (let loop ([i 0]
                                      [prefs (preferences:get sym)])
                             (cond
                               [(= i (car n)) (cdr prefs)]
                               [else (cons (car prefs)
                                           (loop (+ i 1)
                                                 (cdr prefs)))])))
                          (cond
                            [(= 0 (send lb get-number))
                             (send remove-button enable #f)]
                            [else
                             (send lb set-selection 
                                   (if (= (car n) (send lb get-number))
                                       (- (send lb get-number) 1)
                                       (car n)))]))))])])
      (unless (pair? (send lb get-selections))
        (send remove-button enable #f))
      (preferences:add-callback sym
                                (λ (p v)
                                  (send lb clear)
                                  (for-each (λ (x) (send lb append (pref->string x))) v)))))
  
  (define (get-new-simple-keyword label)
    (λ ()
      (let ([new-one
             (keymap:call/text-keymap-initializer
              (λ ()
                (get-text-from-user
                 (format (string-constant enter-new-keyword) label)
                 (format (string-constant x-keyword) label))))])
        (and new-one
             (let ([parsed (with-handlers ((exn:fail:read? (λ (x) #f)))
                             (read (open-input-string new-one)))])
               
               (and (symbol? parsed)
                    (symbol->string parsed)))))))
  
  (define (get-new-cond-keyword)
    (define f (new dialog% [label (format (string-constant enter-new-keyword) "Cond")]))
    (define tb (keymap:call/text-keymap-initializer
                (λ ()
                  (new text-field% 
                       [parent f]
                       [label #f]))))
    (define number-panel (new horizontal-panel% [parent f] [stretchable-height #f]))
    (define number-label (new message% 
                              [parent number-panel]
                              [label (string-constant skip-subexpressions)]))
    (define number
      (keymap:call/text-keymap-initializer
       (λ ()
         (new text-field% 
              [parent number-panel]
              [init-value "1"]
              [min-width 50]
              [label #f]))))
    
    (define answers #f)
    (define bp (new horizontal-panel% 
                    [parent f] 
                    [stretchable-height #f]
                    [alignment '(right center)]))
    (define (confirm-callback b e)
      (let ([n (string->number (send number get-value))]
            [sym (with-handlers ([exn:fail:read? (λ (x) #f)])
                   (read (open-input-string (send tb get-value))))])
        (when (and (number? n)
                   (symbol? sym))
          (set! answers (list (symbol->string sym) n)))
        (send f show #f)))
    
    (define (cancel-callback b e)
      (send f show #f))
    
    (define-values (ok-button cancel-button)
      (gui-utils:ok/cancel-buttons bp confirm-callback cancel-callback 
                                   (string-constant ok) (string-constant cancel)))
    (send tb focus)
    (send f show #t)
    answers)
  
  (define stupid-internal-definition-syntax1
    (mk-list-box 'framework:square-bracket:letrec "Letrec" values (get-new-simple-keyword "Letrec")))
  (define stupid-internal-definition-syntax3
    (mk-list-box 'framework:square-bracket:local 
                 "Local" 
                 values
                 (get-new-simple-keyword "Local")))
  (define stupid-internal-definition-syntax2
    (mk-list-box 'framework:square-bracket:cond/offset 
                 "Cond" 
                 (λ (l) (format "~a (~a)" (car l) (cadr l)))
                 get-new-cond-keyword))
  
  (define check-box (new check-box% 
                         [parent main-panel]
                         [label (string-constant fixup-open-brackets)]
                         [value (preferences:get 'framework:fixup-open-parens)]
                         [callback 
                          (λ (x y)
                            (preferences:set 'framework:fixup-open-parens 
                                             (send check-box get-value)))]))
  (preferences:add-callback
   'framework:fixup-open-parens
   (λ (p v)
     (send check-box set-value v)))
  
  main-panel)

(define (make-indenting-prefs-panel p)
  (define get-keywords
    (λ (hash-table)
      (letrec ([all-keywords (hash-map hash-table list)]
               [pick-out (λ (wanted in out)
                           (cond
                             [(null? in) (sort out string<=?)]
                             [else (if (eq? wanted (cadr (car in))) 
                                       (pick-out wanted (cdr in) 
                                                 (cons (format "~s" (car (car in))) out))
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
                      (hash-ref (car (preferences:get 'framework:tabify))
                                parsed
                                (λ () #f)))
                 (message-box (string-constant error)
                              (format (string-constant already-used-keyword) parsed))]
                [(symbol? parsed)
                 (let* ([pref (preferences:get 'framework:tabify)]
                        [ht (car pref)])
                   (hash-set! ht parsed keyword-symbol)
                   (preferences:set 'framework:tabify pref)
                   (update-list-boxes ht))]
                [else (message-box 
                       (string-constant error)
                       (format (string-constant expected-a-symbol) new-one))])))))))
  (define delete-callback
    (λ (list-box)
      (λ (button command)
        (define selections (send list-box get-selections))
        (define symbols 
          (map (λ (x) (read (open-input-string (send list-box get-string x)))) selections))
        (for-each (λ (x) (send list-box delete x)) (reverse selections))
        (define pref (preferences:get 'framework:tabify))
        (define ht (car pref))
        (for-each (λ (x) (hash-remove! ht x)) symbols)
        (preferences:set 'framework:tabify pref))))
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
  (define (update-pref sel x)
    (let ([pref (preferences:get 'framework:tabify)])
      (let ([pref
             (let loop ([pref pref][sel sel])
               (if (zero? sel)
                   (cons x (cdr pref))
                   (cons (car pref) (loop (cdr pref) (sub1 sel)))))])
        (preferences:set 'framework:tabify pref))))
  (define-values (begin-list-box begin-regexp-text) 
    (make-column "Begin"
                 'begin
                 begin-keywords
                 (λ (x) (update-pref 1 x))))
  (define-values (define-list-box define-regexp-text) 
    (make-column "Define" 
                 'define 
                 define-keywords
                 (λ (x) (update-pref 2 x))))
  (define-values (lambda-list-box lambda-regexp-text)
    (make-column "Lambda"
                 'lambda
                 lambda-keywords
                 (λ (x) (update-pref 3 x))))
  (define (update-list-boxes hash-table)
    (let-values ([(begin-keywords define-keywords lambda-keywords) (get-keywords hash-table)]
                 [(reset) (λ (list-box keywords)
                            (send list-box clear)
                            (for-each (λ (x) (send list-box append x)) keywords))])
      (reset begin-list-box begin-keywords)
      (reset define-list-box define-keywords)
      (reset lambda-list-box lambda-keywords)
      #t))
  (define update-gui
    (λ (pref)
      (update-list-boxes (car pref))
      (send begin-regexp-text set-value (or (object-name (cadr pref)) ""))
      (send define-regexp-text set-value (or (object-name (caddr pref)) ""))
      (send lambda-regexp-text set-value (or (object-name (cadddr pref)) ""))))
  (preferences:add-callback 'framework:tabify (λ (p v) (update-gui v)))
  (update-gui (preferences:get 'framework:tabify))
  main-panel)
