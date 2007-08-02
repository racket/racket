#|

profile todo:
  - use origin fields
  - sort out various ways of clearing out the profiling information

|#

(module debug mzscheme
  (require (lib "unit.ss")
           (lib "stacktrace.ss" "errortrace")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "file.ss")
           "drsig.ss"
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           (lib "bday.ss" "framework" "private")
           "bindings-browser.ss")
  
  (define orig (current-output-port))
  
  (provide debug@)
  (define-unit debug@
    (import [prefix drscheme:rep: drscheme:rep^]
            [prefix drscheme:frame: drscheme:frame^]
            [prefix drscheme:unit: drscheme:unit^]
            [prefix drscheme:language: drscheme:language^]
            [prefix drscheme:language-configuration: drscheme:language-configuration/internal^]
            [prefix drscheme:init: drscheme:init^])
    (export drscheme:debug^)
    
    
    (define (oprintf . args) (apply fprintf orig args))
    
    
    
    ;;                             ;                        
    ;                                                      
    ;                                                      
    ;;;;  ;;  ;;   ;;; ;        ;;;     ;;;    ;;;  ; ;;;  
    ;   ;  ;   ;  ;   ;           ;    ;   ;  ;   ;  ;;  ; 
    ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
    ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
    ;   ;  ;   ;  ;   ;           ;    ;   ;  ;   ;  ;   ; 
    ; ;;;    ;;; ;  ;;;;         ;;;;;   ;;;    ;;;  ;;;  ;;
    ;                                    
    ;                                    
    ;;;                                     
    
    ;; type debug-source = (union symbol (instanceof editor<%>))
    
    ;; original-output-port : output-port
    ;; for debugging -- be sure to print to here, not the current output port
    (define original-output-port (current-output-port))
    
    ;; cm-key : symbol
    ;; the key used to put information on the continuation
    (define cm-key (gensym 'drscheme-debug-continuation-mark-key))
    
    (define (get-cm-key) cm-key)
    
    ;; error-delta : (instanceof style-delta%)
    (define error-delta (make-object style-delta% 'change-style 'italic))
    (send error-delta set-delta-foreground (make-object color% 255 0 0))
    
    ;; get-error-color : -> (instanceof color%)
    (define get-error-color
      (let ([w-o-b (make-object color% 63 0 0)]
            [b-o-w (make-object color% "PINK")])
        (λ ()
          (if (preferences:get 'framework:white-on-black?)
              w-o-b
              b-o-w))))
    
    (define clickable-image-snip%
      (class image-snip%
        (init-rest args)
        (inherit get-flags set-flags get-admin get-extent)
        
        (define callback void)
        (define/public (set-callback cb) (set! callback cb))
        (define/public (get-callback) callback)
        
        (define grabbed? #f)
        (define clicked? #f)
        (define mouse-x #f)
        (define mouse-y #f)
        
        (define/override (draw dc x y left top right bottom dx dy draw-caret)
          (super draw dc x y left top right bottom dx dy draw-caret)
          (when clicked?
            (let ([brush (send dc get-brush)]
                  [pen (send dc get-pen)])
              (let-values ([(w h) (get-w/h dc)])
                (send dc set-brush (send the-brush-list find-or-create-brush "black" 'hilite))
                (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
                (send dc draw-rectangle x y w h)
                (send dc set-pen pen)
                (send dc set-brush brush)))))
        
        (define/override (on-event dc x y editorx editory evt)
          (cond
            [(send evt button-down? 'left)
             (set! grabbed? #t)
             (set! clicked? #t)
             (set! mouse-x x)
             (invalidate dc)]
            [(send evt leaving?)
             (set! clicked? #f)
             (set! mouse-x #f)
             (set! mouse-y #f)
             (invalidate dc)]
            [(send evt button-up? 'left)
             (when clicked?
               (callback))
             (set! grabbed? #f)
             (set! clicked? #f)
             (invalidate dc)]))
        
        (define/private (invalidate dc)
          (let ([admin (get-admin)])
            (when admin
              (let-values ([(w h) (get-w/h dc)])
                (send admin needs-update this 0 0 w h)))))
        
        (define/private (get-w/h dc)
          (let ([wb (box 0)]
                [hb (box 0)])
            ;; know that the snip is the same size everywhere, 
            ;; so just use (0,0) for its position
            (get-extent dc 0 0 wb hb #f #f #f #f)
            (values (unbox wb)
                    (unbox hb))))
        
        (apply super-make-object args)
        (set-flags (cons 'handles-events (get-flags)))))
    
    ;; make-note% : string -> (union class #f)
    (define (make-note% filename flag)
      (let ([bitmap (make-object bitmap% 
                      (build-path (collection-path "icons") filename)
                      flag)])
        (and (send bitmap ok?)
             (letrec ([note%
                       (class clickable-image-snip%
                         (inherit get-callback)
                         (define/public (get-image-name) filename)
                         (define/override (copy) 
                           (let ([n (new note%)])
                             (send n set-callback (get-callback))
                             n))
                         (super-make-object bitmap))])
               note%))))
    
    (define bug-note% (make-note% "bug09.png" 'png/mask))
    (define mf-note% (make-note% "mf.gif" 'gif))
    (define file-note% (make-note% "file.gif" 'gif))
    
    ;; display-stats : (syntax -> syntax)
    ;; count the number of syntax expressions & number of with-continuation-marks in an 
    ;; expanded expression ... except that it counts keywords, too.
    ;; returns its argument.
    ;(define (display-stats stx)
    ;  (let ([exps 0]
    ;        [wcms 0])
    ;    (let loop ([stx stx])
    ;      (kernel-syntax-case stx ()
    ;        [(#%with-continuation-mark key mark body)
    ;         (set! wcms (+ wcms 1))
    ;         (loop #`body)]
    ;        [(subexps ...)
    ;         (set! exps (+ exps 1))
    ;         (for-each loop (syntax->list stx))]
    ;        [exp
    ;         (set! exps (+ exps 1))]))
    ;    (fprintf (current-error-port) "exps: ~v\nwcms: ~v\n" exps wcms))
    ;  stx)
    
    ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
    ;; adds debugging information to `sexp' and calls `oe'
    (define (make-debug-eval-handler oe)
      (let ([debug-tool-eval-handler
             (λ (orig-exp)
               (if (compiled-expression? (if (syntax? orig-exp)  
                                             (syntax-e orig-exp)  
                                             orig-exp))
                   (oe orig-exp)
                   (let loop ([exp (if (syntax? orig-exp)
                                       orig-exp
                                       (namespace-syntax-introduce
                                        (datum->syntax-object #f orig-exp)))])
                     (let ([top-e (expand-syntax-to-top-form exp)]) 
                       (syntax-case top-e (begin) 
                         [(begin expr ...)
                          ;; Found a `begin', so expand/eval each contained 
                          ;; expression one at a time 
                          (let i-loop ([exprs (syntax->list #'(expr ...))]
                                       [last-one (list (void))])
                            (cond
                              [(null? exprs) 
                               (apply values last-one)]
                              [else 
                               (i-loop (cdr exprs)
                                       (call-with-values 
                                        (λ () 
                                          (call-with-continuation-prompt
                                           (λ () (loop (car exprs)))
                                           (default-continuation-prompt-tag)
                                           (λ args
                                             (apply
                                              abort-current-continuation 
                                              (default-continuation-prompt-tag)
                                              args))))
                                        list))]))]
                         [_else 
                          ;; Not `begin', so proceed with normal expand and eval 
                          (let* ([annotated (annotate-top (expand-syntax top-e) #f)])
                            (oe annotated))])))))])
        debug-tool-eval-handler))
    
    ;; make-debug-error-display-handler/text  : (-> (union #f (is-a?/c text%)))
    ;;                                              ((listof (list text% number number)) -> void)
    ;;                                              (string (union TST exn) -> void)
    ;;                                             -> string (union TST exn)
    ;;                                             -> void
    (define (make-debug-error-display-handler/text get-rep highlight-errors orig-error-display-handler)
      (define (debug-error-display-handler msg exn)
        (let ([rep (get-rep)])
          (cond
            [rep
             (show-error-and-highlight 
              msg 
              exn
              (λ (srcs-to-display cms)
                (parameterize ([current-eventspace drscheme:init:system-eventspace])
                  (queue-callback
                   (λ ()
                     ;; need to make sure that the user's eventspace is still the same
                     ;; and still running here?
                     (highlight-errors rep srcs-to-display cms))))))]
            [else 
             (orig-error-display-handler msg exn)])))
      debug-error-display-handler)
    
    (define (print-bug-to-stderr msg cms)
      (when (port-writes-special? (current-error-port))
        (let ([note% (if (mf-bday?) mf-note% bug-note%)])
          (when note%
            (let ([note (new note%)])
              (send note set-callback (λ () (show-backtrace-window msg cms)))
              (write-special note (current-error-port))
              (display #\space (current-error-port)))))))
    
    (define (show-error-and-highlight msg exn highlight-errors)
      (let ([cms
             (and (exn? exn) 
                  (continuation-mark-set? (exn-continuation-marks exn))
                  (continuation-mark-set->list 
                   (exn-continuation-marks exn)
                   cm-key))])
        (when (and cms
                   (pair? cms))
          (print-bug-to-stderr msg cms))
        
        (let ([srcs-to-display (find-src-to-display exn cms)])
          (for-each display-srcloc-in-error srcs-to-display)
          
          (display msg (current-error-port))
          (when (exn:fail:syntax? exn)
            (show-syntax-error-context (current-error-port) exn))
          (newline (current-error-port))
          
          ;; need to flush here so that error annotations inserted in next line
          ;; don't get erased if this output were to happen after the insertion
          (flush-output (current-error-port))
          
          (highlight-errors srcs-to-display
                            (and cms
                                 (filter 
                                  (λ (x)
                                    (and (pair? x)
                                         (is-a? (car x) text:basic<%>)
                                         (pair? (cdr x))
                                         (number? (cadr x))
                                         (number? (cddr x))))
                                  cms))))))
    
    ;; display-srcloc-in-error : src-loc -> void
    ;; prints out the src location information for src-to-display
    ;; as it would appear in an error message
    (define (display-srcloc-in-error src-to-display)
      (let* ([raw-src (srcloc-source src-to-display)]
             [src (if (and (is-a? raw-src editor<%>)
                           (not (is-a? raw-src drscheme:unit:definitions-text<%>)))
                      (let* ([b (box #f)]
                             [fn (send raw-src get-filename b)])
                        (and (not (unbox b))
                             fn))
                      raw-src)])
        (when (and (path? src) file-note%)
          (when (port-writes-special? (current-error-port))
            (let ([note (new file-note%)])
              (send note set-callback 
                    (λ () (open-and-highlight-in-file src-to-display)))
              (write-special note (current-error-port))
              (display #\space (current-error-port))))
          (display (path->string (find-relative-path (current-directory)
                                                     (normalize-path src)))
                   (current-error-port))
          (let ([line (srcloc-line src-to-display)]
                [col (srcloc-column src-to-display)]
                [pos (srcloc-position src-to-display)])
            (cond
              [(and (number? line) (number? col))
               (fprintf (current-error-port) ":~a:~a" line col)]
              [pos
               (fprintf (current-error-port) "::~a" pos)]))
          (display ": " (current-error-port)))))
    
    ;; find-src-to-display : exn (union #f (listof (list* <src> number number)))
    ;;                    -> (listof srclocs)
    ;; finds the source location to display, choosing between
    ;; the stack trace and the exception record.
    (define (find-src-to-display exn cms)
      (let ([has-info?
             (λ (srcloc)
               (ormap (λ (f) (f srcloc))
                      (list srcloc-column
                            srcloc-line
                            srcloc-position
                            srcloc-source
                            #;srcloc-span)))])  ;; don't consider span alone to count as `info'
        (cond
          [(and (exn:srclocs? exn)
                (ormap has-info? ((exn:srclocs-accessor exn) exn)))
           ((exn:srclocs-accessor exn) exn)]
          [(pair? cms)
           (let ([fst (car cms)])
             (list (make-srcloc (car fst)
                                #f
                                #f
                                (cadr fst)
                                (cddr fst))))]
          [else '()])))
    
    
    (define (show-syntax-error-context port exn)
      (let ([error-text-style-delta (make-object style-delta%)]
            [send-out
             (λ (msg f) 
               (if (port-writes-special? (current-error-port))
                   (let ([snp (make-object string-snip% msg)])
                     (f snp)
                     (write-special snp (current-error-port)))
                   (display msg (current-error-port))))])
        (send error-text-style-delta set-delta-foreground (make-object color% 200 0 0))
        (send-out " in:" void)
        (let ([show-one
               (λ (expr)
                 (display " " (current-error-port))
                 (send-out (format "~s" (syntax-object->datum expr))
                           (λ (snp)
                             (send snp set-style
                                   (send the-style-list find-or-create-style
                                         (send snp get-style)
                                         error-text-style-delta)))))]
              [exprs (exn:fail:syntax-exprs exn)])
          (cond
            [(null? exprs) (void)]
            [(null? (cdr exprs)) (show-one (car exprs))]
            [else
             (for-each (λ (expr)
                         (display "\n " (current-error-port))
                         (show-one expr))
                       exprs)]))))
    
    ;; make-debug-error-display-handler : (string (union TST exn) -> void) -> string (union TST exn) -> void
    ;; adds in the bug icon, if there are contexts to display
    (define (make-debug-error-display-handler orig-error-display-handler)
      (make-debug-error-display-handler/text
       (λ ()
         (let ([rep (drscheme:rep:current-rep)])
           (and (is-a? rep drscheme:rep:text<%>)
                (eq? (send rep get-err-port) (current-error-port))
                rep)))
       (λ (rep errs arrows) (send rep highlight-errors errs arrows))
       orig-error-display-handler))
    
    
    ;; insert/clickback : (instanceof text%) (union string (instanceof snip%)) (-> void)
    ;; inserts `note' and a space at the end of `rep'
    ;; also sets a clickback on the inserted `note' (but not the space).
    (define (insert/clickback rep note clickback)
      (let ([before (send rep last-position)])
        (send rep insert (if (string? note)
                             note
                             (send note copy))
              before before)
        (let ([after (send rep last-position)])
          (send rep insert #\space after after)
          (send rep set-clickback before after
                (λ (txt start end)
                  (clickback))))))
    
    ;; with-mark : mark-stx syntax (any? -> syntax) -> syntax
    ;; a member of stacktrace-imports^
    ;; guarantees that the continuation marks associated with cm-key are
    ;; members of the debug-source type, after unwrapped with st-mark-source
    (define (with-mark src-stx expr)
      (let ([source (cond
                      [(path? (syntax-source src-stx))
                       (syntax-source src-stx)]
                      [(is-a? (syntax-source src-stx) editor<%>)
                       (syntax-source src-stx)]
                      [else #f])]
            [position (or (syntax-position src-stx) 0)]
            [span (or (syntax-span src-stx) 0)])
        (if source
            (with-syntax ([expr expr]
                          [mark (list* source position span)]
                          [cm-key cm-key])
              (syntax
               (with-continuation-mark 'cm-key
                 'mark
                 expr)))
            expr)))
    
    ;; current-backtrace-window : (union #f (instanceof frame:basic<%>))
    ;; the currently visible backtrace window, or #f, if none
    (define current-backtrace-window #f)
    
    ;; reset-backtrace-window : -> void
    ;; effect: updates current-backtrace-window
    ;; closes the current backtrace window and creates a new (unshown) one
    (define (reset-backtrace-window)
      (when current-backtrace-window
        (send current-backtrace-window close)
        (set! current-backtrace-window #f))
      
      (set! current-backtrace-window 
            (make-object backtrace-frame%
              (string-constant backtrace-window-title)
              #f
              (preferences:get 'drscheme:backtrace-window-width)
              (preferences:get 'drscheme:backtrace-window-height)
              (preferences:get 'drscheme:backtrace-window-x)
              (preferences:get 'drscheme:backtrace-window-y))))
    
    ;; hide-backtrace-window : -> void
    (define (hide-backtrace-window)
      (when current-backtrace-window
        (send current-backtrace-window close)
        (set! current-backtrace-window #f)))
    
    ;; backtrace-frame% : (extends frame:basic<%>)
    (define backtrace-frame%
      (class (drscheme:frame:basics-mixin (frame:standard-menus-mixin frame:basic%))
        (define/override (on-size x y)
          (preferences:set 'drscheme:backtrace-window-width x)
          (preferences:set 'drscheme:backtrace-window-height y)
          (super on-size x y))
        (define/override (on-move x y)
          (preferences:set 'drscheme:backtrace-window-x x)
          (preferences:set 'drscheme:backtrace-window-y y)
          (super on-move x y))
        (define/override (edit-menu:between-find-and-preferences edit-menu) (void))
        (define/override (edit-menu:between-select-all-and-find edit-menu) (void))
        (define/override (file-menu:between-save-as-and-print file-menu) (void))
        (define/augment (on-close) 
          (set! current-backtrace-window #f)
          (inner (void) on-close))
        (super-new)))
    
    ;; show-backtrace-window : string
    ;;                         (listof mark?)
    ;;                         -> 
    ;;                         void
    (define (show-backtrace-window error-text dis)
      (reset-backtrace-window)
      (letrec ([text (make-object (text:wide-snip-mixin text:hide-caret/selection%))]
               [mf-bday-note (when (mf-bday?)
                               (instantiate message% ()
                                 (label (string-constant happy-birthday-matthias))
                                 (parent (send current-backtrace-window get-area-container))))]
               [ec (make-object (canvas:color-mixin canvas:wide-snip%)
                     (send current-backtrace-window get-area-container)
                     text)]
               [di-vec (list->vector dis)]
               [index 0]
               [how-many-at-once 15]
               [show-next-dis
                (λ ()
                  (let ([start-pos (send text get-start-position)]
                        [end-pos (send text get-end-position)])
                    (send text begin-edit-sequence)
                    (send text set-position (send text last-position))
                    (let loop ([n index])
                      (cond
                        [(and (< n (vector-length di-vec))
                              (< n (+ index how-many-at-once)))
                         (show-frame ec text (vector-ref di-vec n))
                         (loop (+ n 1))]
                        [else
                         (set! index n)]))
                    
                    ;; add 'more frames' link
                    (when (< index (vector-length di-vec))
                      (let ([end-of-current (send text last-position)])
                        (send text insert #\newline)
                        (let ([hyper-start (send text last-position)])
                          (send text insert 
                                (let* ([num-left
                                        (- (vector-length di-vec)
                                           index)]
                                       [num-to-show
                                        (min how-many-at-once
                                             num-left)])
                                  (if (= num-left 1)
                                      (string-constant last-stack-frame)
                                      (format (if (num-left . <= . num-to-show)
                                                  (string-constant last-stack-frames)
                                                  (string-constant next-stack-frames))
                                              num-to-show))))
                          (let ([hyper-end (send text last-position)])
                            (send text change-style (gui-utils:get-clickback-delta
                                                     (preferences:get 'framework:white-on-black?))
                                  hyper-start hyper-end)
                            (send text set-clickback
                                  hyper-start hyper-end
                                  (λ x
                                    (send text begin-edit-sequence)
                                    (send text lock #f)
                                    (send text delete end-of-current (send text last-position))
                                    (show-next-dis)
                                    (send text set-position 
                                          (send text last-position)
                                          (send text last-position))
                                    (send text lock #t)
                                    (send text end-edit-sequence)))
                            
                            (send text insert #\newline)
                            (send text set-paragraph-alignment (send text last-paragraph) 'center)))))
                    
                    (send text set-position start-pos end-pos)
                    (send text end-edit-sequence)))])
        (send current-backtrace-window set-alignment 'center 'center)
        (send current-backtrace-window reflow-container)
        (send text auto-wrap #t)
        (send text set-autowrap-bitmap #f)
        (send text insert error-text)
        (send text insert "\n\n")
        (send text change-style error-delta 0 (- (send text last-position) 1))
        (show-next-dis)
        (send text set-position 0 0)
        (send text lock #t)
        (send text hide-caret #t)
        (send current-backtrace-window show #t)))
    
    ;; show-frame : (instanceof editor-canvas%)
    ;;              (instanceof text%) 
    ;;              st-mark?
    ;;              -> 
    ;;              void 
    ;; shows one frame of the continuation
    (define (show-frame editor-canvas text di)
      (let* ([debug-source (car di)]
             [start (cadr di)]
             [span (cddr di)]
             [fn (get-filename debug-source)]
             [start-pos (send text last-position)])
        
        ;; make hyper link to the file
        (send text insert (format "~a: ~a-~a" fn start (+ start span)))
        (let ([end-pos (send text last-position)])
          (send text insert " ")
          (send text change-style 
                (gui-utils:get-clickback-delta (preferences:get 'framework:white-on-black?))
                start-pos 
                end-pos)
          (send text set-clickback
                start-pos end-pos
                (λ x
                  (open-and-highlight-in-file (make-srcloc debug-source #f #f start span)))))
        
        ;; make bindings hier-list
        (let ([bindings (st-mark-bindings di)])
          (when (not (null? bindings))
            (send text insert (render-bindings/snip bindings))))
        (send text insert #\newline)
        
        (insert-context editor-canvas text debug-source start span)
        (send text insert #\newline)))
    
    ;; insert-context : (instanceof editor-canvas%)
    ;;                  (instanceof text%)
    ;;                  debug-info
    ;;                  number
    ;;                  -> 
    ;;                  void
    (define (insert-context editor-canvas text file start span)
      (let-values ([(from-text close-text)
                    (cond
                      [(symbol? file)
                       ;; can this case happen?
                       (let ([text (new text:basic%)])
                         (if (send text load-file (symbol->string file))
                             (values text 
                                     (λ () (send text on-close)))
                             (values #f (λ () (void)))))]
                      [(path? file)
                       (let ([text (new text:basic%)])
                         (if (send text load-file file)
                             (values text 
                                     (λ () (send text on-close)))
                             (values #f (λ () (void)))))]
                      [(is-a? file editor<%>)
                       (values file void)]
                      [else (error 'insert-context "unknown file spec ~e" file)])])
        (when from-text
          (let* ([finish (+ start span -1)]
                 [context-text (copy/highlight-text from-text start finish)])
            (send context-text lock #t)
            (send context-text hide-caret #t)
            (send text insert "  ")
            (let ([snip (make-object editor-snip% context-text)])
              (send snip use-style-background #t)
              (send editor-canvas add-wide-snip snip)
              (let ([p (send text last-position)])
                (send text insert snip p p)
                (send text insert #\newline)
                (when (preferences:get 'framework:white-on-black?)
                  (send text change-style white-on-black-style p (+ p 1))))))
          (close-text))))
    
    (define white-on-black-style (make-object style-delta%))
    (define stupid-internal-define-syntax1 (send white-on-black-style set-delta-foreground "white"))
    
    ;; copy/highlight-text : text number number -> text
    ;; copies the range from `start' to `finish', including the entire paragraph at
    ;; each end and highlights the characters corresponding the original range,
    ;; in the resulting text
    (define (copy/highlight-text from-text start finish)
      (let* ([to-text (new text:standard-style-list%)]
             [para-start-pos (send from-text paragraph-start-position 
                                   (send from-text position-paragraph start))]
             [para-end-pos (send from-text paragraph-end-position
                                 (send from-text position-paragraph 
                                       finish))]
             [from-start (- start para-start-pos)]
             [from-end (+ from-start (- finish start))])
        (send from-text split-snip para-start-pos)
        (send from-text split-snip para-end-pos)
        (let loop ([snip (send from-text find-snip para-start-pos 'after-or-none)])
          (when (and snip
                     (< (send from-text get-snip-position snip) para-end-pos))
            (send to-text insert (send snip copy))
            (loop (send snip next))))
        (send to-text highlight-range (- from-start 1) from-end (get-error-color) #f #f 'high)
        to-text))
    
    ;; get-filename : debug-source -> string
    (define (get-filename file)
      (cond
        [(symbol? file) (symbol->string file)]
        [(path? file) (path->string file)]
        [(is-a? file editor<%>)
         (get-filename-from-editor file)]))
    
    ;; get-filename-from-editor : (is-a?/c editor<%>) -> string
    (define (get-filename-from-editor editor)
      (let* ([untitled (string-constant unknown-debug-frame)]
             [canvas (send editor get-canvas)]
             [frame (and canvas (send canvas get-top-level-window))])
        (if (is-a? frame drscheme:unit:frame%)
            (let ([filename (send (send frame get-definitions-text) get-filename)])
              (cond
                [(and filename (eq? editor (send frame get-interactions-text)))
                 (format (string-constant files-interactions) filename)]
                [(eq? editor (send frame get-interactions-text))
                 (string-constant current-interactions)]
                [filename filename]
                [else (string-constant current-definitions)]))
            (or (send editor get-filename) 
                untitled))))
    
    ;; open-and-highlight-in-file : srcloc -> void
    (define (open-and-highlight-in-file srcloc)
      (let* ([debug-source (srcloc-source srcloc)]
             [position (srcloc-position srcloc)]
             [span (srcloc-span srcloc)]
             [frame (cond
                      [(path? debug-source) (handler:edit-file debug-source)]
                      [(is-a? debug-source editor<%>)
                       (let ([canvas (send debug-source get-canvas)])
                         (and canvas
                              (send canvas get-top-level-window)))])]
             [editor (cond
                       [(path? debug-source)
                        (cond
                          [(and frame (is-a? frame drscheme:unit:frame%))
                           (send frame get-definitions-text)]
                          [(and frame (is-a? frame frame:editor<%>))
                           (send frame get-editor)]
                          [else #f])]
                       [(is-a? debug-source editor<%>) debug-source])]
             [rep (and (is-a? frame drscheme:unit:frame%)
                       (send frame get-interactions-text))])
        (when frame
          (send frame show #t))
        (when (and rep editor)
          (when (is-a? editor text:basic<%>)
            (send rep highlight-error editor position (+ position span))
            (send editor set-caret-owner #f 'global)))))
    
    
    
    ;                                                                                      
    ;                                                                                      
    ;                                                                                      
    ;                                                                                      
    ;                                                                                      
    ;   ;                  ;                                                               
    ;  ;;;;   ;;;    ;;;  ;;;;       ;;;    ;;;   ;     ;  ;;;   ; ;  ;;;     ;; ;    ;;;  
    ;   ;    ;   ;  ;      ;        ;   ;  ;   ;   ;   ;  ;   ;  ;;  ;   ;   ;  ;;   ;   ; 
    ;   ;   ;    ;  ;;     ;       ;      ;     ;  ;   ; ;    ;  ;       ;  ;    ;  ;    ; 
    ;   ;   ;;;;;;   ;;    ;       ;      ;     ;   ; ;  ;;;;;;  ;    ;;;;  ;    ;  ;;;;;; 
    ;   ;   ;          ;   ;       ;      ;     ;   ; ;  ;       ;   ;   ;  ;    ;  ;      
    ;   ;    ;         ;   ;        ;   ;  ;   ;     ;    ;      ;   ;   ;   ;  ;;   ;     
    ;    ;;   ;;;;  ;;;     ;;       ;;;    ;;;      ;     ;;;;  ;    ;;;;;   ;; ;    ;;;; 
    ;                                                                            ;         
    ;                                                                       ;    ;         
    ;                                                                        ;;;;          
    
    
    (define test-coverage-enabled (make-parameter #f))
    
    (define current-test-coverage-info (make-thread-cell #f))
    
    (define (initialize-test-coverage-point key expr)
      (unless (hash-table? (thread-cell-ref current-test-coverage-info))
        (let ([rep (drscheme:rep:current-rep)])
          (when rep
            (let ([ut (eventspace-handler-thread (send rep get-user-eventspace))])
              (when (eq? ut (current-thread))
                (let ([ht (make-hash-table)])
                  (thread-cell-set! current-test-coverage-info ht)
                  (send rep set-test-coverage-info ht)))))))
      (let ([ht (thread-cell-ref current-test-coverage-info)])
        (when (hash-table? ht)
          ;; if rep isn't around, we don't do test coverage...
          ;; this can happen when check syntax expands, for example
          (hash-table-put! ht key (list #f expr)))))
    
    (define (test-covered key)
      (let ([ht (thread-cell-ref current-test-coverage-info)])
        (when (hash-table? ht) ;; as in the `when' test in `initialize-test-coverage-point'
          (let ([v (hash-table-get ht key)])
            (set-car! v #t)))))
    
    (define test-coverage-interactions-text<%>
      (interface ()
        set-test-coverage-info
        get-test-coverage-info))
    
    (define test-coverage-tab<%>
      (interface ()
        show-test-coverage-annotations ;; hash-table (union #f style) (union #f style) boolean -> void
        get-test-coverage-info-visible?
        ask-about-clearing-test-coverage?))
    
    (define test-coverage-interactions-text-mixin
      (mixin (drscheme:rep:text<%> text:basic<%>) (test-coverage-interactions-text<%>)
        (inherit get-context)
        (field [test-coverage-info #f]
               [test-coverage-on-style #f]
               [test-coverage-off-style #f]
               [ask-about-reset? #f])
        (define/public set-test-coverage-info
          (opt-lambda (ht [on-style #f] [off-style #f] [ask? #t])
            (set! test-coverage-info ht)
            (set! test-coverage-on-style on-style)
            (set! test-coverage-off-style off-style)
            (set! ask-about-reset? ask?)))
        (define/public (get-test-coverage-info) 
          test-coverage-info)
        
        (inherit get-top-level-window)
        (define/augment (after-many-evals)
          (when test-coverage-info
            (send (get-context) show-test-coverage-annotations
                  test-coverage-info
                  test-coverage-on-style
                  test-coverage-off-style
                  ask-about-reset?))
          (inner (void) after-many-evals))
        
        (super-new)))
    
    (define test-coverage-definitions-text-mixin
      (mixin ((class->interface text%) drscheme:unit:definitions-text<%>) ()
        (inherit get-canvas get-tab)
        
        (define/private (clear-test-coverage?)
          (if (preferences:get 'drscheme:test-coverage-ask-about-clearing?)
              (let ([msg-box-result
                     (message-box/custom
                      (string-constant drscheme)
                      (string-constant test-coverage-clear?)
                      (string-constant yes)
                      (string-constant no)
                      (string-constant test-coverage-clear-and-do-not-ask-again)
                      (send (get-canvas) get-top-level-window)
                      '(default=1)
                      2)])
                (case msg-box-result
                  [(1) #t]
                  [(2) #f]
                  [(3)
                   (preferences:set 'drscheme:test-coverage-ask-about-clearing? #f)
                   #t]))
              #t))
        
        (define/public (clear-test-coverage)
          (let ([tab (get-tab)])
            (when (send tab get-test-coverage-info-visible?)
              (send tab clear-test-coverage-display)
              (let ([it (send tab get-ints)])
                (when (is-a? it test-coverage-interactions-text<%>)
                  (send it set-test-coverage-info #f))))))
        
        (define/private (can-clear-coverage?)
          (let ([tab (get-tab)])
            (or (not tab)
                (not (send tab get-test-coverage-info-visible?))
                (not (send tab ask-about-clearing-test-coverage?))
                (clear-test-coverage?))))
        
        (define/augment (can-insert? x y)
          (and (inner #t can-insert? x y)
               (can-clear-coverage?)))
        
        (define/augment (can-delete? x y)
          (and (inner #t can-delete? x y)
               (can-clear-coverage?)))
        
        (define/augment (after-insert x y)
          (inner (void) after-insert x y)
          (clear-test-coverage))
        
        (define/augment (after-delete x y)
          (inner (void) after-delete x y)
          (clear-test-coverage))
        
        (super-new)))
    
    (define test-covered-style-delta (make-object style-delta%))
    (send test-covered-style-delta set-delta-foreground "forest green")
    
    (define test-not-covered-style-delta (make-object style-delta%))
    (send test-not-covered-style-delta set-delta-foreground "firebrick")
    
    (define erase-test-coverage-style-delta (make-object style-delta% 'change-normal-color))
    
    (define test-coverage-tab-mixin
      (mixin (drscheme:rep:context<%> drscheme:unit:tab<%>) (test-coverage-tab<%>)
        
        (field [internal-clear-test-coverage-display #f])
        
        (define/public (clear-test-coverage-display)
          (when internal-clear-test-coverage-display
            (internal-clear-test-coverage-display)
            (set! internal-clear-test-coverage-display #f)))
        
        (field [ask-about-reset? #t])
        (define/public (ask-about-clearing-test-coverage?) ask-about-reset?)
        
        (define/public (get-test-coverage-info-visible?)
          (not (not internal-clear-test-coverage-display)))
        
        (define/public (show-test-coverage-annotations ht on-style off-style ask?)
          (set! ask-about-reset? ask?)
          (let* ([edit-sequence-ht (make-hash-table)]
                 [locked-ht (make-hash-table)]
                 [already-frozen-ht (make-hash-table)]
                 [actions-ht (make-hash-table 'equal)]
                 [on/syntaxes (hash-table-map ht (λ (_ pr) pr))]
                 
                 ;; can-annotate : (listof (list boolean syntax))
                 ;; boolean is #t => code was run
                 ;;            #f => code was not run
                 ;; remove those that cannot be annotated
                 [can-annotate
                  (filter (λ (pr)
                            (let ([stx (cadr pr)])
                              (and (syntax? stx)
                                   (let ([src (syntax-source stx)]
                                         [pos (syntax-position stx)]
                                         [span (syntax-span stx)])
                                     (and (is-a? src text:basic<%>)
                                          pos
                                          span)))))
                          on/syntaxes)]
                 
                 ;; filtered : (listof (list boolean syntax))
                 ;; remove redundant expressions
                 [filtered
                  (let (;; actions-ht : (list src number number) -> (list boolean syntax)
                        [actions-ht (make-hash-table 'equal)])
                    (for-each
                     (λ (pr)
                       (let* ([stx (cadr pr)]
                              [on? (car pr)]
                              [key (list (syntax-source stx)
                                         (syntax-position stx)
                                         (syntax-span stx))]
                              [old (hash-table-get actions-ht key (λ () 'nothing))])
                         (cond
                           [(eq? old 'nothing) (hash-table-put! actions-ht key (list on? stx))]
                           [(car old) ;; recorded as executed
                            (void)]
                           [(not (car old)) ;; recorded as unexected
                            (when on?
                              (hash-table-put! actions-ht key (list #t stx)))])))
                     can-annotate)
                    (hash-table-map actions-ht (λ (k v) v)))])
            
            ;; if everything is covered *and* no coloring has been done, do no coloring.
            (unless (and (andmap car filtered)
                         (not (get-test-coverage-info-visible?)))
              (let (;; sorted : (listof (list boolean syntax))
                    ;; sorting predicate:
                    ;;  x < y if
                    ;;    x's span is bigger than y's (ie, do larger expressions first)
                    ;;    unless x and y are the same source location.
                    ;;    in that case, color red first and then green
                    [sorted
                     (sort
                      filtered
                      (λ (x y)
                        (let* ([x-stx (cadr x)]
                               [y-stx (cadr y)]
                               [x-pos (syntax-position x-stx)]
                               [y-pos (syntax-position y-stx)]
                               [x-span (syntax-span x-stx)]
                               [y-span (syntax-span y-stx)]
                               [x-on (car x)]
                               [y-on (car y)])
                          (cond
                            [(and (= x-pos y-pos)
                                  (= x-span x-span))
                             (or y-on
                                 (not x-on))]
                            [else (>= x-span y-span)]))))])
                
                ;; turn on edit-sequences in all editors to be touched by new annotations
                ;; also fill in the edit-sequence-ht
                (for-each
                 (λ (pr)
                   (let ([src (syntax-source (cadr pr))])
                     (hash-table-get 
                      edit-sequence-ht
                      src
                      (λ ()
                        (hash-table-put! edit-sequence-ht src #f)
                        (send src begin-edit-sequence #f)
                        (when (send src is-locked?)
                          (hash-table-put! locked-ht src #t)
                          (send src lock #f))))))
                 sorted)
                
                ;; clear out old annotations (and thaw colorers)
                (when internal-clear-test-coverage-display
                  (internal-clear-test-coverage-display)
                  (set! internal-clear-test-coverage-display #f))
                
                ;; freeze the colorers, but avoid a second freeze (so we can avoid a second thaw)
                (hash-table-for-each
                 edit-sequence-ht
                 (λ (src _)
                   (if (send src is-frozen?)
                       (hash-table-put! already-frozen-ht src #t)
                       (send src freeze-colorer))))
                
                ;; set new annotations
                (for-each
                 (λ (pr)
                   (let ([stx (cadr pr)]
                         [on? (car pr)])
                     (when (syntax? stx)
                       (let* ([src (syntax-source stx)]
                              [pos (syntax-position stx)]
                              [span (syntax-span stx)])
                         (send src change-style
                               (if on?
                                   (or on-style test-covered-style-delta)
                                   (or off-style test-not-covered-style-delta))
                               (- pos 1)
                               (+ (- pos 1) span)
                               #f)))))
                 sorted)
                
                ;; relock editors
                (hash-table-for-each 
                 locked-ht
                 (λ (txt _) (send txt lock #t)))
                
                ;; end edit sequences
                (hash-table-for-each 
                 edit-sequence-ht
                 (λ (txt _) (send txt end-edit-sequence)))
                
                ;; save thunk to reset these new annotations
                (set! internal-clear-test-coverage-display
                      (λ ()
                        (hash-table-for-each
                         edit-sequence-ht
                         (λ (txt _) 
                           (send txt begin-edit-sequence #f)))
                        (hash-table-for-each
                         edit-sequence-ht
                         (λ (txt _) 
                           (let ([locked? (send txt is-locked?)])
                             (when locked? (send txt lock #f))
                             (send txt change-style 
                                   erase-test-coverage-style-delta
                                   0
                                   (send txt last-position)
                                   #f)
                             (when locked? (send txt lock #t)))))
                        (hash-table-for-each
                         edit-sequence-ht
                         (λ (txt _) 
                           (unless (hash-table-get already-frozen-ht txt #f)
                             (let ([locked? (send txt is-locked?)])
                               (when locked? (send txt lock #f))
                               (send txt thaw-colorer)
                               (when locked? (send txt lock #t))))
                           (send txt end-edit-sequence)))))))))
        
        (inherit get-defs)
        (define/augment (clear-annotations)
          (inner (void) clear-annotations)
          (send (get-defs) clear-test-coverage))
        
        (super-new)))
    
    
    
    
    
    ;;;    ;    ;;;      ;                 
    ;              ;                        
    ;              ;                        
    ; ;;;   ; ;;;   ;;;   ;;;;;  ;;;      ;    ;;;   ; ;;;    ;;; ;
    ;   ;   ;     ;   ;   ;       ;      ;      ;    ;;  ;  ;   ; 
    ;   ;   ;     ;   ;   ;       ;      ;      ;    ;   ;  ;   ; 
    ;   ;   ;     ;   ;   ;       ;      ;      ;    ;   ;  ;   ; 
    ;   ;   ;     ;   ;   ;       ;      ;      ;    ;   ;  ;   ; 
    ;;;;   ;;;;    ;;;   ;;;;   ;;;;;  ;;;;;; ;;;;; ;;;  ;;  ;;;; 
    ;                                                           ; 
    ;                                                           ; 
    ;;;                                                       ;;;  
    
    
    (define profile-key (gensym))
    
    ;; prof-info =
    ;; (make-prof-info
    ;;    boolean ;; protect against nested calls
    ;;    number[number of calls]
    ;;    number[time spent in all calls]
    ;;   (union #f symbol) 
    ;;   expression)
    (define-struct prof-info (nest num time name expr))
    
    ;; copy-prof-info : prof-info -> prof-info
    (define (copy-prof-info prof-info)
      (make-prof-info (prof-info-nest prof-info)
                      (prof-info-num prof-info)
                      (prof-info-time prof-info)
                      (prof-info-name prof-info)
                      (prof-info-expr prof-info)))
    
    ;; any-info? : prof-info -> boolean
    (define (any-info? prof-info)
      (or (not (zero? (prof-info-num prof-info)))
          (not (zero? (prof-info-time prof-info)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; profiling runtime support
    
    ;; parameter
    ;; imported into errortrace
    (define profiling-enabled (make-parameter #f))
    
    ;; holds a hash-table for the profiling information
    (define current-profile-info (make-thread-cell #f))
    
    
    ;; initialize-profile-point : sym syntax syntax -> void
    ;; called during compilation to register this point as
    ;; a profile point. 
    ;; =user=
    ;; imported into errortrace
    (define (initialize-profile-point key name expr)
      (unless (thread-cell-ref current-profile-info)
        (let ([rep (drscheme:rep:current-rep)])
          (when rep
            (let ([ut (eventspace-handler-thread (send rep get-user-eventspace))])
              (when (eq? ut (current-thread))
                (let ([ht (make-hash-table)])
                  (thread-cell-set! current-profile-info ht)
                  (send (send rep get-context) add-profile-info ht)))))))
      (let ([profile-info (thread-cell-ref current-profile-info)])
        (when profile-info
          (hash-table-put! profile-info
                           key 
                           (make-prof-info #f 0 0 (and (syntax? name) (syntax-e name)) expr))))
      (void))
    
    ;; register-profile-start : sym -> (union #f number)
    ;; =user=
    ;; imported into errortrace
    (define (register-profile-start key)
      (let ([ht (thread-cell-ref current-profile-info)])
        (when ht
          (let ([info (hash-table-get ht key)])
            (set-prof-info-num! info (+ (prof-info-num info) 1))
            (if (prof-info-nest info)
                #f
                (begin
                  (set-prof-info-nest! info #t)
                  (current-process-milliseconds)))))))
    
    ;; register-profile-done : sym (union #f number) -> void
    ;; =user=
    ;; imported into errortrace
    (define (register-profile-done key start)
      (when start
        (let ([ht (thread-cell-ref current-profile-info)])
          (when ht
            (let ([info (hash-table-get ht key)])
              (set-prof-info-nest! info #f)
              (set-prof-info-time! info
                                   (+ (- (current-process-milliseconds) start)
                                      (prof-info-time info)))))))
      (void))
    
    (define (get-color-value/pref val max-val drscheme:profile:low-color drscheme:profile:high-color drscheme:profile:scale)
      (let* ([adjust
              (case drscheme:profile:scale
                [(sqrt) sqrt]
                [(square) (λ (x) (* x x))]
                [(linear) (λ (x) x)])]
             [factor (adjust (if (zero? max-val) 0 (/ val max-val)))]
             [get-rgb-value
              (λ (sel)
                (let ([small (sel drscheme:profile:low-color)]
                      [big (sel drscheme:profile:high-color)])
                  (inexact->exact (floor (+ (* factor (- big small)) small)))))])
        (make-object color% 
          (get-rgb-value (λ (x) (send x red)))
          (get-rgb-value (λ (x) (send x green)))
          (get-rgb-value (λ (x) (send x blue))))))
    
    ;; get-color-value : number number -> (is-a?/c color%)
    ;; returns the profiling color
    ;; for `val' if `max-val' is the largest
    ;; of any profiling amount.
    (define (get-color-value val max-val)
      (get-color-value/pref val 
                            max-val
                            (preferences:get 'drscheme:profile:low-color)
                            (preferences:get 'drscheme:profile:high-color)
                            (preferences:get 'drscheme:profile:scale)))
    
    ;; extract-maximum : (listof prof-info) -> number
    ;; gets the maximum value of the currently preferred profiling info.
    (define (extract-maximum infos)
      (let ([max-value 0]
            [sel (if (eq? (preferences:get 'drscheme:profile-how-to-count) 'time)
                     prof-info-time
                     prof-info-num)])
        (for-each
         (λ (val)
           (set! max-value (max max-value (sel val))))
         infos)
        max-value))
    
    ;; extract-total-time : (listof prof-info) -> number
    (define (extract-total-time infos)
      (let ([sum 0])
        (for-each
         (λ (val) (set! sum (+ sum (prof-info-time val))))
         infos)
        sum))
    
    ;; profile-definitions-mixin : mixin
    (define profile-definitions-text-mixin
      (mixin ((class->interface text%) drscheme:unit:definitions-text<%>) ()
        (inherit get-canvas get-tab)
        
        (define/augment (can-insert? x y)
          (and (inner #t can-insert? x y)
               (can-reset-profile?)))
        
        (define/augment (can-delete? x y)
          (and (inner #t can-delete? x y)
               (can-reset-profile?)))
        
        (define/augment (on-insert x y)
          (inner (void) on-insert x y)
          (do-reset-profile))
        
        (define/augment (on-delete x y)
          (inner (void) on-delete x y)
          (do-reset-profile))
        
        (define/private (can-reset-profile?)
          (let ([canvas (get-canvas)])
            (or (not canvas)
                (let ([frame (send canvas get-top-level-window)])
                  (or (not (send frame get-profile-info-visible?))
                      (eq? (message-box (string-constant drscheme)
                                        (string-constant profiling-clear?)
                                        frame
                                        '(yes-no))
                           'yes))))))
        
        (define/private (do-reset-profile)
          (send (get-tab) reset-profile))
        
        (super-new)))
    
    (define profile-interactions-tab<%>
      (interface ()
        add-profile-info))
    
    (define-local-member-name 
      
      ;; tab methods
      reset-profile  ;; erases profile display & information
      hide-profile  ;; hides profiling info, but it is still here to be shown again
      show-profile  ;; shows the profile info, if there is any to show
      refresh-profile ;; shows current info in profile window
      get-profile-info-text
      can-show-profile?
      get-sort-mode ;; indicates if the results are currently shown sorted by time, or not
      set-sort-mode ;; updates the sort mode flag (only called by the gui control callback)
      
      ;; frame methods
      hide-profile-gui
      show-profile-gui
      
      ;; frame and tab methods
      get-profile-info-visible?
      ; on frame, indicates if the gui stuff shows up currently
      ; on tab, indicates if the user has asked for the gui to show up.
      )
    
    (define profile-tab-mixin
      (mixin (drscheme:unit:tab<%>) (profile-interactions-tab<%>)
        (define profile-info-visible? #f)
        (define/public (get-profile-info-visible?) profile-info-visible?)
        
        (define sort-mode (preferences:get 'drscheme:profile-how-to-count))
        (define/public (get-sort-mode) sort-mode)
        (define/public (set-sort-mode mode) (set! sort-mode mode))
        
        (inherit get-frame is-current-tab?)
        ;; profile-info : (listof hashtable[symbol -o> prof-info])
        (define profile-info '())
        (define/public (add-profile-info ht) (set! profile-info (cons ht profile-info)))
        
        (define/public (show-profile)
          (unless profile-info-visible?
            (set! profile-info-visible? #t)
            (send (get-frame) show-profile-gui)))
        
        (define/public (hide-profile)
          (when profile-info-visible?
            (set! profile-info-visible? #f)
            (send profile-info-text clear-profile-display)
            (when (is-current-tab?)
              (send (get-frame) hide-profile-gui))))
        
        (define/public (reset-profile)
          (hide-profile)
          (set! profile-info '()))
        
        (define/public (refresh-profile) 
          (send profile-info-text refresh-profile profile-info))
        
        ;; can-show-profile? : -> boolean
        ;; indicates if there is any profiling information to be shown.
        (define/public (can-show-profile?)
          (let/ec esc-k
            (for-each
             (λ (ht)
               (hash-table-for-each
                ht
                (λ (key v)
                  (when (any-info? v)
                    (esc-k #t)))))
             profile-info)
            #f))
        
        (define profile-info-text (new profile-text% (tab this)))
        (define/public (get-profile-info-text) profile-info-text)
        
        (define/augment (clear-annotations)
          (inner (void) clear-annotations)
          (reset-profile))
        
        (super-new)))
    
    ;; profile-unit-frame-mixin : mixin
    ;; adds profiling to the unit frame
    (define profile-unit-frame-mixin
      (mixin (drscheme:unit:frame<%> drscheme:frame:<%>) ()
        
        (inherit get-interactions-text get-current-tab)
        
        ;; update-shown : -> void
        ;; updates the state of the profile item's show menu
        (define/override (update-shown)
          (super update-shown)
          (send show-profile-menu-item set-label
                (if profile-info-visible?
                    (string-constant profiling-hide-profile)
                    (string-constant profiling-show-profile))))
        
        ;; add-show-menu-items : menu -> void
        ;; adds the show profile menu item
        (define/override (add-show-menu-items show-menu)
          (super add-show-menu-items show-menu)
          (set! show-profile-menu-item 
                (instantiate menu:can-restore-menu-item% ()
                  (label (string-constant profiling-hide-profile))
                  (parent show-menu)
                  (callback
                   (λ (x y)
                     (show-profile-menu-callback))))))
        
        (define show-profile-menu-item #f)
        (define profile-gui-constructed? #f)
        
        ;; get-profile-info-visible? : -> boolean
        ;; returns #t when the profiling information is visible in the frame.
        (define/public (get-profile-info-visible?) profile-info-visible?)
        
        (field [profile-info-outer-panel #f])
        (define/override (make-root-area-container % parent)
          (set! profile-info-outer-panel
                (super make-root-area-container
                       vertical-panel%
                       parent))
          (make-object % profile-info-outer-panel))
        
        (define/private (show-profile-menu-callback)
          (cond
            [profile-info-visible?
             (send (get-current-tab) hide-profile)]
            [(send (get-current-tab) can-show-profile?)
             (send (get-current-tab) show-profile)
             (send (get-current-tab) refresh-profile)]
            [else
             (message-box (string-constant drscheme)
                          (string-constant profiling-no-information-available))]))
        
        (define/public (hide-profile-gui)
          (when profile-gui-constructed?
            (when profile-info-visible?
              (send profile-info-outer-panel change-children
                    (λ (l)
                      (remq profile-info-panel l)))
              (set! profile-info-visible? #f)
              (update-shown))))
        
        (define/public (show-profile-gui)
          (unless profile-info-visible?
            (construct-profile-gui)
            (send profile-info-outer-panel change-children
                  (λ (l)
                    (append (remq profile-info-panel l)
                            (list profile-info-panel))))
            (set! profile-info-visible? #t)
            (send profile-info-editor-canvas set-editor (send (get-current-tab) get-profile-info-text))
            (send (get-current-tab) refresh-profile)
            (update-shown)))
        
        (field (profile-info-visible? #f))
        
        (define/augment (on-tab-change from-tab to-tab)
          (inner (void) on-tab-change from-tab to-tab)
          (cond
            [(and (not profile-info-visible?)
                  (send to-tab get-profile-info-visible?))
             (show-profile-gui)]
            [(and profile-info-visible?
                  (not (send to-tab get-profile-info-visible?)))
             (hide-profile-gui)])
          (when profile-choice
            (send profile-choice set-selection
                  (profile-mode->selection
                   (send to-tab get-sort-mode))))
          (when profile-info-editor-canvas
            (send profile-info-editor-canvas set-editor 
                  (and (send to-tab can-show-profile?)
                       (send to-tab get-profile-info-text)))))
        
        (super-new)
        
        (define profile-info-panel #f)
        (define profile-info-editor-canvas #f)
        (define profile-choice #f)
        
        (inherit begin-container-sequence end-container-sequence)
        (define/private (construct-profile-gui)
          (unless profile-gui-constructed?
            (set! profile-gui-constructed? #t)
            (begin-container-sequence)
            (let ()
              (define _2
                (set! profile-info-panel (instantiate horizontal-panel% ()
                                           (parent profile-info-outer-panel)
                                           (stretchable-height #f))))
              (define profile-left-side (instantiate vertical-panel% (profile-info-panel)))
              (define _3
                (set! profile-info-editor-canvas (new canvas:basic% 
                                                      (parent profile-info-panel)
                                                      (editor (send (get-current-tab) get-profile-info-text)))))
              (define profile-message (instantiate message% ()
                                        (label (string-constant profiling))
                                        (parent profile-left-side)))
              (define _4
                (set! profile-choice (instantiate radio-box% ()
                                       (label #f)
                                       (parent profile-left-side)
                                       (callback
                                        (λ (x y)
                                          (let ([mode (profile-selection->mode (send profile-choice get-selection))])
                                            (preferences:set 'drscheme:profile-how-to-count mode)
                                            (send (get-current-tab) set-sort-mode mode)
                                            (send (get-current-tab) refresh-profile))))
                                       (choices (list (string-constant profiling-time)
                                                      (string-constant profiling-number))))))
              (define _1
                (send profile-choice set-selection
                      (case (preferences:get 'drscheme:profile-how-to-count)
                        [(time) 0]
                        [(count) 1])))
              (define update-profile-button
                (instantiate button% ()
                  (label (string-constant profiling-update))
                  (parent profile-left-side)
                  (callback
                   (λ (x y)
                     (send (get-current-tab) refresh-profile)))))
              (define hide-profile-button 
                (instantiate button% ()
                  (label (string-constant profiling-hide-profile))
                  (parent profile-left-side)
                  (callback
                   (λ (x y)
                     (send (get-current-tab) hide-profile)))))
              (send profile-choice set-selection 
                    (profile-mode->selection (preferences:get 'drscheme:profile-how-to-count)))
              
              (send profile-left-side stretchable-width #f)
              
              (let ([wid (max (send update-profile-button get-width)
                              (send hide-profile-button get-width)
                              (send profile-choice get-width)
                              (send profile-message get-width))])
                (send update-profile-button min-width wid)
                (send hide-profile-button min-width wid)
                (send profile-choice min-width wid))
              (send profile-left-side set-alignment 'left 'center)
              
              ;; hide profiling info initially, but reflow the container
              ;; so that the invisible children get the right size.
              (send this reflow-container)
              (send profile-info-outer-panel change-children
                    (λ (l)
                      (remq profile-info-panel l))))
            (end-container-sequence)))))
    
    (define (profile-selection->mode sel)
      (case sel
        [(0) 'time]
        [(1) 'count]))
    
    (define (profile-mode->selection mode)
      (case mode
        [(time) 0]
        [(count) 1]))
    
    ;; profile-text% : extends text:basic%
    ;; this class keeps track of a single thread's
    ;; profiling information. these methods are not
    ;; to be called directly, but only by the frame class, since
    ;; they do not completely implement the abstraction for the
    ;; GUI. They only manage the profiling information reported
    ;; in the bottom window
    (define profile-text% 
      (class text:basic%
        (init-field tab)
        
        ;; clear-profile-display : -> void
        ;; clears out the GUI showing the profile results
        (define/public (clear-profile-display)
          (begin-edit-sequence)
          (let ([locked? (is-locked?)])
            (lock #f)
            (clear-old-results)
            (erase)
            (lock locked?)
            (end-edit-sequence)))
        
        (inherit lock is-locked?
                 get-canvas hide-caret get-snip-location
                 begin-edit-sequence end-edit-sequence 
                 erase insert)
        
        ;; clear-old-results : -> void
        ;; removes the profile highlighting
        (field [clear-old-results void])
        
        ;; refresh-profile : (listof hashtable[...]) -> void
        ;; does the work to erase any existing profile info
        ;; and make new profiling info.
        (define/public (refresh-profile profile-info)
          (begin-edit-sequence)
          (lock #f)
          (erase)
          (clear-old-results)
          (let* (;; must copy them here in case the program is still running
                 ;; and thus updating them.
                 [infos '()]
                 [_ (let loop ([profile-info profile-info])
                      (cond
                        [(null? profile-info) (void)]
                        [else 
                         (let ([ht (car profile-info)])
                           (hash-table-for-each
                            ht
                            (λ (key val)
                              (when (any-info? val)
                                (set! infos (cons (copy-prof-info val) infos))))))
                         (loop (cdr profile-info))]))]
                 
                 ;; each editor that gets some highlighting is put
                 ;; into this table and an edit sequence is begun for it.
                 ;; after all ranges are updated, the edit sequences are all closed.
                 [in-edit-sequence (make-hash-table)]
                 [clear-highlight void]
                 [max-value (extract-maximum infos)]
                 [total-time (extract-total-time infos)]
                 [show-highlight
                  (λ (info)
                    (let* ([expr (prof-info-expr info)]
                           [src (syntax-source expr)]
                           [pos (syntax-position expr)]
                           [span (syntax-span expr)])
                      (when (and src
                                 (is-a? src text:basic<%>)
                                 (number? pos)
                                 (number? span))
                        (unless (hash-table-get in-edit-sequence src (λ () #f))
                          (hash-table-put! in-edit-sequence src #t)
                          (send src begin-edit-sequence))
                        (let* ([color (get-color-value 
                                       (if (eq? (preferences:get 'drscheme:profile-how-to-count) 'time)
                                           (prof-info-time info)
                                           (prof-info-num info))
                                       max-value)]
                               [clr (send src highlight-range (- pos 1) (+ pos span -1) color)])
                          (let ([old-thnk clear-highlight])
                            (set! clear-highlight
                                  (λ ()
                                    (clr)
                                    (old-thnk))))))))]
                 [smaller-range?
                  (λ (x y)
                    (let ([x-span (syntax-span (prof-info-expr x))]
                          [y-span (syntax-span (prof-info-expr y))])
                      (if (and x-span y-span)
                          (< x-span y-span)
                          #f)))]
                 
                 [show-line
                  (λ (info newline? highlight-line?)
                    (let* ([expr (prof-info-expr info)]
                           [expr-src (syntax-source expr)]
                           [count (prof-info-num info)]
                           [time (prof-info-time info)]
                           [name (prof-info-name info)])
                      (when newline? (send src-loc-editor insert "\n"))
                      (when highlight-line? (small-blank-line src-loc-editor))
                      (let ([before (send src-loc-editor last-position)])
                        (insert-profile-src-loc src-loc-editor expr name)
                        (let ([after (send src-loc-editor last-position)])
                          (cond
                            [(string? expr-src)
                             (send src-loc-editor change-style (gui-utils:get-clickback-delta) before after)
                             (let ([after (send src-loc-editor last-position)])
                               (send src-loc-editor set-clickback 
                                     before after 
                                     (λ (text start end)
                                       (open-file-and-goto-position expr-src (syntax-position expr)))))]
                            [(is-a? expr-src editor:basic<%>)
                             (send src-loc-editor change-style (gui-utils:get-clickback-delta) before after)
                             (send src-loc-editor set-clickback
                                   before after
                                   (λ (text start end)
                                     (let ([window (send expr-src get-top-level-window)]
                                           [pos (syntax-position expr)])
                                       (when window (send window show #t))
                                       (when pos (send expr-src set-position (- pos 1)))
                                       (send expr-src set-caret-owner #f 'global))))]
                            [else (void)])))
                      
                      (when newline? (send percentage-editor insert "\n"))
                      (when highlight-line? (small-blank-line percentage-editor))
                      (send percentage-editor insert (format-percentage 
                                                      (if (= total-time 0)
                                                          1
                                                          (/ time total-time))))
                      (send percentage-editor set-paragraph-alignment
                            (send percentage-editor last-paragraph)
                            'right)
                      
                      (when newline? (send time-editor insert "\n"))
                      (when highlight-line? (small-blank-line time-editor))
                      (send time-editor insert (format "~a" time))
                      (send time-editor set-paragraph-alignment (send time-editor last-paragraph) 'right)
                      
                      (when newline? (send count-editor insert "\n")) 
                      (when highlight-line? (small-blank-line count-editor))
                      (send count-editor insert (format "~a" count))
                      (send count-editor set-paragraph-alignment (send count-editor last-paragraph) 'right)))]
                 
                 [bigger-value?
                  (λ (x y)
                    (let ([sel (if (eq? 'count (preferences:get 'drscheme:profile-how-to-count))
                                   prof-info-num
                                   prof-info-time)])
                      (> (sel x) (sel y))))]
                 
                 [cleanup-editor
                  (λ (ed)
                    (let* ([ed-admin (send ed get-admin)]
                           [snip (send ed-admin get-snip)]
                           [bl (box 0)]
                           [br (box 0)])
                      (get-snip-location snip bl #f #f)
                      (get-snip-location snip br #f #t)
                      (let ([w (+ (- (unbox br) (unbox bl)) 4)])
                        (send ed set-max-width w)
                        (send ed set-min-width w)))
                    (send ed hide-caret #t)
                    (send ed lock #t))]
                 
                 [top-infos (top 100 (sort infos bigger-value?))])
            (for-each show-highlight top-infos)
            (initialize-editors)
            (let loop ([infos top-infos]
                       [newline? #f]
                       [highlight-counter 0])
              (cond
                [(null? infos) (void)]
                [else 
                 (show-line (car infos) newline? (and newline? (zero? highlight-counter)))
                 (loop (cdr infos) #t (modulo (+ highlight-counter 1) 2))]))
            (cleanup-editor count-editor)
            (cleanup-editor time-editor)
            (cleanup-editor percentage-editor)
            (cleanup-editor src-loc-editor)
            
            (hash-table-for-each
             in-edit-sequence
             (λ (key val)
               (send key end-edit-sequence)))
            (set! clear-old-results 
                  (λ ()
                    (hash-table-for-each
                     in-edit-sequence
                     (λ (key val) (send key begin-edit-sequence)))
                    (clear-highlight)
                    (hash-table-for-each
                     in-edit-sequence
                     (λ (key val) (send key end-edit-sequence)))
                    (set! clear-old-results void))))
          (lock #t)
          (end-edit-sequence)
          (let ([canvas (get-canvas)])
            (when canvas
              (send canvas scroll-to 0 0 1 1 #t 'start))))
        
        ;; top : number (listof X) -> (listof X)
        ;; extracts the first `n' elements from a list.
        (define/private (top n lst)
          (let loop ([n n]
                     [lst lst])
            (cond
              [(null? lst) null]
              [(= 0 n) null]
              [else (cons (car lst) (loop (- n 1) (cdr lst)))])))
        
        (field (src-loc-editor #f)
               (percentage-editor #f)
               (time-editor #f)
               (count-editor #f))
        (define/private (clear-editors)
          (set! src-loc-editor #f)
          (set! percentage-editor #f)
          (set! time-editor #f)
          (set! count-editor #f))
        (define/private (initialize-editors)
          (set! src-loc-editor (instantiate text% ()))
          (set! percentage-editor (instantiate text% ()))
          (set! time-editor (instantiate text% ()))
          (set! count-editor (instantiate text% ()))
          (send src-loc-editor set-styles-sticky #f)            
          (send percentage-editor set-styles-sticky #f)
          (send time-editor set-styles-sticky #f)
          (send count-editor set-styles-sticky #f)
          (insert (instantiate editor-snip% (percentage-editor)))
          (insert (instantiate editor-snip% (time-editor)))
          (insert (instantiate editor-snip% (count-editor)))
          (insert (instantiate editor-snip% (src-loc-editor)))
          (insert-title (string-constant profiling-col-function) src-loc-editor)
          (insert-title (string-constant profiling-col-time-in-msec) time-editor)
          (insert-title (string-constant profiling-col-percent-time) percentage-editor)
          (insert-title (string-constant profiling-col-calls) count-editor))
        
        (define/private (insert-title str txt)
          (send txt insert str)
          (send txt insert "\n")
          (send txt change-style bold-delta 0 (- (send txt last-position) 1))
          (send txt set-paragraph-alignment 0 'center))
        
        (super-new)
        (hide-caret #t)))
    
    ;; format-percentage : number[0 <= n <= 1] -> string
    ;; formats the number as a percentage string with trailing zeros,
    ;; to 3 decimal places.
    (define (format-percentage n)
      (let* ([number-of-places 3]
             [whole-part (floor (* n 100))]
             [decimal-part (- (* n 100) whole-part)]
             [truncated/moved-decimal-part (floor (* (expt 10 number-of-places) decimal-part))]
             [pad
              (λ (str)
                (if ((string-length str) . < . number-of-places)
                    (string-append (make-string (- number-of-places (string-length str)) #\0) 
                                   str)
                    str))])
        (string-append (format "~a" whole-part)
                       "."
                       (pad (format "~a" truncated/moved-decimal-part)))))
    
    (define (small-blank-line txt)
      (let ([before (send txt last-position)])
        (send txt insert "\n")
        (let ([after (send txt last-position)])
          (send txt change-style small-font-style before after))))
    
    (define small-font-style (make-object style-delta% 'change-size 6))
    
    ;; bold-delta : style-delta
    (define bold-delta (make-object style-delta% 'change-bold))
    
    ;; insert-profile-src-loc : syntax name -> string
    (define (insert-profile-src-loc editor stx name)
      (cond
        [name
         (let ([before (send editor last-position)])
           (send editor insert (format "~a" name)))]
        [else
         (let* ([src (syntax-source stx)]
                [filename 
                 (cond
                   [(string? src) src]
                   [(is-a? src editor<%>) (get-filename-from-editor src)]
                   [else (string-constant profiling-unknown-src)])]
                [col (syntax-column stx)]
                [line (syntax-line stx)]
                [pos (syntax-position stx)]
                [span (syntax-span stx)]
                [src
                 (cond
                   [(and col line)
                    (format "~a: ~a.~a" filename line col)]
                   [pos
                    (format "~a: ~a" filename pos)]
                   [else 
                    filename])])
           (send editor insert src))]))
    
    ;; open-file-and-goto-position : string (union #f number) -> void
    (define (open-file-and-goto-position filename pos)
      (let ([frame (handler:edit-file filename)])
        (when (and frame
                   pos
                   (is-a? frame drscheme:unit:frame%))
          (let ([defs (send frame get-definitions-text)])
            (send defs set-position (- pos 1))))))
    
    ;; get-src-filename : tst -> (union #f string)
    (define (get-src-filename src)
      (cond
        [(string? src) src]
        [(is-a? src text%)
         (send src get-filename)]
        [else #f]))
    
    ;; get-src-loc : syntax -> string
    (define (get-src-loc expr)
      (cond
        [(and (number? (syntax-line expr))
              (number? (syntax-column expr))
              (number? (syntax-span expr)))
         (format " ~a.~a [~a]" 
                 (syntax-line expr) 
                 (syntax-column expr)
                 (syntax-span expr))]
        [(and (number? (syntax-position expr))
              (number? (syntax-span expr)))
         (format " ~a-~a" 
                 (syntax-position expr)
                 (syntax-span expr))]
        [else ""]))
    
    (define (add-prefs-panel)
      (preferences:add-panel
       (string-constant profiling)
       (λ (s-parent)
         (letrec ([parent (make-object vertical-panel% s-parent)]
                  [msg (make-object message% 
                         (string-constant profiling-color-config) 
                         parent)]
                  [hp (make-object horizontal-pane% parent)]
                  [low (make-object button% (string-constant profiling-low-color) hp 
                         (λ (x y) (color-callback #t)))]
                  [color-bar (make-object color-bar% hp)]
                  [high (make-object button% (string-constant profiling-high-color) hp
                          (λ (x y) (color-callback #f)))]
                  
                  [scale (instantiate radio-box% ()
                           (label (string-constant profiling-scale))
                           (parent parent)
                           (callback (λ (x y) (scale-callback)))
                           (choices
                            (list (string-constant profiling-sqrt)
                                  (string-constant profiling-linear)
                                  (string-constant profiling-square))))]
                  
                  [color-callback
                   (λ (low?)
                     (let ([color (get-color-from-user 
                                   (if low?
                                       (string-constant profiling-choose-low-color)
                                       (string-constant profiling-choose-high-color))
                                   #f
                                   (preferences:get
                                    (if low?
                                        'drscheme:profile:low-color
                                        'drscheme:profile:high-color)))])
                       (when color
                         (preferences:set 
                          (if low? 'drscheme:profile:low-color 'drscheme:profile:high-color)
                          color))))]
                  [scale-callback
                   (λ ()
                     (preferences:set 
                      'drscheme:profile:scale
                      (case (send scale get-selection)
                        [(0) 'sqrt]
                        [(1) 'linear]
                        [(2) 'square])))])
           (preferences:add-callback
            'drscheme:profile:scale
            (λ (p v)
              (send scale set-selection
                    (case v
                      [(sqrt) 0]
                      [(linear) 1]
                      [(square) 2]))))
           (send parent set-alignment 'left 'center)
           (send hp stretchable-height #f)
           parent))))
    
    (define color-bar%
      (class canvas%
        (inherit get-client-size get-dc)
        (field [pen (make-object pen% "black" 1 'solid)]
               [in-on-paint? #f])
        (define/override (on-paint)
          (set! in-on-paint? #t)
          (let* ([dc (get-dc)]
                 [dummy-pen (send dc get-pen)]
                 [drscheme:profile:low-color (preferences:get 'drscheme:profile:low-color)]
                 [drscheme:profile:high-color (preferences:get 'drscheme:profile:high-color)]
                 [drscheme:profile:scale (preferences:get 'drscheme:profile:scale)])
            (let-values ([(w h) (get-client-size)])
              (let loop ([n 0])
                (when (n . <= . w)
                  (send pen set-color 
                        (get-color-value/pref n w drscheme:profile:low-color drscheme:profile:high-color drscheme:profile:scale))
                  (send dc set-pen pen)
                  (send dc draw-line n 0 n h)
                  (send dc set-pen dummy-pen)
                  (loop (+ n 1))))
              (let-values ([(tw th ta td) (send dc get-text-extent 
                                                (string-constant profiling-example-text))])
                (send dc draw-text
                      (string-constant profiling-example-text)
                      (floor (- (/ w 2) (/ tw 2)))
                      (floor (- (/ h 2) (/ th 2)))))))
          (set! in-on-paint? #f))
        
        ;; queue callbacks here so that the preferences
        ;; values are actually set by the time on-paint
        ;; is called.
        (preferences:add-callback
         'drscheme:profile:scale
         (λ (p v)
           (unless in-on-paint?
             (queue-callback
              (λ ()
                (on-paint))))))
        (preferences:add-callback
         'drscheme:profile:low-color
         (λ (p v)
           (unless in-on-paint?
             (queue-callback
              (λ ()
                (on-paint))))))
        (preferences:add-callback
         'drscheme:profile:high-color
         (λ (p v)
           (unless in-on-paint?
             (queue-callback
              (λ ()
                (on-paint))))))
        
        (super-instantiate ())))
    
    
    
    ;                                
    ;                                
    ;;;   ; ;;;  ; ;;;   ;;;   ; ;;;  ;;;;;  ; ;;;  ;;;;    ;;;    ;;;  
    ;   ;   ;      ;     ;   ;   ;      ;      ;         ;  ;   ;  ;   ; 
    ;;;;;   ;      ;     ;   ;   ;      ;      ;      ;;;;  ;      ;;;;; 
    ;       ;      ;     ;   ;   ;      ;      ;     ;   ;  ;      ;     
    ;   ;   ;      ;     ;   ;   ;      ;   ;  ;     ;   ;  ;   ;  ;   ; 
    ;;;   ;;;;   ;;;;    ;;;   ;;;;     ;;;  ;;;;    ;;; ;  ;;;    ;;;  
    
    
    (define-values/invoke-unit/infer stacktrace@)))
