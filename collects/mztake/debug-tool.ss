(module debug-tool mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "string.ss")
           ;(lib "math.ss")
           (lib "class.ss")
           (lib "unitsig.ss")
           ;(lib "contract.ss")
           (lib "mred.ss" "mred")
           (prefix drscheme:arrow: (lib "arrow.ss" "drscheme"))
           (lib "tool.ss" "drscheme")
           "marks.ss"
           (lib "boundmap.ss" "syntax")
           (lib "bitmap-label.ss" "mrlib")
           "annotator.ss"
           "load-sandbox.ss"
           ;(lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           )
  
  (provide tool@)
  
  ; QUESTIONS/IDEAS
  ; what is the right way to deal with macros?
  ; how can the three tool classes communicate with each other safely

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define phase1 void)
      (define phase2 void)

      (define (extract-language-level settings)
	(let* ([language (drscheme:language-configuration:language-settings-language settings)])
	  (car (last-pair (send language get-language-position)))))

      (define (debugger-does-not-work-for? lang)
	(member lang (list (string-constant beginning-student)
			   (string-constant beginning-student/abbrev)
			   (string-constant intermediate-student)
			   (string-constant intermediate-student/lambda)
			   (string-constant advanced-student))))
      
      (define (break-at bp p)
        (hash-table-get bp p))
      
      (define (truncate str n)
        (if (< (string-length str) n)
            str
            (if (>= n 3)
                (string-append
                 (substring str 0 (- n 3))
                 "...")
                (substring str 0 (min n (string-length str))))))
      
      (define (clean-status s)
	(truncate (regexp-replace* #rx"\n" s " ") 200))

      (define (string-map! f str)
        (let loop ([i 0])
          (when (< i (string-length str))
            (string-set! str i (f (string-ref str i)))
            (loop (add1 i)))
          str))
      
      (define (newlines->spaces str)
        (string-map! (lambda (chr)
                       (case chr
                         [(#\newline) #\space]
                         [else chr]))
                     str))
      
      (define (index-of chr str)
        (let loop ([i 0])
          (if (< i (string-length str))
              (if (char=? chr (string-ref str i))
                  i
                  (loop (add1 i)))
              #f)))
          
      (define (trim-expr-str str)
        (cond
          [(index-of #\newline str) => (lambda (i)
                                         (string-append
                                          (substring str 0 i)
                                          (if (char=? (string-ref str 0) #\()
                                              " ...)"
                                              " ...")))]
          [str]))
      
      (define (average . values)
        (/ (apply + values) (length values)))
      
      (define (debug-definitions-text-mixin super%)
        (class super%
          
          (inherit dc-location-to-editor-location
                   editor-location-to-dc-location
                   invalidate-bitmap-cache
                   begin-edit-sequence
                   end-edit-sequence
                   get-canvas
                   get-top-level-window)
          
          (define parent #f)
          (define debug? #f)
          (define/public (set-parent! p)
            (set! parent p)
            (set! debug? (send parent debug?)))
          (define mouse-over-pos #f)
          (define bp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define bp-brush (send the-brush-list find-or-create-brush "red" 'solid))
          (define bp-mo-pen (send the-pen-list find-or-create-pen "darkgray" 1 'solid))
          (define bp-mo-brush (send the-brush-list find-or-create-brush "pink"
                                    'solid))
          (define bp-tmp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define bp-tmp-brush (send the-brush-list find-or-create-brush "yellow"
                                    'solid))
          (define pc-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define pc-brush (send the-brush-list find-or-create-brush "forestgreen" 'solid))
          (define pc-err-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define pc-err-brush (send the-brush-list find-or-create-brush "red" 'solid))
          (define pc-brk-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define pc-brk-brush (send the-brush-list find-or-create-brush "gray" 'solid))
          
          (super-instantiate ())
          
          (define/augment (on-delete start len)
            (begin-edit-sequence)
            (inner (void) on-delete start len))
          (define/augment (after-delete start len)
            (inner (void) after-delete start len)
            (clean-up)
            (end-edit-sequence))
          
          (define/augment (on-insert start len)
            (begin-edit-sequence)
            (inner (void) on-insert start len))
          (define/augment (after-insert start len)
            (inner (void) after-insert start len)
            (clean-up)
            (end-edit-sequence))

          (define/private (clean-up)
	    (when debug?
	      (set! debug? #f)
	      (when parent
		    (send parent hide-debug))
	      (invalidate-bitmap-cache)))
          
          (define/private (get-pos/text event)
            (let ([event-x (send event get-x)]
                  [event-y (send event get-y)]
                  [on-it? (box #f)])
              (let loop ([editor this])
                (let-values ([(x y) (send editor dc-location-to-editor-location
                                          event-x event-y)])
                  (cond
                    [(is-a? editor text%)
                     (let ([pos (send editor find-position x y #f on-it?)])
                       (cond
                         [(not (unbox on-it?)) (values #f #f)]
                         [else
                          (let ([snip (send editor find-snip pos 'after-or-none)])
                            (if (and snip
                                     (is-a? snip editor-snip%))
                                (loop (send snip get-editor))
                                (values pos editor)))]))]
                    [(is-a? editor pasteboard%)
                     (let ([snip (send editor find-snip x y)])
                       (if (and snip
                                (is-a? snip editor-snip%))
                           (loop (send snip get-editor))
                           (values #f #f)))]
                    [else (values #f #f)])))))
          
          (define/private (find-char-box text left-pos right-pos)
            (let ([xlb (box 0)]
                  [ylb (box 0)]
                  [xrb (box 0)]
                  [yrb (box 0)])
              (send text position-location left-pos xlb ylb #t)
              (send text position-location right-pos xrb yrb #f)
              (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location
                                                   (unbox xlb) (unbox ylb))]
                            [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                            [(xr-off yr-off) (send text editor-location-to-dc-location
                                                   (unbox xrb) (unbox yrb))]
                            [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                (values xl yl xr yr))))

	  (define/private (render v)
	    (if parent
		(send parent render v)
		(printf "~e" v)))
          
          (define/override (on-event event)
            (if (and parent debug?)
                (let ([breakpoints (send parent get-breakpoints)])
                  (cond
                    [(send event leaving?)
                     (when mouse-over-pos
                       (set! mouse-over-pos #f)
                       (invalidate-bitmap-cache))
		     (super on-event event)]
                    [(or (send event moving?)
                         (send event entering?))
                     (let-values ([(pos text) (get-pos/text event)])
                       (when (and pos text)
                         (let ([pos (add1 pos)])
                           (cond
                             ; mouse on breakable pos and hasn't moved significantly
                             [(eq? pos mouse-over-pos)]
                             ; mouse on new breakable pos
                             [(not (eq? (hash-table-get
                                       breakpoints
                                       pos (lambda () 'invalid)) 'invalid))
                              (set! mouse-over-pos pos)
                              (invalidate-bitmap-cache)]
                             ; moved off breakable pos
                             [mouse-over-pos
                              (set! mouse-over-pos #f)
                              (invalidate-bitmap-cache)])
                           (let* ([frames (send parent get-stack-frames)]
                                  [pos-vec (send parent get-pos-vec)]
                                  [id (vector-ref pos-vec pos)]
                                #;
                                  [_ (printf "frames = ~a~npos-vec = ~a~nid = ~a~n"
                                             frames pos-vec id)])
                             (send parent
                                   set-mouse-over-msg
                                   (cond
                                     [(and id frames
                                           (let/ec k
                                             (let* ([id-sym (syntax-e id)]
                                                    [binding (lookup-first-binding
                                                              (lambda (id2) (module-identifier=? id id2))
                                                              frames (lambda ()
                                                                       (k #f
                                                                        #;
                                                                          (format "~a = ~a" id-sym
                                                                                (namespace-variable-value
                                                                                 id-sym
                                                                                 #f
                                                                                 (lambda () (k #f))
                                                                                 (send
                                                                                  (send parent get-interactions-text)
                                                                                  get-user-namespace))))))]
                                                    [val (mark-binding-value
                                                          binding)])
                                               (clean-status (format "~a = ~a" id-sym (render val))))))]
                                     [""]))))))
		     (super on-event event)]
                    [(send event button-down? 'right)
                     (let-values ([(pos text) (get-pos/text event)])
                       (if (and pos text)
                           (let* ([pos (add1 pos)]
                                  [break-status (hash-table-get breakpoints pos (lambda () 'invalid))])
                             (case break-status
                               [(#t #f)
                                (let ([menu (make-object popup-menu% #f)])
                                  (make-object menu-item%
                                    (if break-status
					"Remove pause at this point"
					"Pause at this point")
                                    menu
                                    (lambda (item evt)
                                      (hash-table-put! breakpoints pos (not break-status))
                                      (invalidate-bitmap-cache)))
                                  (let ([pc (send parent get-pc)])
                                    (if (and pc (= pos pc))
                                        (let ([stat (send parent get-break-status)]
                                              [f (get-top-level-window)])
                                          (when (cons? stat)
                                            (send (make-object menu-item%
                                                    (clean-status
                                                     (if (= 2 (length stat))
                                                         (format "value = ~a" (render (cadr stat)))
                                                         (format "~a" (cons 'values 
									    (map (lambda (v) (render v)) (rest stat))))))
                                                    menu
                                                    void) enable #f))
                                          (when (not (eq? stat 'break))
                                            (make-object menu-item%
                                              (if (cons? stat)
                                                  "Change return value..."
                                                  "Skip expression...")
                                              menu
                                              (lambda (item evt)
                                                (let ([tmp (get-text-from-user "Return value" #f)])
                                                  (when tmp
                                                    (let/ec k
                                                      (send parent set-break-status
                                                            (cons 'exit-break
                                                                  (call-with-values
                                                                      (lambda ()
                                                                        (with-handlers ([exn:fail? k]) ; LATER: message box
                                                                          (eval-string tmp)))
                                                                    list))))))))))
                                        (make-object menu-item%
                                          "Continue to this point"
                                          menu
                                          (lambda (item evt)
                                            (hash-table-put!
                                             breakpoints pos
                                             (lambda () (hash-table-put! breakpoints pos #f) #t))
                                            (invalidate-bitmap-cache)
                                            (when (send parent get-stack-frames)
                                              (send parent resume))))))
                                  (send (get-canvas) popup-menu menu
                                        (+ 1 (inexact->exact (floor (send event get-x))))
                                        (+ 1 (inexact->exact (floor (send event get-y))))))]
                               [(invalid)
                                (let* ([frames (send parent get-stack-frames)]
                                       [pos-vec (send parent get-pos-vec)]
                                       [id (vector-ref pos-vec pos)]
                                       #;
                                       [_ (printf "frames = ~a~npos-vec = ~a~nid = ~a~n"
                                                    frames pos-vec id)])
                                  (unless (and
                                           id frames
                                           (let/ec k
                                             (let* ([id-sym (syntax-e id)]
                                                    [binding (lookup-first-binding
                                                              (lambda (id2) (module-identifier=? id id2))
                                                              frames (lambda () (k #f)))]
                                                        [val (mark-binding-value
                                                              binding)]
                                                        [menu (make-object popup-menu% #f)])
                                               (send (make-object menu-item%
                                                       (clean-status
                                                        (format "~a = ~a" id-sym val))
                                                       menu
                                                       (lambda (item evt)
                                                         (printf "~a" val))) enable #f)
                                               (make-object menu-item%
                                                 (format "(set! ~a ...)" id-sym)
                                                 menu
                                                 (lambda (item evt)
                                                   (let ([tmp
                                                          (get-text-from-user
                                                               (format "New value for ~a" id-sym) #f #f
                                                               (format "~a" val))])
                                                     (when tmp
                                                       (mark-binding-set! binding (eval-string tmp))))))
                                               (send (get-canvas) popup-menu menu
                                                     (+ 1 (inexact->exact (floor (send event get-x))))
                                                     (+ 1 (inexact->exact (floor (send event get-y)))))
                                               #t)))
                                    (super on-event event)))]))
                           (super on-event event)))]
                    [else (super on-event event)]))
                (super on-event event)))
          
          (define/override (on-paint before dc left top right bottom dx dy draw-caret)
            (super on-paint before dc left top right bottom dx dy draw-caret)
            (when (and parent debug? (not before))
              (let ([breakpoints (send parent get-breakpoints)])
                (hash-table-for-each
                 breakpoints
                 (lambda (pos enabled?)
                   (when (and (>= pos 0) (or enabled? (and mouse-over-pos (= mouse-over-pos pos))))
                     (let*-values ([(xl yl xr yr) (find-char-box this (sub1 pos) pos)]
                                   [(diameter) (- xr xl)]
                                   [(yoff) (/ (- yr yl diameter) 2)])
                       (let ([op (send dc get-pen)]
                             [ob (send dc get-brush)])
                         (case enabled?
                           [(#t) (send dc set-pen bp-pen)
                                 (send dc set-brush bp-brush)]
                           [(#f) (send dc set-pen bp-mo-pen)
                                 (send dc set-brush bp-mo-brush)]
                           [else (send dc set-pen bp-tmp-pen)
                                   (send dc set-brush bp-tmp-brush)])
                         ;(drscheme:arrow:draw-arrow dc xl yl xr yr dx dy)
                         (send dc draw-ellipse (+ xl dx) (+ yl dy yoff) diameter diameter)
                         #;
                         (send dc draw-polygon stop-sign
                                 (+ xl dx)
                                 (+ yl dy 2))
                         (send dc set-pen op)
                         (send dc set-brush ob)))))))
              (let ([pos (send parent get-pc)])
                (when pos
                  (let*-values ([(xl yl xr yr) (find-char-box this (sub1 pos) pos)]
                                [(ym) (average yl yr)])
                    (let ([op (send dc get-pen)]
                          [ob (send dc get-brush)])
                      (case (send parent get-break-status)
                        [(error) (send dc set-pen pc-err-pen)
                                 (send dc set-brush pc-err-brush)]
                        [(break) (send dc set-pen pc-brk-pen)
                                 (send dc set-brush pc-brk-brush)]
                        [else    (send dc set-pen pc-pen)
                                 (send dc set-brush pc-brush)]))
                    (drscheme:arrow:draw-arrow dc xl ym xr ym dx dy))
                  #;
                  (let loop ([end-pos pos]
                             [marks (send parent get-stack-frames)])
                    (when (cons? marks)
                      (let*-values ([(start-pos) (syntax-position (mark-source (first marks)))]
                                    [(xl0 yl0 xr0 yr0) (find-char-box this (sub1 start-pos) start-pos)]
                                    [(xm0) (average xl0 xr0)]
                                    [(ym0) (average yl0 yr0)]
                                    [(xl yl xr yr) (find-char-box this (sub1 end-pos) end-pos)]
                                    [(xm) (average xl xr)]
                                    [(ym) (average yl yr)])
                        (let ([op (send dc get-pen)]
                              [ob (send dc get-brush)])
                          (case (send parent get-break-status)
                            [(error) (send dc set-pen pc-err-pen)
                                     (send dc set-brush pc-err-brush)]
                            [(break) (send dc set-pen pc-brk-pen)
                                     (send dc set-brush pc-brk-brush)]
                            [else    (send dc set-pen pc-pen)
                                     (send dc set-brush pc-brush)]))
                        (drscheme:arrow:draw-arrow dc xm0 ym0 xr ym dx dy)
                        (loop start-pos (rest marks)))))))))

	  (define/augment (after-set-next-settings s)
	    (send (get-top-level-window) check-current-language-for-debugger)
	    (inner (void) after-set-next-settings s))))
      
      (define (debug-interactions-text-mixin super%)
        (class super%
          
          (inherit run-in-evaluation-thread
                   display-results)
          
          (define parent #f)
          (define/public (set-parent! p)
            (set! parent p))
          
          (super-instantiate ())
          
          ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
          ;; adds debugging information to `sexp' and calls `oe'
          (define/private (make-debug-eval-handler oe break? break-before break-after)
            (lambda (orig-exp)
              (if (or (compiled-expression? (if (syntax? orig-exp)
                                                (syntax-e orig-exp)
                                                orig-exp))
                      (not parent)
                      (not (syntax-source orig-exp))
                      (not (eq? (syntax-source orig-exp)
                                (send parent get-definitions-text))))
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
                             [(null? exprs) (apply values last-one)]
                             [else (i-loop (cdr exprs)
                                           (call-with-values
                                            (lambda () (loop (car exprs)))
                                            list))]))]
                        [_else
                         ;; Not `begin', so proceed with normal expand and eval 
                         (parameterize ([current-eval oe])
                           (eval/annotations
                            top-e
                            (lambda (fn m) #f) ; TODO: multiple file support
                            (lambda (stx)
                              (let*-values ([(breakpoints) (send parent get-breakpoints)]
                                            [(pos-vec) (send parent get-pos-vec)]
                                            [(annotated break-posns)
                                             (annotate-for-single-stepping
                                              (expand-syntax top-e)
                                              break?
                                              break-before
                                              break-after
                                              (lambda (type bound binding)
                                                ;(display-results (list bound))
                                                (let loop ([i 0])
                                                  (when (< i (syntax-span bound))
                                                    (vector-set! pos-vec (+ i (syntax-position bound)) binding)
                                                    (loop (add1 i)))))
					      void)])
                                (for-each (lambda (posn) (hash-table-put! breakpoints posn #f)) break-posns)
                                ;(display-results (list orig-exp))
                                annotated))))]))))))
          
          (define/override (reset-console)
            (super reset-console)
            (when (and parent (send parent debug?))
              (let ([breakpoints (send parent get-breakpoints)])
                (run-in-evaluation-thread
                 (lambda ()
                   ;(print-struct #t)
                   (let ([self (current-thread)]
                         [oeh (current-exception-handler)]
                         [err-hndlr (error-display-handler)])
                     (error-display-handler
                      (lambda (msg exn)
                        (err-hndlr msg exn)
                        (if (and (eq? self (current-thread)) (exn:fail? exn))
                            (send parent suspend oeh
                                  (continuation-mark-set->list (exn-continuation-marks exn) debug-key)
                                  'error)))) ; this breaks the buttons because it looks like we can resume
                     (current-eval
                      (make-debug-eval-handler
                       (current-eval)
                       (lambda (pos)
                         (or (hash-table-get breakpoints -1)
                             (let ([bp (hash-table-get breakpoints pos)])
                               (if (procedure? bp)
                                   (bp)
                                   bp))))
                       ; break-before
                       (lambda (top-mark ccm)
                         (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                           (send parent suspend oeh (cons top-mark debug-marks) 'entry-break)))
                       ; break-after
                       (case-lambda
                         [(top-mark ccm val)
                          (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                            (car (send parent suspend oeh (cons top-mark debug-marks) (list 'exit-break val))))]
                         [(top-mark ccm . vals) 
                          (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                            (apply values
				   (send parent suspend oeh (cons top-mark debug-marks) (cons 'exit-break vals))))])))
                     (current-exception-handler
                      (lambda (exn)
                        (if (and (exn:break? exn) (send parent suspend-on-break?))
                            (let ([marks (exn-continuation-marks exn)]
                                  [cont (exn:break-continuation exn)])
                              (send parent suspend oeh (continuation-mark-set->list marks debug-key) 'break)
                              (cont))
                            (oeh exn))))))))))))
      
      (define (debug-unit-frame-mixin super%)
        (class super%
          
          (inherit get-button-panel
                   get-definitions-text
                   get-interactions-text
                   get-menu-bar
                   get-current-tab
                   get-top-level-window
		   get-eventspace)
          
          (define breakpoints (make-hash-table))
          (hash-table-put! breakpoints -1 #f)
          (define suspend-sema (make-semaphore 1))
          (define resume-ch (make-channel))
          (define in-user-ch (make-channel))
          (define want-suspend-on-break? #f)
          (define want-debug? #f)
          (define/public (debug?)
            want-debug?)
          (define stack-frames #f)
	  (define current-language-settings #f)
          (define pos-vec (make-vector 1))
          (define/public (suspend-on-break?)
            want-suspend-on-break?)
          (define/public (get-stack-frames)
            stack-frames)
          (define/public (get-pos-vec)
            pos-vec)
          (define/public (get-breakpoints)
            breakpoints)
          (define break-status #f)
          (define/public (get-break-status)
            break-status)
          (define/public (set-break-status stat)
            (set! break-status stat))
          (define control-panel #f)
          (define/public (resume)
	    (let ([v break-status])
	      (resume-gui)
	      (channel-put resume-ch (if (pair? v)
					 (cdr v)
					 #f))))
          (define/public (set-mouse-over-msg msg)
            (when (not (string=? msg (send mouse-over-message get-label)))
              (send mouse-over-message set-label msg)))
          
          (define/public (get-pc)
            (if (cons? stack-frames)
                (let* ([src-stx (mark-source (first stack-frames))]
                       [start (syntax-position src-stx)]
                       [end (and start (+ start (syntax-span src-stx) -1))])
                  (if (cons? break-status)
                      end
                      start))
                #f))

	  (define/public (render v)
	    ;; ==drscheme eventspace thread==
	    ;; only when a user thread is suspended
	    (let ([result-ch (make-channel)])
	      (channel-put in-user-ch (lambda ()
					(let ([s (open-output-string)])
					  (send (drscheme:language-configuration:language-settings-language 
						 current-language-settings)
						render-value
						v
						(drscheme:language-configuration:language-settings-settings
						 current-language-settings)
						s)
					  (channel-put result-ch (get-output-string s)))))
	      (channel-get result-ch)))
          
	  (define/private (suspend-gui frames status)
	    (set! want-suspend-on-break? #f)
	    (hash-table-put! breakpoints -1 #f)
	    (send pause-button enable #f)
	    (send step-button enable #t)
	    (send resume-button enable #t)
	    ;;(fprintf (current-error-port) "break: ~a~n" (map expose-mark frames))
	    ;;(printf "status = ~a~n" status)
	    (set! stack-frames frames)
	    (set! break-status status)
	    (when (cons? status)
	      (let ([expr (mark-source (first frames))])
		(send status-message set-label
		      (clean-status
		       (format "~a ==> ~a"
			       (trim-expr-str
				(send (get-definitions-text) get-text
				      (sub1 (syntax-position expr))
				      (+ -1 (syntax-position expr) (syntax-span expr))))
			       (if (= 2 (length status))
				   (render (cadr status))
				   (cons 'values (map (lambda (v) (render v)) (rest status)))))))))
	    (cond [(get-pc) => (lambda (pc) (send (get-definitions-text) scroll-to-position pc))])
	    (send (get-definitions-text) invalidate-bitmap-cache))

	  (define/private (resume-gui)
	    (set! stack-frames #f)
	    (set! break-status #f)
	    (send pause-button enable #t)
	    (send step-button enable #f)
	    (send resume-button enable #f)
	    (send status-message set-label "")
	    (send (get-definitions-text) invalidate-bitmap-cache))

          (define/public suspend
	    ;; ==called from user thread==
            (opt-lambda (break-handler frames [status #f])
	      ;; suspend-sema ensures that we allow only one suspended thread
	      ;;  at a time
	      (if (semaphore-try-wait? suspend-sema)
		  (begin
		    (parameterize ([current-eventspace (get-eventspace)])
		      (queue-callback (lambda () (suspend-gui frames status))))
		    (with-handlers ([exn:break?
				     (lambda (exn)
				       (let ([wait-sema (make-semaphore)])
					 (parameterize ([current-eventspace (get-eventspace)])
					   (queue-callback (lambda () 
							     (resume-gui)
							     (semaphore-post wait-sema))))
					 (semaphore-wait wait-sema))
				       (semaphore-post suspend-sema)
				       (break-handler exn))])
		      (begin0
		       (let loop ()
			 (sync/enable-break resume-ch
					    (handle-evt
					     in-user-ch
					     (lambda (thunk)
					       (thunk)
					       (loop)))))
		       (semaphore-post suspend-sema))))
		  (if (pair? status)
		      (cdr status)
		      #f))))
          
          (define (my-execute debug?)
            (set! want-debug? debug?)
            (if debug?
                (show-debug)
                (hide-debug))
	    (set! current-language-settings (and debug?
						 (send (get-definitions-text) get-next-settings)))
            (set! breakpoints (make-hash-table))
            (hash-table-put! breakpoints -1 #t)
            (set! pos-vec (make-vector (add1 (send (get-definitions-text) last-position)) #f))
            (set! resume-ch (make-channel))
            (set! want-suspend-on-break? #f)
            (set! stack-frames #f)
            (send (get-definitions-text) set-parent! this)
            (send (get-interactions-text) set-parent! this)
            (super execute-callback))
          
          (define/override (execute-callback)
            (my-execute #f))
          
          (define/augment (enable-evaluation)
            (send debug-button enable #t)
            (inner (void) enable-evaluation))
          
          (define/augment (disable-evaluation)
            (send debug-button enable #f)
            (inner (void) disable-evaluation))
          
          (define debug-parent-panel 'uninitialized-debug-parent-panel)
          (define debug-panel 'uninitialized-debug-panel)
          (define/override (get-definitions/interactions-panel-parent)
            (set! debug-parent-panel
                  (make-object vertical-panel%
                    (super get-definitions/interactions-panel-parent)))
            (set! debug-panel (instantiate horizontal-panel% ()
                                (parent debug-parent-panel)
                                (stretchable-height #f)
                                (alignment '(center center))
                                (style '(border))))
            (send debug-parent-panel change-children (lambda (l) null))
            #;
            (instantiate button% ()
              (label "Hide")
              (parent debug-panel)
              (callback (lambda (x y) (hide-debug)))
              (stretchable-height #t))
            (make-object vertical-panel% debug-parent-panel))

          (define/public (hide-debug)
            (when (member debug-panel (send debug-parent-panel get-children))
              (send debug-parent-panel change-children
                    (lambda (l) (remq debug-panel l)))))
          
          (define/public (show-debug)
            (unless (member debug-panel (send debug-parent-panel get-children))
              (send debug-parent-panel change-children
                    (lambda (l) (cons debug-panel l)))))
                    
          (super-new)
          
          (define status-message
            (instantiate message% ()
              [label " "]
              [parent debug-panel]
              [stretchable-width #t]))
          
          (define debug-button
            (make-object button%
              ((bitmap-label-maker
		(string-constant debug-tool-button-name)
                (build-path (collection-path "mztake" "icons") "icon-small.png")) this)
              (make-object vertical-pane% (get-button-panel))
              (lambda (button evt)
                (my-execute #t))))
          
          (define pause-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Pause"
                       (build-path (collection-path "mztake" "icons") "pause.png")) this)]
              [parent debug-panel]
              [callback (lambda (button evt)
                          (if stack-frames
                              (bell)
                              (begin
                                (set! want-suspend-on-break? #t)
                                (send (get-current-tab) break-callback)
                                (send (get-current-tab) reset-offer-kill))))]
              [enabled #t]))
          
          (define resume-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Continue"
                       (build-path (collection-path "mztake" "icons") "resume.png")) this)]
              [parent debug-panel]
              [callback (lambda (button evt)
                          (if stack-frames
			      (resume)
                              (bell)))]
              [enabled #f]))
          
          (define step-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Step"
                       (build-path (collection-path "mztake" "icons") "step.png")) this)]
              [parent debug-panel]
              [callback (lambda (btn evt)
                          (if stack-frames
                              (begin
                                (hash-table-put! breakpoints -1 #t)
				(resume))
                              (bell)))]
              [enabled #f]))

          (define mouse-over-message
            (instantiate message% ()
              [label " "]
              [parent debug-panel]
              [stretchable-width #t]))

	  (define/augment (on-tab-change old new)
	    (check-current-language-for-debugger)
	    (inner (void) on-tab-change old new))

	  (define/public (check-current-language-for-debugger)
	    (if (debugger-does-not-work-for? (extract-language-level 
					      (send (get-definitions-text) get-next-settings)))
		(when (send debug-button is-shown?)
		  (send (send debug-button get-parent) delete-child debug-button))
		(unless (send debug-button is-shown?)
		  (send (send debug-button get-parent) add-child debug-button))))
          
          (send (get-button-panel) change-children
                (lambda (_)
                  (cons (send debug-button get-parent)
                        (remq (send debug-button get-parent) _))))

	  ; hide debug button if it's not supported for the initial language:
	  (check-current-language-for-debugger)))
      (drscheme:get/extend:extend-definitions-text debug-definitions-text-mixin)
      (drscheme:get/extend:extend-interactions-text debug-interactions-text-mixin)
      (drscheme:get/extend:extend-unit-frame debug-unit-frame-mixin))))
