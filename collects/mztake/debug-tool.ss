(module debug-tool mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "string.ss")
           ;(lib "math.ss")
           (lib "class.ss")
           (lib "unit.ss")
           (lib "contract.ss")
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
  
  (define (robust-syntax-source stx)
    (and (syntax? stx) (syntax-source stx)))
  
  ; QUESTIONS/IDEAS
  ; what is the right way to deal with macros?
  ; how can the three tool classes communicate with each other safely

  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^) 
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
      
      (define (robust-vector-ref vec idx)
        (if (< idx (vector-length vec))
            (vector-ref vec idx)
            #f))
      
      (define (break-at bp p)
        (hash-table-get bp p #f))
      
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
                   get-top-level-window
                   get-tab)
          
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
            (let ([breakpoints (send (get-tab) get-breakpoints)]
                  [shifts empty])
              (hash-table-for-each
               breakpoints
               (lambda (pos status)
                 (cond
                   ; deletion after breakpoint: no effect
                   [(<= pos start)]
                   ; deletion of breakpoint: remove from table
                   [(and (< start pos)
                         (<= pos (+ start len)))
                    (hash-table-remove! breakpoints pos)]
                   ; deletion before breakpoint: shift breakpoint
                   [(> pos (+ start len))
                    (hash-table-remove! breakpoints pos)
                    (set! shifts (cons (cons (- pos len) status) shifts))])))
              (for-each (lambda (p) (hash-table-put! breakpoints (car p) (cdr p)))
                        shifts))
            (inner (void) on-delete start len))
          (define/augment (after-delete start len)
            (inner (void) after-delete start len)
            (when (send (get-tab) debug?)
              (send (get-tab) hide-debug))
            (end-edit-sequence))
          
          (define/augment (on-insert start len)
            (begin-edit-sequence)
            (let ([breakpoints (send (get-tab) get-breakpoints)]
                  [shifts empty])
              (hash-table-for-each
               breakpoints
               (lambda (pos status)
                 (when (< start pos)
                   (hash-table-remove! breakpoints pos)
                   (set! shifts (cons (cons (+ pos len) status) shifts)))))
              (for-each (lambda (p) (hash-table-put! breakpoints (car p) (cdr p)))
                        shifts))
            (inner (void) on-insert start len))
          (define/augment (after-insert start len)
            (inner (void) after-insert start len)
            (when (send (get-tab) debug?)
              (send (get-tab) hide-debug))
            (end-edit-sequence))

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
            (send (get-tab) render v))
          
          (define/override (on-event event)
            (if (send (get-tab) debug?)
                (let ([breakpoints (send (get-tab) get-breakpoints)])
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
                           (let* ([frames (send (get-tab) get-stack-frames)]
                                  [pos-vec (send (get-tab) get-pos-vec)]
                                  [id (robust-vector-ref pos-vec pos)]
                                #;
                                  [_ (printf "frames = ~a~npos-vec = ~a~nid = ~a~n"
                                             frames pos-vec id)])
                             (send (get-tab)
                                   set-mouse-over-msg
                                   ; consider rewriting so we can look up top-level vars even
                                   ; when not suspended
                                   (cond
                                     [(and id frames
                                           (let/ec k
                                             (let* ([id-sym (syntax-e id)]
                                                    [binding (lookup-first-binding
                                                              (lambda (id2) (module-identifier=? id id2))
                                                              frames (lambda ()
                                                                       ;(printf "failed to find var ~a on stack~n" id)
                                                                       (k (clean-status
                                                                           (format "~a = ~a" id-sym
                                                                                   (render
                                                                                    ((send (get-tab) lookup-top-level-var
                                                                                           id
                                                                                           (lambda () (k #f))))))))))]
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
                                  (let ([pc (send (get-tab) get-pc)])
                                    (if (and pc (= pos pc))
                                        (let* ([stat (send (get-tab) get-break-status)]
                                               [f (get-top-level-window)]
                                               [rendered-value (if (cons? stat)
								   (if (= 2 (length stat))
                                                                       (render (cadr stat))
                                                                       (format "~a" (cons 'values 
                                                                                          (map (lambda (v) (render v)) (rest stat)))))
								   "")])
                                          (when (cons? stat)
                                            #;(send (make-object menu-item%
                                                      (clean-status (format "expr -> ~a" rendered-value))
                                                      menu
                                                      void) enable #f)
                                            (make-object menu-item%
                                              "Print return value to console"
                                              menu
                                              (lambda _ (send (get-tab) print-to-console (format "return val = ~a"
                                                                                                 rendered-value)))))
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
                                                      (send (get-tab) set-break-status
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
                                            (when (send (get-tab) get-stack-frames)
                                              (send (get-tab) resume))))))
                                  (send (get-canvas) popup-menu menu
                                        (+ 1 (inexact->exact (floor (send event get-x))))
                                        (+ 1 (inexact->exact (floor (send event get-y))))))]
                               [(invalid)
                                (let* ([frames (send (get-tab) get-stack-frames)]
                                       [pos-vec (send (get-tab) get-pos-vec)]
                                       [id (robust-vector-ref pos-vec pos)]
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
                                               (make-object menu-item%
                                                 (clean-status
                                                  (format "Print value of ~a to console" id-sym))
                                                 menu
                                                 (lambda (item evt)
                                                   (send (get-tab) print-to-console (format "~a = ~a" id-sym val))))
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
            (when (and (send (get-tab) debug?) (not before))
              (let ([breakpoints (send (get-tab) get-breakpoints)])
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
              (let ([pos (send (get-tab) get-pc)])
                (when pos
                  (let*-values ([(xl yl xr yr) (find-char-box this (sub1 pos) pos)]
                                [(ym) (average yl yr)])
                    (let ([op (send dc get-pen)]
                          [ob (send dc get-brush)])
                      (case (send (get-tab) get-break-status)
                        [(error) (send dc set-pen pc-err-pen)
                                 (send dc set-brush pc-err-brush)]
                        [(break) (send dc set-pen pc-brk-pen)
                                 (send dc set-brush pc-brk-brush)]
                        [else    (send dc set-pen pc-pen)
                                 (send dc set-brush pc-brush)]))
                    (drscheme:arrow:draw-arrow dc xl ym xr ym dx dy))
                  #;
                  (let loop ([end-pos pos]
                             [marks (send (get-tab) get-stack-frames)])
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
                          (case (send (get-tab) get-break-status)
                            [(error) (send dc set-pen pc-err-pen)
                                     (send dc set-brush pc-err-brush)]
                            [(break) (send dc set-pen pc-brk-pen)
                                     (send dc set-brush pc-brk-brush)]
                            [else    (send dc set-pen pc-pen)
                                     (send dc set-brush pc-brush)]))
                        (drscheme:arrow:draw-arrow dc xm0 ym0 xr ym dx dy)
                        (loop start-pos (rest marks)))))))))

	  (define/augment (after-set-next-settings s)
            (let ([tlw (get-top-level-window)])
              (when tlw
                (send tlw check-current-language-for-debugger)))
	    (inner (void) after-set-next-settings s))))
      
      (define (debug-interactions-text-mixin super%)
        (class super%
          
          (inherit run-in-evaluation-thread
                   display-results
                   #;get-tab)
          
          (super-instantiate ())
          
	  (define tab #f)
	  (define/private (get-tab) tab)
	  (define/public (set-tab t) (set! tab t))

          ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
          ;; adds debugging information to `sexp' and calls `oe'
          (define/private (make-debug-eval-handler oe break? break-before break-after)
            (lambda (orig-exp)
              (if (compiled-expression? (if (syntax? orig-exp)
                                                (syntax-e orig-exp)
                                                orig-exp))
                  (oe orig-exp)
                  (let loop ([exp (if (syntax? orig-exp)
                                      orig-exp
                                      (namespace-syntax-introduce
                                       (datum->syntax-object #f orig-exp)))])
                    (let ([top-e (expand-syntax-to-top-form exp)])
                      (parameterize ([current-eval oe])
                        (eval/annotations
                         top-e
                         (lambda (fn m) #f) ; TODO: multiple file support
                         (lambda (stx)
                           (let*-values ([(breakpoints) (send (get-tab) get-breakpoints)]
                                         [(pos-vec) (send (get-tab) get-pos-vec)]
                                         [(annotated break-posns)
                                          (annotate-for-single-stepping
                                           (expand-syntax top-e)
                                           break?
                                           break-before
                                           break-after
                                           (lambda (type bound binding)
                                             ;(display-results (list bound))
                                             (when (eq? (robust-syntax-source bound)
                                                        (robust-syntax-source exp))
                                               (let loop ([i 0])
                                                 (when (< i (syntax-span bound))
                                                   (vector-set! pos-vec (+ i (syntax-position bound)) binding)
                                                   (loop (add1 i))))))
                                           (lambda (mod var val)
                                             (send (get-tab) add-top-level-binding var val)
                                             #;
                                             (printf "top-level binding: ~a ~a ~a~n" mod var val))
                                           (send (get-tab) get-defs))])
                             (hash-table-for-each
                              breakpoints
                              (lambda (pos status)
                                ; possible efficiency problem for large files with many breakpoints
                                (when (and (syntax-position top-e)
                                           (>= pos (syntax-position top-e))
                                           (< pos (+ (syntax-position top-e) (syntax-span top-e)))
                                           (not (memq pos break-posns)))
                                  (hash-table-remove! breakpoints pos))))
                             (for-each (lambda (posn)
                                         (hash-table-put!
                                          breakpoints posn
                                          (hash-table-get breakpoints posn (lambda () #f)))) break-posns)
                             ;(display-results (list orig-exp))
                             annotated)))))))))
          
          (define/override (reset-console)
            (super reset-console)
            (when (and (get-tab) (send (get-tab) debug?))
              (let ([breakpoints (send (get-tab) get-breakpoints)])
                (run-in-evaluation-thread
                 (lambda ()
                   ;(print-struct #t)
                   (let ([self (current-thread)]
                         [oeh (uncaught-exception-handler)]
                         [err-hndlr (error-display-handler)])
                     (error-display-handler
                      (lambda (msg exn)
                        (err-hndlr msg exn)
                        (if (and (eq? self (current-thread)) (exn:fail? exn))
                            (send (get-tab) suspend oeh
                                  (continuation-mark-set->list (exn-continuation-marks exn) debug-key)
                                  'error)))) ; this breaks the buttons because it looks like we can resume
                     (current-eval
                      (make-debug-eval-handler
                       (current-eval)
                       (lambda (pos)
                         (or (hash-table-get breakpoints -1)
                             (let ([bp (hash-table-get breakpoints pos #f)])
                               (if (procedure? bp)
                                   (bp)
                                   bp))))
                       ; break-before
                       (lambda (top-mark ccm)
                         (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                           (send (get-tab) suspend oeh (cons top-mark debug-marks) 'entry-break)))
                       ; break-after
                       (case-lambda
                         [(top-mark ccm val)
                          (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                            (car (send (get-tab) suspend oeh (cons top-mark debug-marks) (list 'exit-break val))))]
                         [(top-mark ccm . vals) 
                          (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                            (apply values
				   (send (get-tab) suspend oeh (cons top-mark debug-marks) (cons 'exit-break vals))))])))
                     (uncaught-exception-handler
                      (lambda (exn)
                        (if (and (exn:break? exn) (send (get-tab) suspend-on-break?))
                            (let ([marks (exn-continuation-marks exn)]
                                  [cont (exn:break-continuation exn)])
                              (send (get-tab) suspend oeh (continuation-mark-set->list marks debug-key) 'break)
                              (cont))
                            (oeh exn))))))))))))
      
      (define (debug-tab-mixin super%)
        (class super%
          
          (inherit get-defs
                   get-ints
                   get-frame)

          (define breakpoints (make-hash-table))
          (hash-table-put! breakpoints -1 #f)
          (define suspend-sema (make-semaphore 1))
          (define resume-ch (make-channel))
          (define in-user-ch (make-channel))
          (define want-suspend-on-break? #f)
          (define want-debug? #f)
          (define/public (debug?) want-debug?)
          (define stack-frames #f)
	  (define current-language-settings #f)
          (define pos-vec (make-vector 1))
          (define/public suspend-on-break?
            (case-lambda
              [() want-suspend-on-break?]
              [(v) (set! want-suspend-on-break? v)]))
          (define/public (get-stack-frames) stack-frames)
          (define/public (get-pos-vec) pos-vec)
          (define/public (get-breakpoints) breakpoints)
          (define break-status #f)
          (define/public (get-break-status) break-status)
          (define/public (set-break-status stat) (set! break-status stat))
          (define top-level-bindings empty)
          (define/public (add-top-level-binding var val)
            (set! top-level-bindings (cons (cons var val) top-level-bindings)))
          (define/public (lookup-top-level-var var failure-thunk)
            #;
            (printf "looking for ~a in ~a~n" var top-level-bindings)
            (let loop ([bindings top-level-bindings])
              (cond
                [(empty? bindings) (failure-thunk)]
                [(let ([res (or (bound-identifier=? var (caar bindings))
                                (free-identifier=? var (caar bindings)))])
                   #;
                   (printf "~a = ~a -> ~a~n" var (caar bindings) res)
                   res) (cdar bindings)]
                [(loop (rest bindings))])))
          (define control-panel #f)
          (define/public (resume)
	    (let ([v break-status])
	      (resume-gui)
	      (channel-put resume-ch (if (pair? v)
					 (cdr v)
					 #f))))
          (define/public (set-mouse-over-msg msg)
            (send (get-frame) set-mouse-over-msg msg))
          
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
          
	  (define/public (print-to-console v)
	    ;; ==drscheme eventspace thread==
	    ;; only when a user thread is suspended
            (channel-put in-user-ch (lambda () (fprintf (current-error-port) " ### DEBUGGER: ~a~n" v))))
          
	  (define/public (suspend-gui frames status)
	    (set! want-suspend-on-break? #f)
	    (hash-table-put! breakpoints -1 #f)
	    (send (send (get-frame) get-pause-button) enable #f)
	    (send (send (get-frame) get-step-button) enable #t)
	    (send (send (get-frame) get-resume-button) enable #t)
	    ;;(fprintf (current-error-port) "break: ~a~n" (map expose-mark frames))
	    ;;(printf "status = ~a~n" status)
	    (set! stack-frames frames)
	    (set! break-status status)
	    (when (cons? status)
	      (let ([expr (mark-source (first frames))])
		(send (send (get-frame) get-status-message) set-label
		      (clean-status
		       (format "~a ==> ~a"
			       (trim-expr-str
				(send (get-defs) get-text
				      (sub1 (syntax-position expr))
				      (+ -1 (syntax-position expr) (syntax-span expr))))
			       (if (= 2 (length status))
				   (render (cadr status))
				   (cons 'values (map (lambda (v) (render v)) (rest status)))))))))
	    (cond [(get-pc) => (lambda (pc) (send (get-defs) scroll-to-position pc))])
	    (send (get-defs) invalidate-bitmap-cache))

	  (define/public (resume-gui)
	    (set! stack-frames #f)
	    (set! break-status #f)
	    (send (send (get-frame) get-pause-button) enable #t)
	    (send (send (get-frame) get-step-button) enable #f)
	    (send (send (get-frame) get-resume-button) enable #f)
	    (send (send (get-frame) get-status-message) set-label "")
	    (send (get-defs) invalidate-bitmap-cache))

          (define/public suspend
	    ;; ==called from user thread==
            (opt-lambda (break-handler frames [status #f])
	      ;; suspend-sema ensures that we allow only one suspended thread
	      ;;  at a time
	      (if (semaphore-try-wait? suspend-sema)
		  (begin
		    (parameterize ([current-eventspace (send (get-frame) get-eventspace)])
		      (queue-callback (lambda () (suspend-gui frames status))))
		    (with-handlers ([exn:break?
				     (lambda (exn)
				       (let ([wait-sema (make-semaphore)])
					 (parameterize ([current-eventspace (send (get-frame) get-eventspace)])
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
          
          (define/public (prepare-execution debug?)
            (set! want-debug? debug?)
            (if debug?
                (send (get-frame) show-debug)
                (send (get-frame) hide-debug))
	    (set! current-language-settings
                  (and debug? (send (get-defs) get-next-settings)))
            ;(set! breakpoints (make-hash-table))
            (hash-table-put! breakpoints -1 #t)
            (set! pos-vec (make-vector (add1 (send (get-defs) last-position)) #f))
            (set! top-level-bindings empty)
            (set! resume-ch (make-channel))
            (set! want-suspend-on-break? #f)
            (set! stack-frames #f)
            (send (get-ints) set-tab this))

          (define/public (hide-debug)
            (send (get-frame) hide-debug))
          
          (define/override (enable-evaluation)
            (send (send (get-frame) get-debug-button) enable #t)
            (super enable-evaluation))
          
          (define/override (disable-evaluation)
            (send (send (get-frame) get-debug-button) enable #f)
            (super disable-evaluation))
          
          (super-new)))
      
      (define (debug-unit-frame-mixin super%)
        (class super%
          
          (inherit get-button-panel
                   get-definitions-text
                   get-interactions-text
                   get-menu-bar
                   get-current-tab
                   get-top-level-window
		   get-eventspace)
          
	  (define current-language-settings #f)
          (define control-panel #f)
          (define/public (set-mouse-over-msg msg)
            (when (not (string=? msg (send mouse-over-message get-label)))
              (send mouse-over-message set-label msg)))
                    
          (define/override (execute-callback)
            (send (get-current-tab) prepare-execution #f)
            (super execute-callback))
          
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
                (send (get-current-tab) prepare-execution #t)
                (super execute-callback))))
          
          (define pause-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Pause"
                       (build-path (collection-path "mztake" "icons") "pause.png")) this)]
              [parent debug-panel]
              [callback (lambda (button evt)
                          (if (send (get-current-tab) get-stack-frames)
                              (bell)
                              (begin
                                (send (get-current-tab) suspend-on-break? #t)
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
                          (if (send (get-current-tab) get-stack-frames)
			      (send (get-current-tab) resume)
                              (bell)))]
              [enabled #f]))
          
          (define step-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Step"
                       (build-path (collection-path "mztake" "icons") "step.png")) this)]
              [parent debug-panel]
              [callback (lambda (btn evt)
                          (if (send (get-current-tab) get-stack-frames)
                              (begin
                                (hash-table-put! (send (get-current-tab) get-breakpoints) -1 #t)
				(send (get-current-tab) resume))
                              (bell)))]
              [enabled #f]))

          (define/public (get-debug-button) debug-button)
          (define/public (get-pause-button) pause-button)
          (define/public (get-resume-button) resume-button)
          (define/public (get-step-button) step-button)
          (define/public (get-status-message) status-message)
          
          (define mouse-over-message
            (instantiate message% ()
              [label " "]
              [parent debug-panel]
              [stretchable-width #t]))

	  (define/augment (on-tab-change old new)
	    (check-current-language-for-debugger)
            (if (send new debug?)
                (let ([status (send new get-break-status)])
                  (if status
                      (send new suspend-gui (send new get-stack-frames) status)
                      (send new resume-gui))
                  (show-debug))
                (hide-debug))
	    (inner (void) on-tab-change old new))

	  (define/public (check-current-language-for-debugger)
            (let* ([settings (send (get-definitions-text) get-next-settings)]
                   [lang (drscheme:language-configuration:language-settings-language settings)]
                   [visible? (and (send lang capability-value 'mztake:debug-button)
                                (not (debugger-does-not-work-for?
                                      (extract-language-level settings))))])
              (if visible?
                  (unless (send debug-button is-shown?)
                    (send (send debug-button get-parent) add-child debug-button))
                  (when (send debug-button is-shown?)
                    (send (send debug-button get-parent) delete-child debug-button)))))
          
          (send (get-button-panel) change-children
                (lambda (_)
                  (cons (send debug-button get-parent)
                        (remq (send debug-button get-parent) _))))

	  ; hide debug button if it's not supported for the initial language:
	  (check-current-language-for-debugger)))
      (drscheme:language:register-capability 'mztake:debug-button (flat-contract boolean?) #t)
      (drscheme:get/extend:extend-definitions-text debug-definitions-text-mixin)
      (drscheme:get/extend:extend-interactions-text debug-interactions-text-mixin)
      (drscheme:get/extend:extend-unit-frame debug-unit-frame-mixin)
      (drscheme:get/extend:extend-tab debug-tab-mixin))))
