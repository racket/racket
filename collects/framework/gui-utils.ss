
(module gui-utils mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "contract.ss")
           (lib "string-constant.ss" "string-constants"))
  
  (define-syntax (provide/contract/docs stx)
    (syntax-case stx ()
      [(_ (name contract docs ...) ...)
       (syntax (provide/contract (name contract) ...))]))
  
  (provide/contract/docs
   
   (gui-utils:trim-string
    (string?
     (and/c number? positive?)
     . ->d .
     (λ (str size)
       (and/c string?
	      (λ (str)
		((string-length str) . <= . size)))))
    (str size)
    "Constructs a string whose size is less"
    "than \\var{size} by trimming the \\var{str}"
    "and inserting an ellispses into it.")

   (gui-utils:cancel-on-right?
    (-> boolean?)
    ()
    "Returns \\scheme{#t} if cancel should be on the right-hand side (or below)"
    "in a dialog and \\scheme{#f} otherwise."
    ""
    "See also"
    "@flink gui-utils:ok/cancel-buttons %"
    ".")
   (gui-utils:ok/cancel-buttons
    (opt->*
     ((is-a?/c area-container<%>)
      ((is-a?/c button%) (is-a?/c event%) . -> . any)
      ((is-a?/c button%) (is-a?/c event%) . -> . any))
     (string?
      string?)
     ((is-a?/c button%)
      (is-a?/c button%)))
    ((parent
      confirm-callback
      cancel-callback)
     ((confirm-label (string-constant ok))
      (cancel-label (string-constant cancel))))
    "Adds an Ok and a cancel button to a panel, changing the order"
    "to suit the platform. Under \\MacOSBoth{} and unix, the confirmation action"
    "is on the right (or bottom) and under Windows, the canceling action is on the"
    "right (or bottom)."
    "The confirmation action button has the \\scheme|'(border)| style."
    "The buttons are also sized to be the same width."
    ""
    "The first result is be the OK button and the second is"
    "the cancel button."
    ""
    "See also"
    "@flink gui-utils:cancel-on-right? %"
    ".")
   
   (gui-utils:next-untitled-name
    (-> string?)
    ()
    "Returns a name for the next opened untitled frame. The first"
    "name is ``Untitled'', the second is ``Untitled 2'',"
    "the third is ``Untitled 3'', and so forth.")
   (gui-utils:cursor-delay
    (case->
     (-> real?)
     (real? . -> . void?))
    (() (new-delay))
    "This function is {\\em not\\/} a parameter."
    "Instead, the state is just stored in the closure."
    ""
    "The first case in the case lambda"
    "returns the current delay in seconds before a watch cursor is shown,"
    "when either \\iscmprocedure{gui-utils:local-busy-cursor} or"
    "\\iscmprocedure{gui-utils:show-busy-cursor} is called."

    "The second case in the case lambda"
    "Sets the delay, in seconds, before a watch cursor is shown, when"
    "either \\iscmprocedure{gui-utils:local-busy-cursor} or"
    "\\iscmprocedure{gui-utils:show-busy-cursor} is called.")
   (gui-utils:show-busy-cursor
    (opt->
     ((-> any/c))
     (integer?)
     any/c)
    ((thunk)
     ((delay (gui-utils:cursor-delay))))
    "Evaluates \\rawscm{(\\var{thunk})} with a watch cursor. The argument"
    "\\var{delay} specifies the amount of time before the watch cursor is"
    "opened. Use \\iscmprocedure{gui-utils:cursor-delay} to set this value"
    "to all calls."
    ""
    "This function returns the result of \\var{thunk}.")
   (gui-utils:delay-action
    (real?
     (-> void?)
     (-> void?)
     . -> .
     void?)
    (delay-time open close)
    "Use this function to delay an action for some period of time. It also"
    "supports cancelling the action before the time period elapses. For"
    "example, if you want to display a watch cursor, but you only want it"
    "to appear after 2 seconds and the action may or may not take more than"
    "two seconds, use this pattern:"
    ""
    "\\begin{schemedisplay}"
    "(let ([close-down"
    "       (gui-utils:delay-action"
    "        2"
    "        (λ () .. init watch cursor ...)"
    "        (λ () .. close watch cursor ...))])"
    "  ;; .. do action ..."
    "  (close-down))"
    "\\end{schemedisplay}"
    ""
    "Creates a thread that waits \\var{delay-time}. After \\var{delay-time}"
    "has elapsed, if the result thunk has {\\em not} been called, call"
    "\\var{open}. Then, when the result thunk is called, call"
    "\\var{close}. The function \\var{close} will only be called if"
    "\\var{open} has been called.")

   (gui-utils:local-busy-cursor
    (opt->
     ((is-a?/c window<%>)
      (-> any/c))
     (integer?)
     any/c)
    ((window thunk)
     ((delay (gui-utils:cursor-delay))))
    "Evaluates \\rawscm{(\\var{thunk})} with a watch cursor in \\var{window}. If"
    "\\var{window} is \\rawscm{\\#f}, the watch cursor is turned on globally. The"
    "argument \\var{delay} specifies the amount of time before the watch"
    "cursor is opened. Use "
    "@flink gui-utils:cursor-delay "
    "to set this value for all uses of this function."
    ""
    "The result of this function is the result of \\var{thunk}.")

   (gui-utils:unsaved-warning
    (opt->
     (string?
      string?)
     (boolean?
      (union false/c
	     (is-a?/c frame%)
	     (is-a?/c dialog%)))
     (symbols 'continue 'save 'cancel))
    ((filename action)
     ((can-save-now? #f)
      (parent #f)))

    "This displays a dialog that warns the user of a unsaved file."
    ""
    "The string, \\var{action}, indicates what action is about to"
    "take place, without saving. For example, if the application"
    "is about to close a file, a good action is \\rawscm{\"Close"
    "Anyway\"}. The result symbol indicates the user's choice. If"
    "\\var{can-save-now?} is \\rawscm{\\#f}, this function does not"
    "give the user the ``Save'' option and thus will not return"
    "\\rawscm{'save}.")

   (gui-utils:get-choice
    (opt->
     (string?
      string?
      string?)
     (string?
      any/c
      (union false/c (is-a?/c frame%) (is-a?/c dialog%))
      (symbols 'app 'caution 'stop))
     any/c)
    ((message true-choice false-choice)
     ((title (string-constant warning))
      (default-result 'disallow-close)
      (paren #f)
      (style 'app)))

    "Opens a dialog that presents a binary choice to the user. The user is forced"
    "to choose between these two options, ie cancelling or closing the dialog"
    "opens a message box asking the user to actually choose one of the two options."
    ""
    "The dialog will contain the string \\var{message} and two buttons,"
    "labeled with the \\var{true-choice} and the \\var{false-choice}.  If the"
    "user clicks on \\var{true-choice} \\rawscm{\\#t} is returned. If the user"
    "clicks on \\var{false-choice}, \\rawscm{\\#f} is returned."
    ""
    "The argument \\var{default-result} determines how closing the window is"
    "treated. If the argument is \\rawscm{'disallow-close}, closing the window"
    "is not allowed. If it is anything else, that value is returned when"
    "the user closes the window."
    ""
    "If "
    "@flink gui-utils:cancel-on-right?"
    "returns \\scheme|#t|, the false choice is on the right."
    "Otherwise, the true choice is on the right."
    ""
    "The \\var{style} parameter is (eventually) passed to"
    "@link message"
    "as an icon in the dialog.")

   (gui-utils:get-clicked-clickback-delta
    (-> (is-a?/c style-delta%))
    ()
    "This delta is designed for use with"
    "@link text set-clickback %"
    ". Use it as one of the \\iscmclass{style-delta} argument to"
    "@link text set-clickback %"
    "."
    ""
    "See also"
    "@flink gui-utils:get-clickback-delta %"
    ".")

   (gui-utils:get-clickback-delta
    (-> (is-a?/c style-delta%))
    ()
    "This delta is designed for use with"
    "@link text set-clickback %"
    ". Use the result of this function as the style"
    "for the region"
    "text where the clickback is set." 
    ""
    "See also"
    "@flink gui-utils:get-clicked-clickback-delta %"
    "."))

  (define (trim-string str size)
    (let ([str-size (string-length str)])
      (cond
	[(<= str-size size)
	 str]
	[else
	 (let* ([between "..."]
		[pre-length (- (quotient size 2)
			       (quotient (string-length between) 2))]
		[post-length (- size
				pre-length
				(string-length between))])
	   (cond
	     [(or (<= pre-length 0)
		  (<= post-length 0))
	      (substring str 0 size)]
	     [else
	      (string-append
	       (substring str 0 pre-length)
	       between
	       (substring str
			  (- str-size post-length)
			  str-size))]))])))
  
  ;; selected-text-color : color
  (define selected-text-color (send the-color-database find-color "black"))

  ;; unselected-text-color : color
  (define unselected-text-color (case (system-type)
                                  [(macosx) (make-object color% 75 75 75)]
                                  [else (send the-color-database find-color "black")]))

  ;; selected-brush : brush
  (define selected-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))

  ;; unselected-brush : brush
  (define unselected-brush (send the-brush-list find-or-create-brush (get-panel-background) 'solid))

  ;; button-down/over-brush : brush
  (define button-down/over-brush 
    (case (system-type)
      [(macosx) (send the-brush-list find-or-create-brush
                      "light blue"
                      'solid)]
      [else
       (send the-brush-list find-or-create-brush
             (make-object color% 225 225 255)
             'solid)]))

  
  ;; name-box-pen : pen
  ;; this pen draws the lines around each individual item
  (define name-box-pen (send the-pen-list find-or-create-pen "black" 1 'solid))

  ;; background-brush : brush
  ;; this brush is set when drawing the background for the control
  (define background-brush 
    (case (system-type)
      [(macosx) (send the-brush-list find-or-create-brush (get-panel-background) 'panel)]
      [else (send the-brush-list find-or-create-brush "white" 'solid)]))
  
  ;; background-pen : pen
  ;; this pen is set when drawing the background for the control
  (define background-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
    
  ;; label-font : font
  (define label-font (send the-font-list find-or-create-font
                           (if (eq? (system-type) 'windows) 10 12)
                           'system 'normal
                           (if (eq? (system-type) 'macosx) 'bold 'normal)
                           #f))

  ;; name-gap : number
  ;; the space between each name
  (define name-gap 4)
  
  ;; hang-over : number
  ;; the amount of space a single entry "slants" over
  (define hang-over 8)
  
  ;; top-space : number
  ;; the gap at the top of the canvas, above all the choices
  (define top-space 4)
  
  ;; bottom-space : number
  ;; the extra space below the words
  (define bottom-space 2)
  
  ;; end choices-canvas%
  
  (define (cancel-on-right?) (eq? (system-type) 'windows))
  
  (define ok/cancel-buttons
    (opt-lambda (parent 
                 confirm-callback
                 cancel-callback 
                 [confirm-str (string-constant ok)]
                 [cancel-str (string-constant cancel)])
      (let ([confirm (λ ()
                       (instantiate button% ()
                         (parent parent)
                         (callback confirm-callback)
                         (label confirm-str)
                         (style '(border))))]
            [cancel (λ ()
                      (instantiate button% ()
                        (parent parent)
                        (callback cancel-callback)
                        (label cancel-str)))])
        (let-values ([(b1 b2)
                      (cond
                        [(cancel-on-right?)
                         (values (confirm) (cancel))]
                        [else
                         (values (cancel) (confirm))])])
          (let ([w (max (send b1 get-width)
                        (send b2 get-width))])
            (send b1 min-width w)
            (send b2 min-width w)
	    (if (cancel-on-right?)
		(values b1 b2)
		(values b2 b1)))))))

  
  (define clickback-delta (make-object style-delta% 'change-underline #t))
  (send clickback-delta set-delta-foreground "BLUE")
  (define (get-clickback-delta) clickback-delta)
  (define clicked-clickback-delta (make-object style-delta%))
  (send clicked-clickback-delta set-delta-background "BLACK")
  (define (get-clicked-clickback-delta) clicked-clickback-delta)
  
  (define next-untitled-name
    (let ([n 1])
      (λ ()
        (begin0
          (cond
            [(= n 1) (string-constant untitled)]
            [else (format (string-constant untitled-n) n)])
          (set! n (+ n 1))))))
  
  (define cursor-delay
    (let ([x 0.25])
      (case-lambda
        [() x]
        [(v) (set! x v) x])))
  
  (define show-busy-cursor
    (opt-lambda (thunk [delay (cursor-delay)])
      (local-busy-cursor #f thunk delay)))
  
  (define delay-action
    (λ (delay-time open close)
      (let ([semaphore (make-semaphore 1)]
            [open? #f]
            [skip-it? #f])
        (thread 
         (λ ()
           (sleep delay-time)
           (semaphore-wait semaphore)
           (unless skip-it?
             (set! open? #t)
             (open))
           (semaphore-post semaphore)))
        (λ ()
          (semaphore-wait semaphore)
          (set! skip-it? #t)
          (when open?
            (close))
          (semaphore-post semaphore)))))
  
  (define local-busy-cursor
    (let ([watch (make-object cursor% 'watch)])
      (case-lambda
        [(win thunk) (local-busy-cursor win thunk (cursor-delay))]
        [(win thunk delay)
         (let* ([old-cursor #f]
                [cursor-off void])
           (dynamic-wind
            (λ ()
              (set! cursor-off
                    (delay-action
                     delay
                     (λ ()
                       (if win
                           (begin (set! old-cursor (send win get-cursor))
                                  (send win set-cursor watch))
                           (begin-busy-cursor)))
                     (λ ()
                       (if win
                           (send win set-cursor old-cursor)
                           (end-busy-cursor))))))
            (λ () (thunk))
            (λ () (cursor-off))))])))
  
  (define unsaved-warning
    (opt-lambda (filename action-anyway (can-save-now? #f) (parent #f))
      (let ([mb-res (message-box/custom 
                     (string-constant warning)
                     (format (string-constant file-is-not-saved) filename)
                     (string-constant save)
                     (string-constant cancel)
                     action-anyway
                     parent
                     (if can-save-now?
                         '(default=1 caution)
                         '(default=2 caution))
                     2)])
        (case mb-res
          [(1) 'save]
          [(2) 'cancel]
          [(3) 'continue]))))
  
  (define get-choice
    (opt-lambda (message 
                 true-choice
                 false-choice 
                 (title (string-constant warning))
                 (default-result 'disallow-close)
                 (parent #f)
                 (style 'app))
      (let ([mb-res (message-box/custom
                     title
                     message
                     true-choice
                     false-choice
                     #f
                     parent
                     (case default-result
                       [(disallow-close) 
                        (if (eq? style 'app)
                            `(default=1 disallow-close)
                            `(default=1 disallow-close ,style))]
                       [else (if (eq? style 'app)
                                 `(default=1)
                                 `(default=1 ,style))])
                     default-result)])
        (case mb-res
          [(1) #t]
          [(2) #f]
          [else mb-res]))))
  
  ;; manual renaming
  (define gui-utils:trim-string trim-string)
  (define gui-utils:next-untitled-name next-untitled-name)
  (define gui-utils:show-busy-cursor show-busy-cursor)
  (define gui-utils:delay-action delay-action)
  (define gui-utils:local-busy-cursor local-busy-cursor)
  (define gui-utils:unsaved-warning unsaved-warning)
  (define gui-utils:get-choice get-choice)
  (define gui-utils:get-clicked-clickback-delta get-clicked-clickback-delta)
  (define gui-utils:get-clickback-delta get-clickback-delta)
  (define gui-utils:ok/cancel-buttons ok/cancel-buttons)
  (define gui-utils:cancel-on-right? cancel-on-right?)
  (define gui-utils:cursor-delay cursor-delay))
