(module web-cells mzscheme  
  (require (lib "struct.ss")
           (lib "contract.ss"))
  
  (define-struct (exn:fail:frame:top exn) ())
  (define (exn:fail:frame:top-raise)
    (raise (make-exn:fail:frame:top
	    "Reached top of stack"
	    (current-continuation-marks))))
  
  ;; frames
  (define-struct frame ())
  (define-struct (frame:empty frame) ())
  ; frame:ns : (alist * (box frame) * namespace)
  (define-struct (frame:ns frame) (annotations boxed-parent namespace))
  
  ; frame:ns?/raise : frame -> frame
  (define (frame:ns?/raise f)
    (if (frame:ns? f)
	f
	(exn:fail:frame:top-raise)))
  
  ; make-frame/parent : (box frame) -> frame:ns
  (define (make-frame/parent parent-frame-box)
    (make-frame:ns (list) parent-frame-box (make-namespace 'empty)))
  
  ; search-frames : frame:ns (frame:ns -> boolean?) -> frame
  ; Returns the first frame in the stack that matches the predicate
  (define (search-frames a-frame predicate?)
    (if (predicate? a-frame)
	a-frame
	(search-frames (frame:ns?/raise
			(unbox (frame:ns-boxed-parent a-frame)))
		       predicate?)))
  
  ; frame-ref : frame:ns symbol -> any
  ; Lookups up the variable in the frame and its parent(s)
  (define (frame-ref a-frame var)
    #;(printf "~S~n" (list (namespace-mapped-symbols (frame:ns-namespace a-frame)) var))
    (namespace-variable-value 
     var #f 
     (lambda ()
       (frame-ref (frame:ns?/raise
		   (unbox (frame:ns-boxed-parent a-frame)))
                  var))
     (frame:ns-namespace a-frame)))
  
  ; frame-set? : frame:ns symbol -> boolean
  (define (frame-set? a-frame var)
    (not
     (not 
      (namespace-variable-value
       var #f
       (lambda () #f)
       (frame:ns-namespace a-frame)))))
  
  ; frame-set! : frame:ns symbol any -> void
  ; Sets the variable in the frame to a value
  (define (frame-set! a-frame var val)
    (namespace-set-variable-value! 
     var val
     #t (frame:ns-namespace a-frame)))
  
  ;; frame stacks  
  (define *global-root-id* (gensym))
  
  ; *frame-stack* : (box frame)
  (define *frame-stack*
    (make-parameter
     (box (copy-struct frame:ns (make-frame/parent (box (make-frame:empty)))
                       [frame:ns-annotations (list (cons *global-root-id* #t))]))))
  
  ; annotation-present? : symbol frame:ns -> boolean
  (define (annotation-present? i a-frame)
    (not (not (assq i (frame:ns-annotations a-frame)))))
  
  ; global-root? : frame:ns -> boolean
  (define (global-root? a-frame)
    (annotation-present? *global-root-id* a-frame))
    
  ; make-frame/top : -> frame:ns
  (define (make-frame/top)
    (define cur-top-box (*frame-stack*))
    (define cur-top (unbox cur-top-box))
    (make-frame/parent cur-top-box))
  
  ; push-frame! : -> void
  ; Pushs a new frame onto the session stack
  (define (push-frame!)
    (*frame-stack* (box (make-frame/top))))
  
  ; pop-frame! : -> void
  ; Pops the frame from the stack
  (define (pop-frame!)
    (*frame-stack* (frame:ns-boxed-parent (unbox (*frame-stack*)))))
  
  ; save-stack/push/return : (-> 'a) -> 'a
  ; Pushes a frame after the thunk's execution with the same parent as the call site
  (define (save-stack/push/return thunk)
    (define initial-stack (*frame-stack*))
    (begin0 (thunk)
            (*frame-stack* initial-stack)
            (push-frame!)))
  
  ; syntax version of above
  (define-syntax with-frame-after
    (syntax-rules ()
      [(_ body ...)
       (save-stack/push/return (lambda () body ...))]))
  
  ; parameterized-push : (-> 'a) -> 'a
  (define (parameterized-push thunk)
    (parameterize ([*frame-stack* (box (make-frame/top))])
      (thunk)))
  
  ; syntax version of above
  (define-syntax with-frame
    (syntax-rules ()
      [(_ body ...)
       (parameterized-push (lambda () body ...))]))
  
  ; search-stack : (frame -> boolean) -> frame
  (define (search-stack predicate?)
    (search-frames (frame:ns?/raise (unbox (*frame-stack*)))
		   predicate?))
  
  ; cells
  (define-struct cell (id))
  (define-struct (cell:local cell) ())
  
  (define web-cell? cell:local?)
  
  ; ext:make-'a 'b -> 'a
  (define (make-web-cell default)
    (define new-name (gensym))
    (frame-set! (search-stack global-root?)
                new-name default)
    (make-cell:local new-name))
    
  ; cell:local-ref : cell:local -> any
  ; returns the value of the local cell
  (define (web-cell-ref lc)
    (frame-ref (search-stack frame?)
               (cell-id lc)))  
  ; cell:local-mask : cell:local any -> void
  ; masks the local cell to the given value
  (define (web-cell-shadow lc nv)
    (frame-set! (search-stack frame?)
                (cell-id lc)
                nv))
    
  (provide with-frame ; syntax
           with-frame-after)
  (provide/contract
   [web-cell? (any/c . -> . boolean?)]
   [make-web-cell (any/c . -> . web-cell?)]
   [web-cell-ref (web-cell? . -> . any/c)]
   [web-cell-shadow (web-cell? any/c . -> . void)]))