(module web-cells mzscheme  
  (require (lib "struct.ss"))

  (define-struct (exn:fail:frame:top exn) ())
  (define (exn:fail:frame:top-raise)
    (raise (make-exn:fail:frame:top
	    "Reached top of stack"
	    (current-continuation-marks))))
  (provide exn:fail:frame:top?)
  
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
  (define *session-root-id* (gensym))

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
  
  ; session-root? : frame:ns -> boolean
  (define (session-root? a-frame)
    (annotation-present? *session-root-id* a-frame))
  
  ; make-frame/top : -> frame:ns
  (define (make-frame/top)
    (let* ([cur-top-box (*frame-stack*)]
	   [cur-top (unbox cur-top-box)])
      (cond
        #;[(not (frame:ns? cur-top))
           ; Construct global
           (copy-struct frame:ns (make-frame/parent cur-top-box)
                        [frame:ns-annotations (list (cons *global-root-id* #t))])]
        [(global-root? cur-top)
         ; Construct session
         (copy-struct frame:ns (make-frame/parent cur-top-box)
                      [frame:ns-annotations (list (cons *session-root-id* #t))])]
        [else
         ; Construct normal
         (make-frame/parent cur-top-box)])))
  
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
    (let ([initial-stack (*frame-stack*)])
      (begin0 (thunk)
              (*frame-stack* initial-stack)
              (push-frame!))))
  
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
  (define-struct (cell:global cell) ())
  (define-struct (cell:session cell) ())
  (define-struct (cell:local cell) ())
  
  ; ext:make-'a 'b -> 'a
  (define (ext:make-cell:global default)
    (let ([new-name (gensym)])
      (frame-set! (search-stack global-root?)
		  new-name default)
      (make-cell:global new-name)))
  (define (ext:make-cell:session default)
    (let ([new-name (gensym)])
      (frame-set! (search-stack global-root?)
                new-name default)
      (make-cell:session new-name)))
  (define (ext:make-cell:local default)
    (let ([new-name (gensym)])
      (frame-set! (search-stack global-root?)
                  new-name default)
      (make-cell:local new-name)))
      
  ; cell:global-ref : cell:global -> any
  ; returns the value of the global cell
  (define (cell:global-ref gc)
    (frame-ref (search-stack global-root?)
               (cell-id gc)))
  ; cell:global-set! : cell:global any -> void
  ; sets the value of the global cell
  (define (cell:global-set! gc nv)
    (frame-set! (search-stack global-root?)
                (cell-id gc)
                nv))
  
  ; cell:session-ref : cell:session -> any
  ; returns the value of the session cell
  (define (cell:session-ref sc)
    (frame-ref (search-stack session-root?)
               (cell-id sc)))
  ; cell:session-set! : cell:session any -> void
  ; sets the value of the session cell
  (define (cell:session-set! sc nv)
    (frame-set! (search-stack session-root?)
                (cell-id sc)
                nv))
  
  ; cell:local-ref : cell:local -> any
  ; returns the value of the local cell
  (define (cell:local-ref lc)
    (frame-ref (search-stack frame?)
               (cell-id lc)))
  ; cell:local-set! : cell:local any -> void
  ; sets the value of the local cell at the last place it was set, including the default
  (define (cell:local-set! lc nv)
    (frame-set! (search-stack
                 (lambda (f) (frame-set? f (cell-id lc))))
                (cell-id lc)
                nv))
  ; cell:local-mask : cell:local any -> void
  ; masks the local cell to the given value
  (define (cell:local-mask lc nv)
    (frame-set! (search-stack frame?)
                (cell-id lc)
                nv))
  
  ; cell-ref : cell -> any
  (define (cell-ref c)
    (cond 
      [(cell:global? c) (cell:global-ref c)]
      [(cell:session? c) (cell:session-ref c)]
      [(cell:local? c) (cell:local-ref c)]))
  
;  ;; linking parameters to cells
;  (define *parameter-links* (ext:make-cell:session (list)))
;  (define-struct parameter-link (parameter cell))
;  
;  ; link-parameter : parameter cell -> void
;  (define (link-parameter p c)
;    (cell:session-set! *parameter-links*
;                       (cons (make-parameter-link p c)
;                             (cell:session-ref *parameter-links*))))
;  
;  ; reinstall-linked-parameters : -> void
;  (define (reinstall-linked-parameters)
;    (for-each (lambda (link)
;                ((parameter-link-parameter link)
;                 (cell-ref (parameter-link-cell link))))
;              (cell:session-ref *parameter-links*)))
   
  (provide with-frame
           with-frame-after
           (rename cell:global? web-cell:global?)
           (rename ext:make-cell:global make-web-cell:global)
           (rename cell:global-ref web-cell:global-ref)
           (rename cell:global-set! web-cell:global-set!)
           (rename cell:session? web-cell:session?)
           (rename ext:make-cell:session make-web-cell:session)
           (rename cell:session-ref web-cell:session-ref)
           (rename cell:session-set! web-cell:session-set!)
           (rename cell:local? web-cell:local?)
           (rename ext:make-cell:local make-web-cell:local)
           (rename cell:local-ref web-cell:local-ref)
           (rename cell:local-set! web-cell:local-set!)
           (rename cell:local-mask web-cell:local-mask)))