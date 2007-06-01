(module abort-resume mzscheme
  (require (lib "list.ss")
           (lib "plt-match.ss")
           (lib "serialize.ss")
           "../private/define-closure.ss"
           "../lang/web-cells.ss")
  (provide
   
   ;; AUXILLIARIES
   abort
   resume
   the-cont-key
   the-save-cm-key
   safe-call?
   the-undef
   activation-record-list
   current-saved-continuation-marks-and
   
   ;; "SERVLET" INTERFACE
   send/suspend
   
   ;; "CLIENT" INTERFACE
   dispatch-start
   dispatch)
  
  ;; **********************************************************************
  ;; **********************************************************************
  ;; AUXILLIARIES  
  (define-struct mark-key ())
  (define the-cont-key (make-mark-key))
  (define the-save-cm-key (make-mark-key))
  (define safe-call? (make-mark-key))
  (define web-prompt (make-continuation-prompt-tag 'web)) 
  
  (define (current-saved-continuation-marks-and key val)
    (reverse
     (list* (cons key val)
            (let-values ([(current)
                          (continuation-mark-set->list (current-continuation-marks) the-save-cm-key)])
              (if (empty? current)
                  empty
                  (first current))))))
  
  ;; current-continuation-as-list: -> (listof value)
  ;; check the safety marks and return the list of marks representing the continuation
  (define (activation-record-list)
    (let* ([cm (current-continuation-marks)]
           [sl (reverse (continuation-mark-set->list cm safe-call?))])
      (if (andmap (lambda (x)
                    (if (pair? x)
                        (car x)
                        x))
                  sl)
          (begin #;(printf "Safe continuation capture from ~S with cm ~S~n" sl cm)
                 #;(printf "CMs: ~S~n" (continuation-mark-set->list* cm (list the-cont-key the-save-cm-key)))
                 (reverse (continuation-mark-set->list* cm (list the-cont-key the-save-cm-key))))
          (error "Attempt to capture a continuation from within an unsafe context:" sl))))
  
  ;; abort: ( -> alpha) -> alpha
  ;; erase the stack and apply a thunk
  (define (abort thunk)
    #;(printf "abort ~S~n" thunk)
    (abort-current-continuation web-prompt thunk))
  
  ;; resume: (listof (value -> value)) value -> value
  ;; resume a computation given a value and list of frame procedures
  (define (resume frames val)
    #;(printf "~S~n" `(resume ,frames ,val))
    (match frames
      [(list)
       (apply values val)]
      [(list-rest f fs)
       (match f
         [(vector #f #f)
          (error 'resume "Empty frame")]
         [(vector f #f)
          (call-with-values (lambda () (with-continuation-mark the-cont-key f (resume fs val)))
                            f)]
         [(vector #f (list))
          (resume fs val)]
         [(vector #f (list-rest (list-rest cm-key cm-val) cms))
          (with-continuation-mark 
              the-save-cm-key 
            (current-saved-continuation-marks-and cm-key cm-val)
            (with-continuation-mark cm-key cm-val
              (begin
                #;(printf "r: w-c-m ~S ~S~n" cm-key cm-val)
                (resume (list* (vector #f cms) fs) val))))]
         [(vector f cm)
          (resume (list* (vector f #f) (vector #f cm) fs) val)])]))
  
  ;; rebuild-cms : frames (-> value) -> value
  (define (rebuild-cms frames thunk)
    #;(printf "~S~n" `(rebuild-cms ,frames ,thunk))
    (match frames
      [(list) 
       (thunk)]
      [(list-rest f fs)
       (match f
         [(vector f #f)
          (rebuild-cms fs thunk)]
         [(vector f (list))
          (rebuild-cms fs thunk)]
         [(vector f (list-rest (list-rest cm-key cm-val) cms))
          (with-continuation-mark cm-key cm-val
            (begin
              #;(printf "rcm: w-c-m ~S ~S~n" cm-key cm-val)
              (rebuild-cms (list* (vector #f cms) fs) thunk)))])]))
  
  (define (abort/cc thunk)
    (call-with-continuation-prompt
     thunk
     web-prompt))
  
  ;; a serializable undefined value
  (define-serializable-struct undef ())
  (define the-undef (make-undef))    
  
  ;; **********************************************************************
  ;; **********************************************************************
  ;; "SERVLET" INTERFACE  
  
  (define-closure kont x (wcs current-marks)
    (abort (lambda ()
             ; Restoring the web-cells is separate from the continuation
             (restore-web-cell-set! wcs)
             (resume current-marks x))))
  
  ;; send/suspend: (continuation -> response) -> request
  ;; produce the current response and wait for the next request
  (define (send/suspend response-maker)
    (with-continuation-mark safe-call? '(#t send/suspend)
      (let ([current-marks (activation-record-list)]
            [wcs (capture-web-cell-set)])
        ((lambda (k) 
           (abort (lambda ()
                    ; Since we escaped from the previous context, we need to re-install the user's continuation-marks
                    (rebuild-cms current-marks (lambda () (response-maker k))))))
         (make-kont (lambda () (values wcs current-marks)))))))
  
  ;; **********************************************************************
  ;; **********************************************************************
  ;; "CLIENT" INTERFACE
  
  ;; dispatch-start: (request -> response) request -> reponse
  ;; pass the initial request to the starting interaction point
  (define (dispatch-start start req0)
    (abort/cc 
     (lambda ()
       (with-continuation-mark safe-call? '(#t start)
         (start
          (with-continuation-mark the-cont-key start
            req0))))))
  
  ;; dispatch: (request -> (request -> response)) request -> response
  ;; lookup the continuation for this request and invoke it
  (define (dispatch decode-continuation req)   
    (abort/cc
     (lambda ()
       (cond
         [(decode-continuation req)
          => (lambda (k) (k req))]
         [else
          (error "no continuation associated with the provided request")])))))