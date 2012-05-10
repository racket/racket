#lang racket/base
(require racket/contract
         racket/list
         racket/match
         racket/serialize
         web-server/private/servlet
         web-server/managers/manager
         web-server/private/define-closure
         web-server/lang/web-cells)

;; **********************************************************************
;; **********************************************************************
;; AUXILLIARIES  
(define-struct mark-key ())
(define the-cont-key (make-mark-key))
(define the-save-cm-key (make-mark-key))
(define safe-call? (make-mark-key))
(define web-prompt (make-continuation-prompt-tag 'web)) 

(define empty-hash 
  (make-immutable-hash empty))
(define (with-current-saved-continuation-marks-and key val thnk)
  (call-with-immediate-continuation-mark
   the-save-cm-key
   (lambda (old-cms)
     (with-continuation-mark the-save-cm-key
       (hash-set old-cms key val)
       (thnk)))
   empty-hash))

;; current-continuation-as-list: -> (listof value)
;; check the safety marks and return the list of marks representing the continuation
(define (activation-record-list)
  (let* ([cm (current-continuation-marks web-prompt)]
         ; XXX call this once with a non-#f default
         [sl (continuation-mark-set->list* cm (list safe-call? continuation-of-unsafe-part-mark))])
    (if (calling-context-okay? sl #f)
        (store-unsafe-parts-on-server! (continuation-mark-set->list* cm (list the-cont-key the-save-cm-key continuation-of-unsafe-part-mark)))
        (error "Attempt to capture a continuation from within an unsafe context:" sl))))

;; calling-context-okay? : (listof (vector safe-call? unsafe-continuation-mark)) -> boolean
(define (calling-context-okay? ctxt native-above?)
  (match ctxt
    [(list) #t]
    [(list-rest (vector (or (list-rest safe-call? _)
                            safe-call?)
                        unsafe-part)
                more-ctxt)
     (and (or native-above? safe-call?)
          (calling-context-okay? 
           more-ctxt
           (or unsafe-part native-above?)))]))

;; abort: ( -> alpha) -> alpha
;; erase the stack and apply a thunk
(define (abort thunk)  
  (abort-current-continuation web-prompt thunk))

;; with-continuation-marks : (listof (cons any1 any2)) (-> any3) -> any3
(define (with-continuation-marks cms thnk)
  (match cms
    [(list) (thnk)]
    [(list-rest (cons cm-key cm-val) cms)
     (with-continuation-mark cm-key cm-val
       (with-continuation-marks cms thnk))]))

(define (with-continuation-marks/hash cms thnk)
  (with-continuation-marks 
   (hash-map cms cons)
   thnk))

;; resume*: (listof (value -> value)) value -> value
;; resume a computation given a value and list of frame procedures
(define (resume* frames val)
  #;(printf "~S\n" `(resume ,frames ,val))
  (match frames
    [(list)
     #;(printf "Returning value ~S\n" val)
     (apply values val)]
    [(list-rest frame fs)
     #;(printf "Frame ~S\n" frame)
     (match frame
       [(vector #f #f #f)
        ; XXX Perhaps I should err?
        #;(error 'resume "Empty frame")
        (resume* fs val)]
       [(vector f #f #f)
        (call-with-values (lambda () (with-continuation-mark the-cont-key f (resume* fs val)))
                          f)]
       [(vector #f cms #f)
        (with-continuation-mark the-save-cm-key cms
          (with-continuation-marks/hash cms (lambda () (resume* fs val))))]
       [(vector #f #f nkpt-label)
        (serial->native
         ((get-unsafe-part-from-server nkpt-label)
          (with-continuation-mark continuation-of-unsafe-part-mark nkpt-label
            (resume* fs val))))]
       [(vector f cms nkpt-label)
        (resume* (list* (vector f #f #f)
                       (vector #f cms #f)
                       (if nkpt-label
                           (list* (vector #f #f nkpt-label)
                                  fs)
                           fs))
                val)])]))

(define (resume frames val)
  (resume* (reverse frames) val))

;; rebuild-cms : frames (-> value) -> value
(define (rebuild-cms frames thunk)
  #;(printf "~S\n" `(rebuild-cms ,frames ,thunk))
  (match frames
    [(list) 
     (thunk)]
    [(list-rest frame fs)
     (match (vector-ref frame 1)
       [#f
        (rebuild-cms fs thunk)]
       [cms
        (with-continuation-marks/hash cms (lambda () (rebuild-cms fs thunk)))])]))

(define (call-with-web-prompt thunk)
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

(define (kont-append-fun k f)
  (define-values (wcs current-marks) ((kont-env k)))
  (make-kont 
   (lambda ()
     (values wcs (list* (vector f #f #f) current-marks)))))

;; send/suspend: (continuation -> response) -> request
;; produce the current response and wait for the next request
(define (call-with-serializable-current-continuation response-maker)
  (with-continuation-mark safe-call? '(#t send/suspend)
    (let* ([current-marks (activation-record-list)]
           [wcs (capture-web-cell-set)]
           [k (make-kont (lambda () (values wcs current-marks)))])
      (abort (lambda ()
               ; Since we escaped from the previous context, we need to re-install the user's continuation-marks
               (rebuild-cms (reverse current-marks) (lambda () (response-maker k))))))))

;; combining native and transformed continuations
(define unsafe-barrier-prompt-tag (make-continuation-prompt-tag 'unsafe))
(define continuation-of-unsafe-part-mark (make-mark-key))

(define (store-unsafe-part-on-server! k)
  ((manager-continuation-store! (current-servlet-manager))
   (current-servlet-instance-id) k #f))
(define (get-unsafe-part-from-server k-label)
  (apply (manager-continuation-lookup (current-servlet-manager))
         (current-servlet-instance-id) k-label))

(define store-unsafe-parts-on-server!
  (match-lambda
    [(list) empty]
    [(list-rest (vector f cms unsafe-part) ctxt)
     (list* (vector f cms
                    (if unsafe-part
                        (store-unsafe-part-on-server! unsafe-part)
                        #f))
            (store-unsafe-parts-on-server! ctxt))]))

(define-syntax-rule (serial->native f)
  (serial->native* (lambda () f)))
(define-syntax-rule (native->serial f)
  (native->serial* (lambda () f)))

(define (serial->native* thnk)
  (call-with-continuation-prompt thnk unsafe-barrier-prompt-tag))
(define (native->serial* thnk)
  (call-with-composable-continuation 
   (lambda (unsafe-continuation-portion)
     (with-continuation-mark
         continuation-of-unsafe-part-mark unsafe-continuation-portion
       (thnk)))
   unsafe-barrier-prompt-tag))

;; **********************************************************************
;; **********************************************************************
;; "CLIENT" INTERFACE

;; dispatch-start: (request -> response) request -> reponse
;; pass the initial request to the starting interaction point
(define (dispatch-start start req0)
  (call-with-web-prompt 
   (lambda ()
     (with-continuation-mark safe-call? '(#t start)
       (start
        (with-continuation-mark the-cont-key start
          req0))))))

;; dispatch: (request -> (request -> response)) request -> response
;; lookup the continuation for this request and invoke it
(define (dispatch decode-continuation req)   
  (call-with-web-prompt
   (lambda ()
     (cond
       [(decode-continuation req)
        => (lambda (k) (k req))]
       [else
        (error 'dispatch "no continuation associated with the provided request: ~S" req)]))))

;; **********************************************************************
;; **********************************************************************

; XXX These should really be from web-server/private, but it interferes with testing
(define request? any/c)
(define response? any/c)

(define cms? (and/c hash? immutable?))

(define saved-context?
  (listof (vector/c (or/c false/c procedure?)
                    (or/c false/c cms?)
                    (or/c false/c symbol?))))

(provide/contract
 ;; AUXILLIARIES
 [abort ((-> any) . -> . any)]
 [call-with-web-prompt ((-> any) . -> . any)]
 [resume (saved-context? any/c . -> . any)]
 [the-cont-key mark-key?]
 [the-save-cm-key mark-key?]
 [safe-call? mark-key?]
 [the-undef undef?]
 [activation-record-list (-> saved-context?)]
 [with-current-saved-continuation-marks-and (any/c any/c (-> any/c) . -> . any/c)]
 [kont-append-fun (kont? procedure? . -> . kont?)]
 
 ;; "CLIENT" INTERFACE
 [dispatch ((request? . -> . (request? . -> . response?))
            request?
            . -> .
            response?)]
 [dispatch-start ((request? . -> . response?)
                  request?
                  . -> .
                  response?)])
(provide
 ;; "SERVLET" INTERFACE
 ; A contract would interfere with the safe-call? key
 native->serial
 serial->native
 call-with-serializable-current-continuation)
