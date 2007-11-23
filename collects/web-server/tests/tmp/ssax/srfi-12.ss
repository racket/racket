; Module header is generated automatically
#cs(module srfi-12 mzscheme
(require (lib "defmacro.ss"))
(require "myenv.ss")

;************************************************************************
; srfi-12.scm
; This file is the part of SSAX package (http://ssax.sourceforge.net),
; which is in public domain.



;************************************************************************
;		Implementation of SRFI-12
;
; Most of the generic code and the comments are taken from
;
; SRFI-12: Exception Handling
; By William Clinger, R. Kent Dybvig, Matthew Flatt, and Marc Feeley
; http://srfi.schemers.org/srfi-12/

; The SRFI-12 Reference implementation has been amended where needed with
; a platform-specific code
;


;------------------------------------------------------------------------
; Catching exceptions
; The platform-specific part

; Procedure: with-exception-handler HANDLER THUNK 
; Returns the result(s) of invoking thunk. The handler procedure is
; installed as the current exception handler in the dynamic context of
; invoking thunk.

; Procedure: abort OBJ
; Raises a non-continuable exception represented by OBJ.
; The abort procedure does not ensure that its argument is a
; condition. If its argument is a condition, abort does not ensure that
; the condition indicates a non-continuable exception.
 
; Procedure: exc:signal OBJ
; Raises a continuable exception represented by OBJ.
; In SRFI-12, this procedure is named 'signal'. However, this name
; clashes with the name of an internal Bigloo procedure. In a compiled
; code, this clash leads to a Bus error.

; Procedure: current-exception-handler
; Returns the current exception handler.

(cond-expand
 (gambit
  ; The Gambit implementation relies on internal Gambit procedures,
  ; whose names start with ##
  ; Such identifiers cannot be _read_ on many other systems
  ; The following macro constructs Gambit-specific ids on the fly
  (define-macro (_gid id)
    (string->symbol (string-append "##" (symbol->string id))))
  
  ; `with-exception-handler` is built-in
  
  ; `abort` is built-in
    
  (define (exc:signal obj)	; Encapsulate the object into a cell
    (raise (list obj)))         ; to let Gambit know it's our object

  (define gambit-error error)	; Save the native Gambit 'error' function

  (define (error msg . args)
    (abort (make-property-condition
	    'exn
	    'message (cons msg args))))
  
  ; `current-exception-handler` is built-in  

 )

 (bigloo		       
  (define (with-exception-handler handler thunk)
    (try (thunk)
			; If we raised the condition explicitly, the proc
			; is a pair, whose car is the
			; argument that was passed to 'abort' or 'exc:signal'.
			; The cdr part of the pair is the
			; continuation (for a continuable exception)
	 (lambda (escape proc mes obj)
	   ;(cerr "exn! " proc mes obj nl)
	   (if (pair? proc)		; We've caught the exception thrown
	       (let ((cont (cdr proc)))	; by abort or exc:signal
		 (if (not (null? cont))
		     (cont (handler (car proc))) ; continue after the handler
		     (handler (car proc))) ; Let Bigloo handle the return
		 )			   ; from the handler
	       ; If (pair? proc) is false, we caught the exception
	       ; raised by Bigloo's runtime system
	       ; Let Bigloo handle the return from the handler
	       (handler
		(make-property-condition
		 'exn		; condition kind required by SRFI-12
		 'message
		 (list proc mes obj)))))))

  ; An "at hock" implementation
  (define-macro (handle-exceptions var handle-expr expr . more-exprs)
    `(try
      ,(cons `begin (cons expr more-exprs))		
      (lambda (escape proc mes obj)
        (let((,var
              (if (pair? proc)  ; by abort or exc:signal
                  (car proc)
                  (make-property-condition  ; required by SRFI-12
                   'exn
                   'message
                   (list proc mes obj)))))
          ,handle-expr))))
  
  (define (abort obj)		; Encapsulate the object into a cell
    (the_failure (list obj) "" "")	; to let Bigloo know it's our object
    (exit 4))			; In case the exc:signal handler returns
    
				; Encapsulate the object into a cell
				; to let Bigloo know it's our object.
				; In addition, we capture the continuation:
				; 'exc:signal' generates a continuable
				; exception
  (define (exc:signal obj)
    (bind-exit (escape)
      (the_failure (cons obj escape) "" "")))


  ; When the current-exception-handler is applied, we encapsulate the
  ; argument (the exception) into a cell to let the framework know
  ; it's our exception

  ; We need to capture the continuation at the point current-exception-handler
  ; is invoked, so we can come back to that point and issue 'abort'
  ; in the dynamic context where current-exception-handler is invoked.
  ; We assume that a call to the current-exception-handler is
  ; equivalent to the throwing of a non-continuable exception
  ; (SRFI-12 does not preclude such an assumption).
  
; DL: had to comment it out, because Bigloo compiler dislikes
;  CALL-WITH-CURRENT-CONTINUATION. A temporary solution.
;(define (current-exception-handler)
;    (let ((result
;	   (call-with-current-continuation
;	    (lambda (k)
;	      (lambda (exn) (k (list exn)))))))
;      (if (procedure? result) result
;	  (abort (car result)))))	; re-entrance after k was invoked

	  
  ; A simplified version (which is far more efficient on bigloo)
  ; If this function is invoked in the context of an exception handler,
  ; the function invokes a _parent_ exception handler.
  (define (parent-exception-handler)
    (lambda (exn) (exc:signal exn)))
  )

 (chicken  ; Chicken supports SRFI-12 natively
   (define exc:signal signal)
 )
  
 (plt
  
  ; DL: supported in PLT natively
;  ; Borrowed from Bigloo's cond-expand branch
;  (define (current-exception-handler)
;    (let ((result
;	   (call-with-current-continuation
;	    (lambda (k)
;	      (lambda (exn) (k (list exn)))))))
;      (if (procedure? result) result
;	  (abort (car result)))))
  
  
  ; A helper function which converts an exception (PLT internal exception
  ; or SRFI-12 exception) into CONDITION
  (define (exn:exception->condition obj)
    (cond
      ((exn? obj)  ; PLT internal exception
       (make-property-condition
        'exn		; condition kind required by SRFI-12
        'message
        (exn-message obj)))
      ((pair? obj)  ; exception generated by ABORT or EXN:SIGNAL
       (car obj))
      (else  ; some more conditions should be added, I guess
       obj)))
  
  
  (define-macro (with-exception-handler handler thunk)
    `(with-handlers
         (((lambda (x) #t)
           (lambda (x)
             (,handler (exn:exception->condition x)))))
       (,thunk)))
      
  
  ; Evaluates the body expressions expr1, expr2, ... in sequence with an
  ; exception handler constructed from var and handle-expr. Assuming no
  ; exception is raised, the result(s) of the last body expression is(are)
  ; the result(s) of the HANDLE-EXCEPTIONS expression.
  ; The exception handler created by HANDLE-EXCEPTIONS restores the dynamic
  ; context (continuation, exception handler, etc.) of the HANDLE-EXCEPTIONS
  ; expression, and then evaluates handle-expr with var bound to the value
  ; provided to the handler.
  (define-macro (handle-exceptions var handle-expr expr . more-exprs)
    (cons 
     `with-handlers
     (cons
      `(((lambda (x) #t)
         (lambda (x)
           (let ((,var (exn:exception->condition x)))
             ,handle-expr))))
      (cons expr more-exprs))))
  
  
  ; This implementation was borrowed from Gambit's cond-expand branch
  (define (abort obj)
    (raise (list obj))
    (exit 4))
    
  (define (exc:signal obj)
    (raise (list obj)))

  (define (signal obj)
    (raise (list obj)))
  
  )  ; end of PLT branch

)


;  (define (with-exception-handler handler thunk)
;    (let ((old #f))
;     (dynamic-wind
;      (lambda ()
;        (set! old *current-exn-handler*)
;        (set! *current-exn-handler* handler))
;      thunk
;      (lambda ()
;        (set! *current-exn-handler* old)))))
 
;  (define (abort obj)
;     ((CURRENT-EXCEPTION-HANDLER) obj)
;     (ABORT (make-property-condition
;             'exn
;             'message
;             "Exception handler returned")))
 
;  (define (exc:signal exn)
;   ((CURRENT-EXCEPTION-HANDLER) exn))


;------------------------------------------------------------------------
; Exception conditions
; The following is an approximate implementation of conditions that
; uses lists, instead of a disjoint class of values
; The code below is basically the reference SRFI-12 implementation,
; with a few types fixed.

; A condition is represented as a pair where the first value of the
; pair is this function. A program could forge conditions, and they're
; not disjoint from Scheme pairs.
; Exception conditions are disjoint from any other Scheme values
; (or so should appear).
(define (condition? obj)
  (and (pair? obj)
       (eq? condition? (car obj))))


; Procedure: make-property-condition KIND-KEY PROP-KEY VALUE ...
; This procedure accepts any even number of arguments after kind-key,
; which are regarded as a sequence of alternating prop-key and value
; objects. Each prop-key is regarded as the name of a property, and
; each value is regarded as the value associated with the key that
; precedes it. Returns a kind-key condition that associates the given
; prop-keys with the given values.
(define (make-property-condition kind-key . prop-vals)
  (cons condition? (list (cons kind-key prop-vals))))


; Procedure: make-composite-condition CONDITION ...
; Returns a newly-allocated condition whose components correspond to
; the the given conditions. A predicate created by CONDITION-PREDICATE
; returns true for the new condition if and only if it returns true
; for one or more of its component conditions.
(define (make-composite-condition . conditions)
  (cons condition? (apply append (map cdr conditions))))
 

; Procedure: condition-predicate KIND-KEY 
; Returns a predicate that can be called with any object as its
; argument. Given a condition that was created by
; make-property-condition, the predicate returns #t if and only if
; kind-key is EQV? to the kind key that was passed to
; make-property-condition. Given a composite condition created with
; make-composite-condition, the predicate returns #t if and only if
; the predicate returns #t for at least one of its components.
(define (condition-predicate kind-key)
  (lambda (exn)
    (and (condition? exn) (assv kind-key (cdr exn)))))
 
; Procedure: condition-property-accessor KIND-KEY PROP-KEY
; Returns a procedure that can be called with any condition that satisfies
; (condition-predicate KIND-KEY). Given a condition that was created by
; make-property-condition and KIND-KEY, the procedure returns the value
; that is associated with prop-key. Given a composite condition created with
; make-composite-condition, the procedure returns the value that is
; associated with prop-key in one of the components that
; satisfies (condition-predicate KIND-KEY). 
; Otherwise, the result will be #f

(define (condition-property-accessor kind-key prop-key)
  (lambda (exn)
    (let* ((p ((condition-predicate kind-key) exn))
	   (prop-lst (and p (pair? p) (memq prop-key (cdr p)))))
      (and prop-lst (pair? (cdr prop-lst)) (cadr prop-lst)))))



(provide (all-defined)))
