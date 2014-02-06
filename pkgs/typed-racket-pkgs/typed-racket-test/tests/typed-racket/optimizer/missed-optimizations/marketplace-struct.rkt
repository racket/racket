#;#;
#<<END
TR info: marketplace-struct.rkt 38:9 vm -- struct constructor
TR info: marketplace-struct.rkt 38:9 vm -- struct constructor
TR info: marketplace-struct.rkt 89:39 process -- struct constructor
TR info: marketplace-struct.rkt 96:3 vm -- struct constructor
TR missed opt: marketplace-struct.rkt 118:34 (in-hash-keys (vm-processes state)) -- non-specialized for clause
TR missed opt: marketplace-struct.rkt 127:32 (in-hash-keys (vm-processes state)) -- non-specialized for clause
TR missed opt: marketplace-struct.rkt 132:46 (in-hash-keys (process-endpoints p)) -- non-specialized for clause
TR opt: marketplace-struct.rkt 103:2 (struct-copy vm state (processes (hash-set (vm-processes state) (Process-pid wp) wp))) -- dead else branch
TR opt: marketplace-struct.rkt 103:2 (struct-copy vm state (processes (hash-set (vm-processes state) (Process-pid wp) wp))) -- struct ref
TR opt: marketplace-struct.rkt 103:45 (vm-processes state) -- struct ref
TR opt: marketplace-struct.rkt 110:23 (vm-processes state) -- struct ref
TR opt: marketplace-struct.rkt 111:10 (if wp (struct-copy vm state (processes (hash-remove (vm-processes state) pid))) state) -- dead else branch
TR opt: marketplace-struct.rkt 111:10 (if wp (struct-copy vm state (processes (hash-remove (vm-processes state) pid))) state) -- struct ref
TR opt: marketplace-struct.rkt 112:60 (vm-processes state) -- struct ref
TR opt: marketplace-struct.rkt 118:48 (vm-processes state) -- struct ref
TR opt: marketplace-struct.rkt 127:46 (vm-processes state) -- struct ref
TR opt: marketplace-struct.rkt 132:60 (process-endpoints p) -- struct ref
TR opt: marketplace-struct.rkt 133:35 (process-endpoints p) -- struct ref
END
""
#lang typed/racket/base
#reader tests/typed-racket/optimizer/reset-port

;; excerpt from marketplace
;; a struct constructor logging bug was causing this to fail

(require racket/match)

(define-type PID Number)
(define-type (InterruptK State) (State -> (Transition State)))
(define-type (TrapK Event State) (Event -> (InterruptK State)))
(struct: (State)
	 transition ([state : State])
	 #:transparent)
(define-type (Transition State) (transition State))
(struct: role ([orientation : Orientation]
	       [interest-type : InterestType])
	 #:transparent)
(define-type Role role)
(define-type Orientation (U 'publisher 'subscriber))
(define-type (Constreeof X) (Rec CT (U X (Pairof CT CT) False Void Null)))
(struct: process-spec ([boot : (PID -> CoTransition)])
	 #:transparent)
(define-type ProcessSpec process-spec)
(define-type InterestType (U 'participant 'observer 'everything))
(define-type (Handler State) (TrapK EndpointEvent State))
(define-type EndpointEvent (U PresenceEvent
			      AbsenceEvent
			      MessageEvent))
(struct: presence-event ([role : Role]) #:transparent)
(struct: absence-event ([role : Role]) #:transparent)
(struct: message-event ([role : Role]) #:transparent)
(define-type PresenceEvent presence-event)
(define-type AbsenceEvent absence-event)
(define-type MessageEvent message-event)
(define-type CoTransition (All (Result) (All (State) (Transition State) -> Result) -> Result))



(struct: vm ([processes : (HashTable PID Process)]
	     [next-process-id : PID])
	 #:transparent)

(struct: (State)
	 process ([debug-name : Any]
		  [pid : PID]
		  [state : State]
		  [spawn-ks : (Listof (Pairof Integer (TrapK PID State)))] ;; hmm
		  [endpoints : (HashTable Any (endpoint State))]
		  [meta-endpoints : (HashTable Any (endpoint State))])
	 #:transparent)

(struct: (State)
	 endpoint ([id : eid]
		   [role : role]
		   [handler : (Handler State)])
	 #:transparent)

(struct: eid ([pid : PID]
	      [pre-eid : Any])
	 #:transparent)

(define-type Process		(All (R) (CoProcess R) -> R))
(define-type (CoProcess R)	(All (State) (process State) -> R))

(: mkProcess : (All (State) ((CoProcess Process) State)))
;; A kind of identity function, taking the components of a process to
;; a process.
(define (mkProcess p)
  (lambda (k) ((inst k State) p)))

(: Process-pid : Process -> PID)
(define (Process-pid wp) ((inst wp PID) process-pid))

;; Unwraps a process. Result is the type of the result of the
;; expression; State is a type variable to be bound to the process's
;; private state type. p is to be bound to the unwrapped process; wp
;; is the expression producing the wrapped process. body... are the
;; forms computing a value of type Result.
(define-syntax-rule (unwrap-process State Result (p wp) body ...)
  (let ()
    (: coproc : (All (State) (process State) -> Result))
    (define (coproc p)
      body ...)
    ((inst wp Result) coproc)))

;;---------------------------------------------------------------------------

(: make-vm : process-spec -> vm)
(define (make-vm boot)
  (define primordial (mkProcess ((inst process Void)
				 '#:primordial
				 -1
				 (void)
				 (list)
				 #hash()
				 #hash())))
  (vm (hash-set (ann #hash() (HashTable PID Process))
		(Process-pid primordial)
		primordial)
      0))

(: inject-process : vm Process -> vm)
(define (inject-process state wp)
  (struct-copy vm state [processes (hash-set (vm-processes state) (Process-pid wp) wp)]))

(: always-false : -> False)
(define (always-false) #f)

(: extract-process : vm PID -> (values vm (Option Process)))
(define (extract-process state pid)
  (define wp (hash-ref (vm-processes state) pid always-false))
  (values (if wp
	      (struct-copy vm state [processes (hash-remove (vm-processes state) pid)])
	      state)
	  wp))

(: process-map : (All (State) (process State) -> (process State)) vm -> vm)
(define (process-map f state)
  (for/fold ([state state]) ([pid (in-hash-keys (vm-processes state))])
    (let-values (((state wp) (extract-process state pid)))
      (if (not wp)
	  state
	  (unwrap-process State vm (p wp)
	    (inject-process state (mkProcess (f p))))))))

(: endpoint-fold : (All (A) (All (State) (process State) (endpoint State) A -> A) A vm -> A))
(define (endpoint-fold f seed state)
  (for/fold ([seed seed]) ([pid (in-hash-keys (vm-processes state))])
    (let-values (((state wp) (extract-process state pid)))
      (if (not wp)
	  seed
	  (unwrap-process State A (p wp)
	    (for/fold ([seed seed]) ([pre-eid (in-hash-keys (process-endpoints p))])
	      (define ep (hash-ref (process-endpoints p) pre-eid))
	      ((inst f State) p ep seed)))))))

;;; Local Variables:
;;; eval: (put 'unwrap-process 'scheme-indent-function 3)
;;; End:
