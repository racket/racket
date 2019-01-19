#lang racket/base

(require "place-local.rkt"
         "check.rkt"
         "internal-error.rkt"
         "host.rkt"
         "atomic.rkt"
         "parameter.rkt"
         "../common/queue.rkt"
         "thread.rkt"
         "lock.rkt")

(provide init-future-place!
         futures-enabled?
         current-future
         future
         future?
         would-be-future
         touch
         future-block
         future-wait
         current-future-prompt
         future:condition-broadcast
         future:condition-signal
         future:condition-wait
         future:make-condition
         signal-future
         reset-future-logs-for-tracing!
         mark-future-trace-end!)

(define place-main-thread-id (make-pthread-parameter 0))

(define (init-future-place!)
  (place-main-thread-id (get-pthread-id)))

;; not sure of order here...
(define (get-caller)
  (cond
    [(current-future)
     (current-future)]
    [(not (= (place-main-thread-id) (get-pthread-id)))
     (get-pthread-id)]
    [else
     (current-thread)]))


;; ---------------------------- futures ----------------------------------


(define ID (box 1))

(define get-next-id
  (lambda ()
    (let ([id (unbox ID)])
      (if (box-cas! ID id (+ 1 id))
          id
          (get-next-id)))))

(define futures-enabled? threaded?)

(struct future* (id cond lock prompt
                    would-be? [thunk #:mutable] [engine #:mutable] 
                    [cont #:mutable] [result #:mutable] [done? #:mutable]  
                    [blocked? #:mutable][resumed? #:mutable] 
                    [cond-wait? #:mutable]))

(define (create-future would-be-future?)
  (future* (get-next-id) ;; id
           (future:make-condition) ;; cond
           (make-lock) ;; lock
           (make-continuation-prompt-tag 'future) ;; prompt
           would-be-future? ;; would-be?
           #f   ;; thunk
           #f   ;; engine
           #f   ;; cont
           #f   ;; result
           #f   ;; done?
           #f   ;; blocked?
           #f   ;; resumed?
           #f)) ;; cond-wait?

(define future? future*?)

(define current-future (make-pthread-parameter #f))

(define (current-future-prompt)
  (if (current-future)
      (future*-prompt (current-future))
      (internal-error "Not running in a future.")))

(define (thunk-wrapper f thunk)
  (lambda ()
    (let ([result (thunk)])
      (with-lock ((future*-lock f) (current-future))
        (set-future*-result! f result)
        (set-future*-done?! f #t)
        (future:condition-broadcast (future*-cond f))))))

(define/who (future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (cond
    [(not (futures-enabled?))
     (would-be-future thunk)]
    [else
     (let ([f (create-future #f)])
       (set-future*-engine! f (make-engine (thunk-wrapper f thunk) (future*-prompt f) #f #t))
       (schedule-future f)
       f)]))

(define/who (would-be-future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (let ([f (create-future #t)])
    (set-future*-thunk! f (thunk-wrapper f thunk))
    f))

(define/who (touch f)
  (check who future*? f)
  (cond
    [(future*-done? f)
     (future*-result f)]
    [(future*-would-be? f)
     ((future*-thunk f))
     (future*-result f)]
    [(lock-acquire (future*-lock f) (get-caller) #f) ;; got lock
     (when (or (and (not (future*-blocked? f)) (not (future*-done? f)))
               (and (future*-blocked? f) (not (future*-cont f))))
       (future:condition-wait (future*-cond f) (future*-lock f)))
     (future-awoken f)]
    [else
     (touch f)]))

(define (future-awoken f)
  (cond
    [(future*-done? f) ;; someone else ran continuation
     (lock-release (future*-lock f) (get-caller))
     (future*-result f)]
    [(future*-blocked? f) ;; we need to run continuation
     (set-future*-blocked?! f #f)
     (set-future*-resumed?! f #t)
     (lock-release (future*-lock f) (get-caller))
     ((future*-cont f) '())
     (future*-result f)]
    [else
     (internal-error "Awoken but future is neither blocked nor done.")]))

;; called from chez layer.
(define (future-block)
  (define f (current-future))
  (when (and f (not (future*-blocked? f)) (not (future*-resumed? f)))
    (with-lock ((future*-lock f) f)
      (set-future*-blocked?! f #t))
    (engine-block)))

;; called from chez layer.
;; this should never be called from outside a future.
(define (future-wait)
  (define f (current-future))
  (with-lock ((future*-lock f) f)
    (future:condition-wait (future*-cond f) (future*-lock f))))

;; futures and conditions

(define (wait-future f m)
  (with-lock ((future*-lock f) f)
    (set-future*-cond-wait?! f #t))
  (lock-release m (get-caller))
  (engine-block))

(define (awaken-future f)
  (with-lock ((future*-lock f) (get-caller))
    (set-future*-cond-wait?! f #f)))

;; --------------------------- conditions ------------------------------------

(struct future-condition* (queue lock))

(define (future:make-condition)
  (future-condition* (make-queue) (make-lock)))

(define (future:condition-wait c m)
  (define caller (get-caller))
  (if (own-lock? m caller)
      (begin
        (with-lock ((future-condition*-lock c) caller)
          (queue-add! (future-condition*-queue c) caller))
        (if (future? caller)
            (wait-future caller m)
            (thread-condition-wait (lambda () (lock-release m caller))))
        (lock-acquire m (get-caller))) ;; reaquire lock
      (internal-error "Caller does not hold lock\n")))

(define (signal-future f)
  (future:condition-signal (future*-cond f)))

(define (future:condition-signal c)
  (with-lock ((future-condition*-lock c) (get-caller))
    (let ([waitees (future-condition*-queue c)])
      (unless (queue-empty? waitees)
        (let ([waitee (queue-remove! waitees)])
          (if (future? waitee)
              (awaken-future waitee)
              (thread-condition-awaken waitee)))))))

(define (future:condition-broadcast c)
  (with-lock ((future-condition*-lock c) (get-caller))
    (define waitees '())
    (queue-remove-all! (future-condition*-queue c)
                       (lambda (e)
                         (set! waitees (cons e waitees))))
    (let loop ([q waitees])
      (unless (null? q)
        (let ([waitee (car q)])
          (if (future? waitee)
              (awaken-future waitee)
              (thread-condition-awaken waitee))
          (loop (cdr q)))))))

;; ------------------------------------- future scheduler ----------------------------------------

(define THREAD-COUNT 2)
(define TICKS 1000000000)

(define-place-local global-scheduler #f)
(define (scheduler-running?)
  (not (not global-scheduler)))

(struct worker (id lock mutex cond
                   [queue #:mutable] [idle? #:mutable] 
                   [pthread #:mutable #:auto] [die? #:mutable #:auto])
  #:auto-value #f)

(struct scheduler ([workers #:mutable #:auto])
  #:auto-value #f)

;; I think this atomically is sufficient to guarantee scheduler is only created once.
(define (maybe-start-scheduler)
  (atomically
   (unless global-scheduler
     (set! global-scheduler (scheduler))
     (let ([workers (create-workers)])
       (set-scheduler-workers! global-scheduler workers)
       (start-workers workers)))))

(define (kill-scheduler)
  (when global-scheduler 
    (for-each (lambda (w)
                (with-lock ((worker-lock w) (get-caller))
                  (set-worker-die?! w #t)))
              (scheduler-workers global-scheduler))))

(define (create-workers)
  (let loop ([id 1])
    (cond
      [(< id (+ 1 THREAD-COUNT))
       (cons (worker id (make-lock) (host:make-mutex) (host:make-condition) (make-queue) #t)
             (loop (+ id 1)))]
      [else
       '()])))

;; When a new thread is forked it inherits the values of thread parameters from its creator
;; So, if current-atomic is set for the main thread and then new threads are forked, those new
;; threads current-atomic will be set and then never unset because they will not run code that
;; unsets it.
(define (start-workers workers)
  (for-each (lambda (w)
              (set-worker-pthread! w (fork-pthread (lambda ()
                                                     (current-atomic 0)
                                                     (current-thread #f)
                                                     (current-engine-state #f)
                                                     (current-future #f)
                                                     ((worker-scheduler-func w))))))
            workers))

(define (schedule-future f)
  (maybe-start-scheduler)

  (let ([w (pick-worker)])
    (with-lock ((worker-lock w) (get-caller))
      (host:mutex-acquire (worker-mutex w))
      (queue-add! (worker-queue w) f)
      (host:condition-signal (worker-cond w))
      (host:mutex-release (worker-mutex w)))))

(define (pick-worker)
  (define workers (scheduler-workers global-scheduler))
  (let loop ([workers* (cdr workers)]
             [best (car workers)])
    (cond
      [(or (null? workers*)
           (queue-empty? (worker-queue best)))
       best]
      [(< (queue-length (worker-queue (car workers*)))
          (queue-length (worker-queue best)))
       (loop (cdr workers*)
             (car workers*))]
      [else
       (loop (cdr workers*)
             best)])))

(define (wait-for-work w)
  (define m (worker-mutex w))
  (let try ()
    (cond
      [(not (queue-empty? (worker-queue w))) ;; got work in meantime
       (void)]
      [(host:mutex-acquire m #f) ;; cannot acquire lock while worker is being given work.
       (host:condition-wait (worker-cond w) m)
       (host:mutex-release m)]
      [else ;; try to get lock again.
       (try)])))

(define (worker-scheduler-func worker)
  (lambda ()
    
    (define (loop)
      (lock-acquire (worker-lock worker) (get-pthread-id)) ;; block
      (cond
        [(worker-die? worker) ;; worker was killed
         (lock-release (worker-lock worker) (get-pthread-id))]
        [(queue-empty? (worker-queue worker)) ;; have lock. no work
         (lock-release (worker-lock worker) (get-pthread-id))
         (cond
	   [(steal-work worker)  
	    (do-work)]
	   [else  
	    (wait-for-work worker)])
         (loop)]
        [else  
         (do-work)
         (loop)]))
    
    (define (complete ticks args)
      (void))
    
    (define (expire future worker)
      (lambda (new-eng)
        (set-future*-engine! future new-eng)
        (cond
          [(positive? (current-atomic))
           ((future*-engine future) TICKS (prefix future) complete (expire future worker))]
          [(future*-resumed? future) ;; run to completion
           ((future*-engine future) TICKS void complete (expire future worker))]
          [(not (future*-cont future)) ;; don't want to reschedule future with a saved continuation
           (with-lock ((worker-lock worker) (get-caller))
             (host:mutex-acquire (worker-mutex worker))
             (queue-add! (worker-queue worker) future)
             (host:mutex-release (worker-mutex worker)))]
          [else
           (with-lock ((future*-lock future) (get-caller))
             (future:condition-signal (future*-cond future)))])))
    
    (define (prefix f)
      (lambda ()
        (when (future*-blocked? f)
            (call-with-composable-continuation
             (lambda (k)
               (with-lock ((future*-lock f) (current-future))
                 (set-future*-cont! f k))
               (engine-block))
             (future*-prompt f)))))

    
    ;; need to have lock here.
    (define (do-work)
      (let ([work (queue-remove! (worker-queue worker))])
        (cond
          [(future*-cond-wait? work)
           (queue-add! (worker-queue worker) work)
           (lock-release (worker-lock worker) (get-pthread-id))] ;; put back on queue
          [else
           (lock-release (worker-lock worker) (get-pthread-id))
           (current-future work)
           ((future*-engine work) TICKS (prefix work) complete (expire work worker)) ;; call engine.
           (current-future #f)])))
    
    (loop)))

(define (order-workers w1 w2)
    (cond
     [(< (worker-id w1) (worker-id w2))
      (values w1 w2)]
     [else
      (values w2 w1)]))

  ;; Acquire lock of peer with smallest id # first.
  ;; worker is attempting to steal work from peers
  (define (steal-work worker)
    (let loop ([q (scheduler-workers global-scheduler)])
      (cond
       [(null? q) #f] ;; failed to steal work.
       [(not (eq? (worker-id worker) (worker-id (car q)))) ;; not ourselves
	(let*-values ([(peer) (car q)]
		      [(w1 w2) (order-workers worker peer)]) ;; order them.
	  (lock-acquire (worker-lock w1) (get-pthread-id))
	  (lock-acquire (worker-lock w2) (get-pthread-id))
	  (cond
	   [(> (queue-length (worker-queue peer)) 2) ;; going to steal. Should likely made this # higher.
	    (do ([i (floor (/ (queue-length (worker-queue peer)) 2)) (- i 1)])
		[(zero? i) (void)]
	      (let ([work (queue-remove-end! (worker-queue peer))])
		(queue-add! (worker-queue worker) work)))
	    
	    (lock-release (worker-lock peer) (get-pthread-id)) ;; don't want to release our own lock.
	    #t] ;; stole work
	   [else ;; try a different peer
	    (lock-release (worker-lock worker) (get-pthread-id))
	    (lock-release (worker-lock peer) (get-pthread-id))
	    (loop (cdr q))]))]
       [else (loop (cdr q))])))

;; ----------------------------------------

(define (reset-future-logs-for-tracing!)
  (void))

(define (mark-future-trace-end!)
  (void))
