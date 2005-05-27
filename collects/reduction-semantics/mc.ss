
(module mc mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
	   (lib "generator.ss" "reduction-semantics")
           (prefix matcher: (lib "matcher.ss" "reduction-semantics" "private"))
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide build-reductions 
           generation-depth
           reduction-depth
           generate-and-test
           (struct reduction-graph (initial ht)))
  
  ;; reduction-graph = (make-graph sexp hash-table[sexp -o> (listof sexp)])
  (define-struct reduction-graph (initial ht))
  
  (define generation-depth (make-parameter 2))
  (define reduction-depth (make-parameter 10))
  (define max-queue-size 100)
  
  ;; computes all of the terms of size `n' or smaller for each non-terminal
  ;; in the language
  ;; [ sizes are limited by number of recusive constructions of each non terminal;
  ;;   4 is huge for even tiny languages ]
  (define (generate-and-test lang nt reductions reductions-test)
    
    (define frame (make-object frame% "Status"))
    (define gc-on-bitmap (make-object bitmap% (build-path (collection-path "icons") "recycle.gif")))
    (define gc-off-bitmap (make-object bitmap% 
                            (send gc-on-bitmap get-width)
                            (send gc-on-bitmap get-height)
                            #t))
    (define stupid-internal-define-syntax1
      (let ([bdc (make-object bitmap-dc% gc-off-bitmap)])
        (send bdc clear)
        (send bdc set-bitmap #f)))
    
    (define-values (reductions-queue-size-message 
                    total-reductions-message
                    test-queue-size-message
                    total-tests-message
                    memory-allocated-message)
      (let* ([outer-hp (make-object horizontal-panel% frame)]
             [outer-vp (make-object vertical-panel% outer-hp)]
             [hp1 (make-object horizontal-panel% outer-vp)]
             [hp2 (make-object horizontal-panel% outer-vp)]
             [hp3 (make-object horizontal-panel% outer-vp)]
             [hp4 (make-object horizontal-panel% outer-vp)]
             [hp5 (make-object horizontal-panel% outer-vp)]
             [l1 (make-object message% "To Reduce: " hp1)]
             [l2 (make-object message% "Total Expressions: " hp2)]
             [l3 (make-object message% "To Test: " hp3)]
             [l4 (make-object message% "Total Tests: " hp4)]
             [l5 (make-object message% "Megabytes Allocated: " hp5)]
             [gc-canvas (instantiate canvas% ()
                          (parent outer-hp)
                          (stretchable-width #f)
                          (stretchable-height #f)
                          (min-width (send gc-on-bitmap get-width))
                          (min-height (send gc-on-bitmap get-height)))]
	     [dms-button (instantiate button% ("DMS" outer-hp)
				      [callback (lambda (b e)
						  (dump-memory-stats '<struct>))])])

        (register-collecting-blit gc-canvas
                                  0
                                  0 
                                  (send gc-on-bitmap get-width)
                                  (send gc-on-bitmap get-height)
                                  gc-on-bitmap
                                  gc-off-bitmap)
        (let ([w (max (send l1 get-width)
                      (send l2 get-width)
                      (send l3 get-width)
                      (send l4 get-width)
                      (send l5 get-width))])
          (send l1 min-width w)
          (send l2 min-width w)
          (send l3 min-width w)
          (send l4 min-width w)
          (send l5 min-width w))
        
        (values
         (instantiate message% ()
           (label "0000000")
           ;(stretchable-width #t)
           (parent hp1))
         (instantiate message% ()
           (label "0000000")
           ;(stretchable-width #t)
           (parent hp2))
         (instantiate message% ()
           (label "0000000")
           ;(stretchable-width #t)
           (parent hp3))
         (instantiate message% ()
           (label "0000000")
           ;(stretchable-width #t)
           (parent hp4))
         (instantiate message% ()
           (label "0000000")
           ;(stretchable-width #t)
           (parent hp5)))))
   
    (define go (make-semaphore 0))
    
    (define no-more-terms (box 'no-more-terms))

    (define generation-thread
      (thread
       (lambda ()
         (semaphore-wait go)
         (generate lang nt enqueue-for-reduction-thread)
         (enqueue-for-reduction-thread no-more-terms))))
    
    (define total-reductions 0)
    (define reduction-queue (new-queue))
    (define reduction-queue-sema (make-semaphore 1))
    (define reduction-thread-sema (make-semaphore 0))
    (define reduction-producer-sema (make-semaphore max-queue-size))
    (define (enqueue-for-reduction-thread sexp)
      (semaphore-wait reduction-producer-sema)
      (semaphore-wait reduction-queue-sema)
      (enqueue reduction-queue sexp)
      (set! total-reductions (+ total-reductions 1))
      (semaphore-post reduction-thread-sema)
      (semaphore-post reduction-queue-sema))
    
    (define reduction-thread
      (thread
       (lambda ()
         (semaphore-wait go)
         (let loop ()
           (semaphore-wait reduction-thread-sema)
           (semaphore-wait reduction-queue-sema)
           (let ([sexp (dequeue reduction-queue)])
             (semaphore-post reduction-queue-sema)
             (semaphore-post reduction-producer-sema)
             (cond
               [(eq? sexp no-more-terms)
                (enqueue-for-test-thread no-more-terms)]
               [else
                (enqueue-for-test-thread (build-reductions sexp reductions))
                (loop)]))))))

    (define total-tests 0)
    (define test-queue (new-queue))
    (define test-queue-sema (make-semaphore 1))
    (define test-thread-sema (make-semaphore 0))
    (define test-producer-sema (make-semaphore max-queue-size))
    (define (enqueue-for-test-thread sexp)
      (semaphore-wait test-producer-sema)
      (semaphore-wait test-queue-sema)
      (enqueue test-queue sexp)
      (set! total-tests (+ total-tests 1))
      (semaphore-post test-thread-sema)
      (semaphore-post test-queue-sema))
    
    (define test-thread
      (thread
       (lambda ()
         (semaphore-wait go)
         (let loop ()
           (semaphore-wait test-thread-sema)
           (semaphore-wait test-queue-sema)
           (let ([reds (dequeue test-queue)])
             (semaphore-post test-queue-sema)
             (semaphore-post test-producer-sema)
             (cond
               [(eq? reds no-more-terms)
                (semaphore-post done-semaphore)]
               [else
                (reductions-test reds)]))
           (loop)))))
    
    (define mem-divisor (* 1024 1024))
    (define (update-status)
      (send test-queue-size-message set-label (format "~a" (queue-size test-queue)))
      (send total-tests-message set-label (format "~a" total-tests))
      (send reductions-queue-size-message set-label (format "~a" (queue-size reduction-queue)))
      (send total-reductions-message set-label (format "~a" total-reductions))
      (send memory-allocated-message set-label (number->string (quotient (current-memory-use) mem-divisor))))

    (define status-thread
      (thread
       (lambda ()
         (semaphore-wait go)
         (with-handlers ([exn:break? (lambda (x) (semaphore-post status-thread-done))])
           (let loop ()
             (update-status)
             (sleep 2)
             (loop))))))
    
    (define done-semaphore (make-semaphore 0))
    (define status-thread-done (make-semaphore 0))
    
    (send frame show #t)
    (semaphore-post go)
    (semaphore-post go)
    (semaphore-post go)
    (semaphore-post go)
    (yield done-semaphore)
    (break-thread status-thread)
    (semaphore-wait status-thread-done)
    (update-status)
    (make-object message% "Done." frame))

  
  
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                    ;                      ;                                                   ;      
;                    ;                                                                          ;      
;                    ;                 ;                                                        ;      
;   ; ;;  ;;;    ;;; ;  ;    ;   ;;;; ;;;;  ;    ;;;;   ; ;;;        ;;; ;  ; ;;  ;;;   ; ;;;   ; ;;;  
;   ;;   ;   ;  ;   ;;  ;    ;  ;      ;    ;   ;    ;  ;;   ;      ;   ;;  ;;   ;   ;  ;;   ;  ;;   ; 
;   ;    ;   ;  ;    ;  ;    ;  ;      ;    ;   ;    ;  ;    ;      ;    ;  ;        ;  ;    ;  ;    ; 
;   ;    ;;;;;  ;    ;  ;    ;  ;      ;    ;   ;    ;  ;    ;      ;    ;  ;     ;;;;  ;    ;  ;    ; 
;   ;    ;      ;    ;  ;    ;  ;      ;    ;   ;    ;  ;    ;      ;    ;  ;    ;   ;  ;    ;  ;    ; 
;   ;    ;      ;   ;;  ;   ;;  ;      ;    ;   ;    ;  ;    ;      ;   ;;  ;    ;   ;  ;;   ;  ;    ; 
;   ;     ;;;;   ;;; ;   ;;; ;   ;;;;   ;;  ;    ;;;;   ;    ;       ;;; ;  ;     ;;;;; ; ;;;   ;    ; 
;                                                                        ;              ;              
;                                                                   ;    ;              ;              
;                                                                    ;;;;               ;              

  
  ;; build-reductions : sexp (listof reductions) -> reduction-graph 
  ;; builds the reduction graph for expression according to reductions
  (define (build-reductions expression reductions)
    (let* ([ht (make-hash-table 'equal)]
           [reduce/add
            (lambda (term)
              (let ([reduced (reduce reductions term)])
                (hash-table-put! ht term reduced)
                (filter (lambda (term)
                          (not (hash-table-get ht term (lambda () #f))))
                        reduced)))])
      (let loop ([frontier (list expression)]
                 [depth (reduction-depth)])
        (unless (zero? depth)
          (let* ([new-terms (apply append (map reduce/add frontier))])
            (cond
              [(null? new-terms) (void)]
              [else
               (loop new-terms (- depth 1))]))))
      (make-reduction-graph expression ht)))
  
  
  
  
;                                                                     
;                                                                     
;                                                                     
;                                                  ;                  
;                                                                     
;                                             ;                       
;    ;;; ;   ;;;   ; ;;;    ;;;   ; ;;  ;;;  ;;;;  ;    ;;;;   ; ;;;  
;   ;   ;;  ;   ;  ;;   ;  ;   ;  ;;   ;   ;  ;    ;   ;    ;  ;;   ; 
;   ;    ;  ;   ;  ;    ;  ;   ;  ;        ;  ;    ;   ;    ;  ;    ; 
;   ;    ;  ;;;;;  ;    ;  ;;;;;  ;     ;;;;  ;    ;   ;    ;  ;    ; 
;   ;    ;  ;      ;    ;  ;      ;    ;   ;  ;    ;   ;    ;  ;    ; 
;   ;   ;;  ;      ;    ;  ;      ;    ;   ;  ;    ;   ;    ;  ;    ; 
;    ;;; ;   ;;;;  ;    ;   ;;;;  ;     ;;;;;  ;;  ;    ;;;;   ;    ; 
;        ;                                                            
;   ;    ;                                                            
;    ;;;;                                                             

  
  
  ;; generate : lang sexp (sexp -> void) -> void
  ;; generates the terms up to (generation-depth) in size
  ;; passes each that comes from gdesired-nt to enqueue-for-reduction-thread
  (define (generate lang desired-nt enqueue-for-reduction-thread)
    (let ([gens (lang->generator-table lang
				       '(0 1)
				       '(x y)
				       '("a" "b")
				       null
				       0)])
      (let loop ([n 0])
	(unless (n . > . (generation-depth))
	  (for-each-generated/size (lambda (sexp size)
				     (enqueue-for-reduction-thread sexp))
				   gens 
				   n n desired-nt)
	  (loop (add1 n))))))
  

  ;; find-interesting-nts : (listof nt) sym -> (listof sym)
  (define (find-interesting-nts clang desired-nt)
    (let* ([lang (matcher:compiled-lang-lang clang)]
           [ht (make-hash-table)]
           [nt-syms (map matcher:nt-name lang)])
      (let loop ([nt-sym desired-nt])
        (let ([nt-lst (filter (lambda (x) (eq? (matcher:nt-name x) nt-sym)) lang)])
          (cond
            [(null? nt-lst) (void)]
            [(null? (cdr nt-lst)) 
             (let ([referenced-nt-syms (get-referenced-nts nt-syms (car nt-lst) lang)])
               (for-each
                (lambda (referenced-nt-sym)
                  (unless (hash-table-get ht referenced-nt-sym (lambda () #f))
                    (hash-table-put! ht referenced-nt-sym #t)
                    (loop referenced-nt-sym)))
                referenced-nt-syms))]
            [else (error 'mc.ss "found more than one definition of ~s in grammar" nt-sym)])))
      (hash-table-map ht (lambda (x y) x))))
  
  (define (get-referenced-nts nt-syms nt lang)
    (let loop ([rhss (matcher:nt-rhs nt)]
               [refd-nts null])
      (cond
        [(null? rhss) refd-nts]
        [else (loop (cdr rhss)
                    (get-referenced-nts/rhs nt-syms (car rhss) refd-nts))])))
  
  (define (get-referenced-nts/rhs nt-syms rhs acc)
    (let loop ([pat (matcher:rhs-pattern rhs)]
               [acc acc])
      (cond
        [(null? pat) acc]
        [(pair? pat) (loop (car pat) (loop (cdr pat) acc))]
        [(symbol? pat) 
         (if (memq pat nt-syms)
             (cons pat acc)
             acc)])))
              
        
        
  
;                                        
;                                        
;                                        
;                                        
;                                        
;                                        
;    ;;; ;  ;    ;   ;;;   ;    ;   ;;;  
;   ;   ;;  ;    ;  ;   ;  ;    ;  ;   ; 
;   ;    ;  ;    ;  ;   ;  ;    ;  ;   ; 
;   ;    ;  ;    ;  ;;;;;  ;    ;  ;;;;; 
;   ;    ;  ;    ;  ;      ;    ;  ;     
;   ;   ;;  ;   ;;  ;      ;   ;;  ;     
;    ;;; ;   ;;; ;   ;;;;   ;;; ;   ;;;; 
;        ;                               
;        ;                               
;        ;                               


  
  (define-struct queue (hd tl size))
  (define (new-queue) (make-queue null null 0))
  (define (enqueue queue thnk)
    (set-queue-size! queue (+ (queue-size queue) 1))
    (let ([new-tail (cons thnk null)])
      (if (null? (queue-hd queue))
          (begin
            (set-queue-hd! queue new-tail)
            (set-queue-tl! queue new-tail))
          (begin
            (set-cdr! (queue-tl queue) new-tail)
            (set-queue-tl! queue new-tail)))))
  (define (dequeue queue)
    (when (null? (queue-hd queue))
      (error 'dequeue))
    (set-queue-size! queue (- (queue-size queue) 1))
    (let* ([qh (queue-hd queue)]
	   [fst (car qh)])
      (set-queue-hd! queue (cdr qh))
      (set-cdr! qh #f)
      (when (null? (queue-hd queue))
        (set-queue-tl! queue null))
      fst))
  (define (queue-empty? queue) (null? (queue-hd queue))))
