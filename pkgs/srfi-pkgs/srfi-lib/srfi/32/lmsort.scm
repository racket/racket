(module lmsort mzscheme
  (provide list-merge
           #;list-merge!
           list-merge-sort
           #;list-merge-sort!)
  
  ;;; list merge & list merge-sort	-*- Scheme -*-
  ;;; Copyright (c) 1998 by Olin Shivers.
  ;;; This code is open-source; see the end of the file for porting and
  ;;; more copyright information.
  ;;; Olin Shivers
  
  ;;; Exports:
  ;;; (list-merge  < lis lis) -> list
  ;;; (list-merge! < lis lis) -> list
  ;;; (list-merge-sort  < lis) -> list
  ;;; (list-merge-sort! < lis) -> list
  
  ;;; A stable list merge sort of my own device
  ;;; Two variants: pure & destructive
  ;;;
  ;;; This list merge sort is opportunistic (a "natural" sort) -- it exploits
  ;;; existing order in the input set. Instead of recursing all the way down to
  ;;; individual elements, the leaves of the merge tree are maximal contiguous
  ;;; runs of elements from the input list. So the algorithm does very well on
  ;;; data that is mostly ordered, with a best-case time of O(n) when the input
  ;;; list is already completely sorted. In any event, worst-case time is
  ;;; O(n lg n).
  ;;;
  ;;; The destructive variant is "in place," meaning that it allocates no new
  ;;; cons cells at all; it just rearranges the pairs of the input list with
  ;;; SET-CDR! to order it.
  ;;;
  ;;; The interesting control structure is the combination recursion/iteration
  ;;; of the core GROW function that does an "opportunistic" DFS walk of the
  ;;; merge tree, adaptively subdividing in response to the length of the
  ;;; merges, without requiring any auxiliary data structures beyond the
  ;;; recursion stack. It's actually quite simple -- ten lines of code.
  ;;;	-Olin Shivers 10/20/98
  
  ;;; (mlet ((var-list mv-exp) ...) body ...)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; A LET* form that handles multiple values. Move this into the two clients
  ;;; if you don't have a module system handy to restrict its visibility...
  (define-syntax mlet ; Multiple-value LET*
    (syntax-rules ()
      ((mlet ((() exp) rest ...) body ...)
       (begin exp (mlet (rest ...) body ...)))
      
      ((mlet (((var) exp) rest ...) body ...)
       (let ((var exp)) (mlet (rest ...) body ...)))
      
      ((mlet ((vars exp) rest ...) body ...)
       (call-with-values (lambda () exp) 
                         (lambda vars (mlet (rest ...) body ...))))
      
      ((mlet () body ...) (begin body ...))))
  
  
  ;;; (list-merge-sort < lis)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; A natural, stable list merge sort. 
  ;;; - natural: picks off maximal contiguous runs of pre-ordered data.
  ;;; - stable: won't invert the order of equal elements in the input list.
  
  (define (list-merge-sort elt< lis)
    
    ;; (getrun lis) -> run runlen rest
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Pick a run of non-decreasing data off of non-empty list LIS. 
    ;; Return the length of this run, and the following list.
    (define (getrun lis)
      (let lp ((ans '())  (i 1)  (prev (car lis))  (xs (cdr lis)))
        (if (pair? xs)
            (let ((x (car xs)))
              (if (elt< x prev) 
                  (values (append-reverse ans (cons prev '())) i xs)
                  (lp (cons prev ans) (+ i 1) x (cdr xs))))
            (values (append-reverse ans (cons prev '())) i xs))))
    
    (define (append-reverse rev-head tail)
      (let lp ((rev-head rev-head) (tail tail))
        (if (null-list? rev-head) tail
            (lp (cdr rev-head) (cons (car rev-head) tail)))))
    
    (define (null-list? l)
      (cond ((pair? l) #f)
            ((null? l) #t)
            (else (error "null-list?: argument out of domain" l))))
    
    ;; (merge a b) -> list
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; List merge -- stably merge lists A (length > 0) & B (length > 0).
    ;; This version requires up to |a|+|b| stack frames.
    (define (merge a b)
      (let recur ((x (car a)) (a a)
                              (y (car b)) (b b))
        (if (elt< y x)
            (cons y (let ((b (cdr b)))
                      (if (pair? b)
                          (recur x a (car b) b)
                          a)))
            (cons x (let ((a (cdr a)))
                      (if (pair? a)
                          (recur (car a) a y b)
                          b))))))
    
    ;; (grow s ls ls2 u lw) -> [a la unused]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; The core routine. Read the next 20 lines of comments & all is obvious.
    ;; - S is a sorted list of length LS > 1.
    ;; - LS2 is some power of two <= LS.
    ;; - U is an unsorted list.
    ;; - LW is a positive integer.
    ;; Starting with S, and taking data from U as needed, produce
    ;; a sorted list of *at least* length LW, if there's enough data
    ;; (LW <= LS + length(U)), or use all of U if not.
    ;;
    ;; GROW takes maximal contiguous runs of data from U at a time;
    ;; it is allowed to return a list *longer* than LW if it gets lucky
    ;; with a long run.
    ;;
    ;; The key idea: If you want a merge operation to "pay for itself," the two
    ;; lists being merged should be about the same length. Remember that.
    ;;
    ;; Returns:
    ;;   - A:      The result list
    ;;   - LA:     The length of the result list
    ;;   - UNUSED: The unused tail of U.
    
    (define (grow s ls ls2 u lw)	; The core of the sort algorithm.
      (if (or (<= lw ls) (not (pair? u)))	; Met quota or out of data?
          (values s ls u)			; If so, we're done.
          (mlet (((ls2) (let lp ((ls2 ls2))
                          (let ((ls2*2 (+ ls2 ls2)))
                            (if (<= ls2*2 ls) (lp ls2*2) ls2))))
                 ;; LS2 is now the largest power of two <= LS.
                 ;; (Just think of it as being roughly LS.)
                 ((r lr u2)  (getrun u))			; Get a run, then
                 ((t lt u3)  (grow r lr 1 u2 ls2))) 	; grow it up to be T.
                (grow (merge s t) (+ ls lt)	 		; Merge S & T, 
                      (+ ls2 ls2) u3 lw))))	     		;   and loop.
    
    ;; Note: (LENGTH LIS) or any constant guaranteed 
    ;; to be greater can be used in place of INFINITY.
    (if (pair? lis)				; Don't sort an empty list.
        (mlet (((r lr tail)  (getrun lis))	; Pick off an initial run,
               ((infinity)   #o100000000)		; then grow it up maximally.
               ((a la v)     (grow r lr 1 tail infinity)))
              a)
        '()))
  
  
  ;;; (list-merge-sort! < lis)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; A natural, stable, destructive, in-place list merge sort. 
  ;;; - natural: picks off maximal contiguous runs of pre-ordered data.
  ;;; - stable: won't invert the order of equal elements in the input list.
  ;;; - destructive, in-place: this routine allocates no extra working memory; 
  ;;;   it simply rearranges the list with SET-CDR! operations.
  
  #;
  (define (list-merge-sort! elt< lis)
    ;; (getrun lis) -> runlen last rest
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Pick a run of non-decreasing data off of non-empty list LIS. 
    ;; Return the length of this run, the last cons cell of the run,
    ;; and the following list.
    (define (getrun lis)
      (let lp ((lis lis) (x (car lis)) (i 1) (next (cdr lis)))
        (if (pair? next)
            (let ((y (car next)))
              (if (elt< y x) 
                  (values i lis next)
                  (lp next y (+ i 1) (cdr next))))
            (values i lis next))))
    
    ;; (merge! a enda b endb) -> [m endm]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Destructively and stably merge non-empty lists A & B.
    ;; The last cons of A is ENDA. (The cdr of ENDA can be non-nil.)
    ;; the last cons of B is ENDB. (The cdr of ENDB can be non-nil.)
    ;;
    ;; Return the first and last cons cells of the merged list.
    ;; This routine is iterative & in-place: it runs in constant stack and 
    ;; doesn't allocate any cons cells. It is also tedious but simple; don't
    ;; bother reading it unless necessary.
    (define (merge! a enda b endb)
      ;; The logic of these two loops is completely driven by these invariants:
      ;;   SCAN-A: (CDR PREV) = A. X = (CAR A). Y = (CAR B).
      ;;   SCAN-B: (CDR PREV) = B. X = (CAR A). Y = (CAR B).
      (letrec ((scan-a (lambda (prev  x a  y b)     ; Zip down A until we
                         (cond ((elt< y x)          ; find an elt > (CAR B).
                                (set-cdr! prev b)
                                (let ((next-b (cdr b)))
                                  (if (eq? b endb)
                                      (begin (set-cdr! b a) enda) ; Done.
                                      (scan-b b x a (car next-b) next-b))))
                               
                               ((eq? a enda) (maybe-set-cdr! a b) endb) ; Done.
                               
                               (else (let ((next-a (cdr a)))  ; Continue scan.
                                       (scan-a a (car next-a) next-a y b))))))
               
               (scan-b (lambda (prev  x a  y b)     ; Zip down B while its
                         (cond ((elt< y x)          ; elts are < (CAR A).
                                (if (eq? b endb) 
                                    (begin (set-cdr! b a) enda)      ; Done.
                                    (let ((next-b (cdr b))) ; Continue scan.
                                      (scan-b b x a (car next-b) next-b))))
                               
                               (else (set-cdr! prev a)
                                     (if (eq? a enda) 
                                         (begin (maybe-set-cdr! a b) endb) ; Done.
                                         (let ((next-a (cdr a)))
                                           (scan-a a (car next-a) next-a y b)))))))
               
               ;; This guy only writes if he has to. Called at most once.
               ;; Pointer equality rules; pure languages are for momma's boys.
               (maybe-set-cdr! (lambda (pair val) (if (not (eq? (cdr pair) val)) 
                                                      (set-cdr! pair val)))))
        
        (let ((x (car a))  (y (car b)))
          (if (elt< y x)
              
              ;; B starts the answer list.
              (values b (if (eq? b endb)
                            (begin (set-cdr! b a) enda)
                            (let ((next-b (cdr b)))
                              (scan-b b x a (car next-b) next-b))))
              
              ;; A starts the answer list.
              (values a (if (eq? a enda) 
                            (begin (maybe-set-cdr! a b) endb)
                            (let ((next-a (cdr a)))
                              (scan-a a (car next-a) next-a y b))))))))
    
    ;; (grow s ends ls ls2 u lw) -> [a enda la unused]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; The core routine.
    ;; - S is a sorted list of length LS > 1, with final cons cell ENDS.
    ;;   (CDR ENDS) doesn't have to be nil.
    ;; - LS2 is some power of two <= LS.
    ;; - U is an unsorted list. 
    ;; - LW is a positive integer.
    ;; Starting with S, and taking data from U as needed, produce
    ;; a sorted list of *at least* length LW, if there's enough data
    ;; (LW <= LS + length(U)), or use all of U if not.
    ;;
    ;; GROW takes maximal contiguous runs of data from U at a time;
    ;; it is allowed to return a list *longer* than LW if it gets lucky
    ;; with a long run.
    ;;
    ;; The key idea: If you want a merge operation to "pay for itself," the two
    ;; lists being merged should be about the same length. Remember that.
    ;; 
    ;; Returns:
    ;;   - A:      The result list (not properly terminated)
    ;;   - ENDA:   The last cons cell of the result list.
    ;;   - LA:     The length of the result list
    ;;   - UNUSED: The unused tail of U.
    (define (grow s ends ls ls2 u lw)
      (if (and (pair? u) (< ls lw))
          
          ;; We haven't met the LW quota but there's still some U data to use.
          (mlet (((ls2) (let lp ((ls2 ls2))
                          (let ((ls2*2 (+ ls2 ls2)))
                            (if (<= ls2*2 ls) (lp ls2*2) ls2))))
                 ;; LS2 is now the largest power of two <= LS.
                 ;; (Just think of it as being roughly LS.)
                 ((lr endr u2)   (getrun u))		  ; Get a run from U;
                 ((t endt lt u3) (grow u endr lr 1 u2 ls2)) ; grow it up to be T.
                 ((st end-st)    (merge! s ends t endt)))	  ; Merge S & T,
                (grow st end-st (+ ls lt) (+ ls2 ls2) u3 lw))	  ; then loop.
          
          (values s ends ls u))) ; Done -- met LW quota or ran out of data.
    
    ;; Note: (LENGTH LIS) or any constant guaranteed
    ;; to be greater can be used in place of INFINITY.
    (if (pair? lis)
        (mlet (((lr endr rest)  (getrun lis))	; Pick off an initial run.
               ((infinity)      #o100000000)	; Then grow it up maximally.
               ((a enda la v)   (grow lis endr lr 1 rest infinity)))
              (set-cdr! enda '())			; Nil-terminate answer.
              a)					; We're done.
        
        '()))					; Don't sort an empty list.
  
  
  ;;; Merge
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; These two merge procedures are stable -- ties favor list A.
  
  (define (list-merge < a b)
    (cond ((not (pair? a)) b)
          ((not (pair? b)) a)
          (else (let recur ((x (car a)) (a a)	; A is a pair; X = (CAR A).
                                        (y (car b)) (b b))	; B is a pair; Y = (CAR B).
                  (if (< y x)
                      
                      (let ((b (cdr b)))
                        (if (pair? b)
                            (cons y (recur x a (car b) b))
                            (cons y a)))
                      
                      (let ((a (cdr a)))
                        (if (pair? a)
                            (cons x (recur (car a) a y b))
                            (cons x b))))))))
  
  
  ;;; This destructive merge does as few SET-CDR!s as it can -- for example, if
  ;;; the list is already sorted, it does no SET-CDR!s at all. It is also
  ;;; iterative, running in constant stack.
  
  #;
  (define (list-merge! < a b)
    ;; The logic of these two loops is completely driven by these invariants:
    ;;   SCAN-A: (CDR PREV) = A. X = (CAR A). Y = (CAR B).
    ;;   SCAN-B: (CDR PREV) = B. X = (CAR A). Y = (CAR B).
    (letrec ((scan-a (lambda (prev a x b y)		; Zip down A doing
                       (if (< y x)			; no SET-CDR!s until
                           (let ((next-b (cdr b)))	; we hit a B elt that
                             (set-cdr! prev b)		; has to be inserted.
                             (if (pair? next-b)
                                 (scan-b b a x next-b (car next-b))
                                 (set-cdr! b a)))
                           
                           (let ((next-a (cdr a)))
                             (if (pair? next-a)
                                 (scan-a a next-a (car next-a) b y)
                                 (set-cdr! a b))))))
             
             (scan-b (lambda (prev a x b y)		; Zip down B doing
                       (if (< y x)			; no SET-CDR!s until 
                           (let ((next-b (cdr b)))	; we hit an A elt that
                             (if (pair? next-b)			  ; has to be
                                 (scan-b b a x next-b (car next-b)) ; inserted.
                                 (set-cdr! b a))) 
                           
                           (let ((next-a (cdr a)))
                             (set-cdr! prev a)
                             (if (pair? next-a)
                                 (scan-a a next-a (car next-a) b y)
                                 (set-cdr! a b)))))))
      
      (cond ((not (pair? a)) b)
            ((not (pair? b)) a)
            
            ;; B starts the answer list.
            ((< (car b) (car a))
             (let ((next-b (cdr b)))
               (if (null? next-b)
                   (set-cdr! b a)
                   (scan-b b a (car a) next-b (car next-b))))
             b)
            
            ;; A starts the answer list.
            (else (let ((next-a (cdr a)))
                    (if (null? next-a)
                        (set-cdr! a b)
                        (scan-a a next-a (car next-a) b (car b))))
                  a))))
  
  
  ;;; Copyright
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; This code is
  ;;;     Copyright (c) 1998 by Olin Shivers.
  ;;; The terms are: You may do as you please with this code, as long as
  ;;; you do not delete this notice or hold me responsible for any outcome
  ;;; related to its use.
  ;;;
  ;;; Blah blah blah.
  
  
  ;;; Code tuning & porting
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; This is very portable code. It's R4RS with the following exceptions:
  ;;; - The R5RS multiple-value VALUES & CALL-WITH-VALUES procedures for
  ;;;   handling multiple-value return.
  ;;;
  ;;; This code is *tightly* bummed as far as I can go in portable Scheme.
  ;;;
  ;;; - The fixnum arithmetic in LIST-MERGE-SORT! and COUNTED-LIST-MERGE!
  ;;;    that could be safely switched over to unsafe, fixnum-specific ops,
  ;;;    if you're sure that 2*maxlen is a fixnum, where maxlen is the length
  ;;;    of the longest list you could ever have.
  ;;;
  ;;; - I typically write my code in a style such that every CAR and CDR 
  ;;;   application is protected by an upstream PAIR?. This is the case in this
  ;;;   code, so all the CAR's and CDR's could safely switched over to unsafe
  ;;;   versions. But check over the code before you do it, in case the source
  ;;;   has been altered since I wrote this.
  )
