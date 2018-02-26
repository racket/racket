
(load-relative "loadtest.rktl")

(Section 'wills)

(collect-garbage 'major)
(collect-garbage 'minor)
(err/rt-test (collect-garbage 'other))

(test #t exact-nonnegative-integer? (current-memory-use))
(test #t exact-nonnegative-integer? (current-memory-use #f))
(test #t exact-nonnegative-integer? (current-memory-use (current-custodian)))

(test #f will-executor? 5)
(test #t will-executor? (make-will-executor))

(define we (make-will-executor))

(test #f will-try-execute we)
(test 'no will-try-execute we 'no)

;; Never GC this one:
(test (void) will-register we test (lambda (x) (error 'bad-will-call)))

; There's no excuse for not GCing half or more:
(define counter null)
(let loop ([n 10])
  (unless (zero? n)
    (will-register we (cons n null)
		   (lambda (s)
		     (set! counter (cons (car s) counter))
		     12))
    (loop (sub1 n))))
(collect-garbage)
(collect-garbage)
(let* ([v #f]
       [t (thread (lambda () (set! v (will-execute we))))])
  (sleep 0.1)
  (test #f thread-running? t)
  (test v values 12))
(let loop ([m 1])
  (if (let ([v (will-try-execute we)])
	(test #t 'good-result (or (not v) (= v 12)))
	v)
      (loop (add1 m))
      (begin
	(test #t >= m 5)
	;; Make sure counter grew ok
	(test m length counter)
	;; Make sure they're all different
	(let loop ([l counter])
	  (unless (or (null? l) (null? (cdr l)))
	    (test #f member (car l) (cdr l))
	    (loop (cdr l)))))))

(err/rt-test (will-register we we we))
(err/rt-test (will-register we we (lambda () 10)))
(err/rt-test (will-register 5 we (lambda (s) 10)))

(err/rt-test (will-execute "bad"))
(err/rt-test (will-try-execute "bad"))

(arity-test make-will-executor 0 0)
(arity-test will-executor? 1 1)
(arity-test will-register 3 3)
(arity-test will-execute 1 1)
(arity-test will-try-execute 1 2)

;; ----------------------------------------
;; Test custodian boxes

(let ([c (make-custodian)]
      [we (make-will-executor)]
      [removed null])
  (let ([mk-finalized (lambda (n)
                        (let ([l (list n)])
                          (will-register we l (lambda (v)
                                                (set! removed (cons (car v) removed))))
                          (make-custodian-box c l)))]
        [gc (lambda ()
              (collect-garbage)
              (collect-garbage)
              (let loop ()
                (when (will-try-execute we)
                  (loop)))
              (collect-garbage)
              (collect-garbage))]
        [b1 (make-custodian-box c 12)])
    (test #f sync/timeout 0 b1)
    (let ([saved (map mk-finalized '(a b c d e f g h i))])
      (let loop ([m 2])
        (unless (zero? m)
          (set! removed null)
          (let loop ([n 100])
            (unless (zero? n)
              (mk-finalized n)
              (loop (sub1 n))))
          (gc)
          ;; finalize at least half?
          (test #t > (length removed) 50)
          (test #f ormap symbol? removed)
          (test 12 custodian-box-value b1)
          (loop (sub1 m))))
      (test #t andmap (lambda (x) (and (pair? x) (symbol? (car x))))
            (map custodian-box-value saved))
      (set! removed null)
      (custodian-shutdown-all c)
      (test #f custodian-box-value b1)
      (test b1 sync/timeout 0 b1)
      (test #f ormap values (map custodian-box-value saved))
      (gc)
      (test #t <= 5 (apply + (map (lambda (v) (if (symbol? v) 1 0)) removed))))))

(when (custodian-memory-accounting-available?)
  ;; Check custodian boxes for accounting
  (let* ([c (map (lambda (n) (make-custodian))
                 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))]
         [b (map (lambda (c)
                   (make-custodian-box c (make-bytes 100000)))
                 c)]
         [t (map (lambda (c)
                   ;; Each thread can reach all boxes:
                   (parameterize ([current-custodian c])
                     (thread (lambda () (sync (make-semaphore)) b))))
                 c)])
    ;; Each custodian must be charged at least 100000 bytes:
    (collect-garbage)
    (test #t andmap (lambda (c)
                      ((current-memory-use c) . >= . 100000))
          c)))

(let ()
  (define c1 (make-custodian (current-custodian)))
  (define b1 (make-custodian-box c1 #t))
  (define c2 (make-custodian c1))
  (define b2 (make-custodian-box c2 #t))
  (test '(#t #t) map custodian-box-value (list b1 b2))
  (custodian-shutdown-all c1)
  (test '(#f #f) map custodian-box-value (list b1 b2)))

(let ()
  (let ([c (make-custodian)])
    (let ([l (for/list ([i (in-range 32)])
               (make-custodian-box c 7))])
      (test #t andmap (lambda (b) (number? (custodian-box-value b))) l)
      (custodian-shutdown-all c)
      (test #f ormap (lambda (b) (number? (custodian-box-value b))) l))))

;; check synchronization again:
(let ()
  (define done #f)
  (define c1 (make-custodian (current-custodian)))
  (define b1 (make-custodian-box c1 #t))
  (thread (lambda () (sync b1) (set! done #t)))
  (sync (system-idle-evt))
  (test #f values done)
  (custodian-shutdown-all c1)
  (sync (system-idle-evt))
  (test #t values done))
     
;; ----------------------------------------

(let ([something (gensym)])
  (define e-chain
    (let loop ([n 100] [e #f])
      (if (zero? n)
          e
          (loop (sub1 n)
                (make-ephemeron something e)))))
  (collect-garbage)
  (test 100 'epehemeron-chain
        (let loop ([e e-chain])
          (if e
              (add1 (loop (ephemeron-value e)))
              0)))
  ;; ensure that `something' is retained:
  (test #t symbol? (list-ref (list something) (random 1))))
     
;; ----------------------------------------
;; `weak-box-value' and `ephemeron-value' optional result:

(let ()
  (define stuff
    (for/list ([n 100])
      (cons (make-weak-box (gensym))
            (make-ephemeron (gensym) 10))))
  (define (num-or a b) (if (number? a) a b))
  (collect-garbage)
  (define n (for/fold ([n 0]) ([p stuff])
              (+ n
                   (num-or (weak-box-value (car p) 0) 1)
                   (num-or (ephemeron-value (cdr p) 0) 1))))
  (test #t < n 50))
     
;; ----------------------------------------
;; Phantom bytes:

(unless (eq? 'cgc (system-type 'gc))
  (define s (make-semaphore))
  (define c (make-custodian))
  (define t (parameterize ([current-custodian c])
              (thread (lambda ()
                        (semaphore-wait s)
                        (define b (make-phantom-bytes (expt 2 29)))
                        (test #t phantom-bytes? b)
                        (test #f phantom-bytes? 0)
                        (semaphore-wait s)
                        (set-phantom-bytes! b 0)
                        (semaphore-wait s)))))
  (sync (system-idle-evt))
  (collect-garbage)
  (define m (current-memory-use))
  (define mc (current-memory-use c))
  (semaphore-post s)
  (sync (system-idle-evt))
  (test #t > (current-memory-use) (+ m (expt 2 28)))
  (collect-garbage)
  (test #t > (current-memory-use) (+ m (expt 2 28)))
  (test #t > (current-memory-use c) (+ mc (expt 2 28)))
  (semaphore-post s)
  (sync (system-idle-evt))
  (test #t < (current-memory-use) (+ m (expt 2 28)))
  (collect-garbage)
  (test #t < (current-memory-use) (+ m (expt 2 28)))
  (test #t < (current-memory-use c) (+ mc (expt 2 28)))
  (semaphore-post s)

  (let ([done? #f])
    (sync
     (let ([c (make-custodian)])
       (parameterize ([current-custodian c])
         (thread
          (lambda ()
            (custodian-limit-memory c 10000000)
            (define b (make-phantom-bytes 100))
            (set-phantom-bytes! b 0)
            (set! done? #t))))))
    (test #t values done?)))

;; ----------------------------------------
;; Check that local variables are cleared for space safety
;; before a tail `sync' or `thread-wait':

(unless (eq? 'cgc (system-type 'gc))
  (define weak-syms (make-weak-hash))

  (define thds
    (for/list ([i (in-range 100)])
      (thread (lambda () 
                (define s (gensym))
                (define t (current-thread))
                (define sema (make-semaphore))
                (define r (random 2))
                (hash-set! weak-syms s #t)
                (if (zero? (random 1))
                    (if (zero? r)
                        (sync sema)
                        (thread-wait t))
                    (displayln s))))))

  (sync (system-idle-evt))
  (collect-garbage)
  (test #t < (hash-count weak-syms) 50)

  (for ([t thds]) (kill-thread t)))

;; ----------------------------------------
;; Check that an unoptimizable `(variable-reference-constant? (#%variable-reference r))`
;; expression does not retain a reference to the namespace --- since not retaining
;; a reference can be important to the expansion to a call to a keyword-accepting
;; function.

(unless (eq? 'cgc (system-type 'gc))
  (define (mk)
    (parameterize ([current-namespace (make-base-namespace)])
      (eval '(module module-with-unoptimized-varref-constant racket/base
              (define (r)
                (variable-reference-constant?
                 (#%variable-reference r)))
              (define top (box 1))
              (define top-boxed (make-weak-box top))
              (set! r r)
              (provide r top-boxed)))
      (list (dynamic-require ''module-with-unoptimized-varref-constant 'r)
            (dynamic-require ''module-with-unoptimized-varref-constant 'top-boxed))))

  (let ([l (for/list ([i 10])
             (mk))])
    (collect-garbage)
    (define fraction-retained
      (/ (for/fold ([n 0]) ([p (in-list l)])
           (if (weak-box-value (cadr p))
               (add1 n)
               n))
         (for/fold ([n 0]) ([p (in-list l)])
           (if (car p) (add1 n) n))))
    (test #t < fraction-retained 1/2)))

;; ----------------------------------------
;;  Check space safety conversion for nested `if`s

(let ([ht (make-weak-hasheq)])
  (letrec ([f (lambda (false long-vector values n)
                (begin
                  (if false
                      (if (random) 7 (length long-vector))
                      'long-vector-not-cleared-here)
                  (if (zero? n)
                      (begin
                        (collect-garbage)
                        (hash-count ht))
                      (let ([vec (make-vector 1000)])
                        (hash-set! ht vec #t)
                        (values (f false vec values (sub1 n)))))))])
    (set! f f)
    (test #t < (f #f #f values 100) 33)))

;; ----------------------------------------
;; Check space safety related to `if` under a more nested `let` than
;; a relevant binding

(module allocates-many-vectors racket/base
  (provide go)
  
  (define (f x y)
    (let ([z (make-vector 1024 x)]) ; problem if `z` is retained during non-tail `(y)`
      (let ([w (cons x x)])
        (if (pair? x)
            'ok ; SFS pass should clear `z` in or after this branch
            (error "done" x z z w w)))
      (box (y))))

  (set! f f)
  
  (define (go)
    (let loop ([n 100000])
      (f '(1 2) (lambda ()
                  (if (zero? n)
                      'done
                      (unbox (loop (sub1 n)))))))))

(let ([init-memory-use (current-memory-use)])
  (define done? #f)
  (define t (thread (lambda ()
                      ((dynamic-require ''allocates-many-vectors 'go))
                      (set! done? #t))))
  (define watcher-t (thread
                     (lambda ()
                       (let loop ()
                         (sleep 0.1)
                         (define mu (current-memory-use))
                         (printf "~s\n" mu)
                         (cond
                          [(mu . < . (+ init-memory-use (* 100 1024 1024)))
                           (loop)]
                          [else
                           (kill-thread t)])))))
  (sync t)
  (kill-thread watcher-t)
  (test #t 'many-vectors-in-reasonable-space? done?))

;; ----------------------------------------
;; Check that a thread that has a reference to
;; module-level variables doesn't retain the
;; namespace strongly

(unless (eq? 'cgc (system-type 'gc))
  (define-values (f w)
    (parameterize ([current-namespace (make-base-namespace)])
      (define g (gensym 'gensym-via-namespace))
      (eval `(module n racket/base
              ;; If the namespace is retained strongly, then
              ;; the symbol is reachable through this definition:
              (define anchor (quote ,g))))
      (eval `(module m racket/base
              (require 'n)
              (provide f sema)
              (define sema (make-semaphore))
              (define (f)
                (thread
                 (lambda ()
                   ;; Ideally, this loop retains only `loop`
                   ;; and `sema`. If it retains everything refereneced
                   ;; or defined in the module, though, at least make
                   ;; sure it doesn't retain the whole namespace
                   (let loop () (sync sema) (loop)))))))
      (namespace-require ''m)
      (values (dynamic-require ''m 'f)
              (make-weak-box g))))

  (define t (f))
  (sync (system-idle-evt))

  (collect-garbage)
  (test #f weak-box-value w)
  (kill-thread t))

;; ----------------------------------------
;; Check that ephemeron chains do not lead
;; to O(N^2) behavior with 3m

(unless (eq? 'cgc (system-type 'gc))
  (define (wrapper v) (list 1 2 3 4 5 v))

  ;; Create a chain of ephemerons where we have all
  ;; the the ephemerons immediately in a list,
  ;; but we discover the keys one at a time
  (define (mk n prev-key es)
    (cond
     [(zero? n)
      (values prev-key es)]
     [else
      (define key (gensym))
      (mk (sub1 n)
          key
          (cons (make-ephemeron key (wrapper prev-key))
                es))]))

  ;; Create a chain of ephemerons where we have all
  ;; of the keys immediately in a list,
  ;; but we discover the ephemerons one at a time
  (define (mk* n prev-e keys)
    (cond
     [(zero? n)
      (values prev-e keys)]
     [else
      (define key (gensym))
      (mk* (sub1 n)
           (make-ephemeron key (wrapper prev-e))
           (cons key
                 keys))]))

  (define (measure-time n)
    ;; Hang the discover-keys-one-at-a-time chain
    ;; off the end of the discover-ephemerons-one-at-a-time
    ;; chain, which is the most complex case for avoiding
    ;; quadratic GC times
    (define-values (key es) (mk n (gensym) null))
    (define-values (root holds) (mk* n key es))

    (define ITERS 5)
    (define msecs
      (/ (for/fold ([t 0]) ([i (in-range ITERS)])
           (define start (current-inexact-milliseconds))
           (collect-garbage)
           (+ t (- (current-inexact-milliseconds) start)))
         ITERS))
    ;; Keep `key` and `es` live:
    (if (zero? (random 1))
        msecs
        (list root holds)))

  ;; Making a chain 10 times as long should not increase GC time by more
  ;; than a factor of 10:
  (test #t
        'ephemeron-chain
        (let loop ([attempts 5])
          (or ((/ (measure-time 10000) (measure-time 1000)) . < . 11)
              (and (attempts . > . 1)
                   (loop (sub1 attempts)))))))

;; ----------------------------------------
;; Check that `apply` doesn't retain its argument

(unless (eq? 'cgc (system-type 'gc))
  
  (define retained 0)

  (define (f ignored b k)
    (collect-garbage)
    (when (weak-box-value b)
      (set! retained (add1 retained)))
    (k))
  (set! f f)

  (define (mk . args) args)
  (set! mk mk)

  ;; Tail version:
  (let loop ([i 5])
    (unless (zero? i)
      (define val (gensym))
      (apply f (mk val (make-weak-box val) (lambda () (loop (sub1 i)))))))

  ;; Non-tail version:
  (for ([i 5])
    (define val (gensym))
    (apply f (mk val (make-weak-box val) void)))
  
  (test #t < retained 3))

;; ----------------------------------------

(report-errs)
