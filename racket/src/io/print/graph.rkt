#lang racket/base
(require "../port/nowhere.rkt"
         "../port/output-port.rkt"
         "parameter.rkt"
         "custom-write.rkt"
         "mode.rkt"
         "config.rkt"
         "recur-handler.rkt")

(provide detect-graph
         (struct-out as-constructor))

(define (detect-graph v mode config)
  (define print-graph? (print-graph))
  (cond
    [(quick-no-graph? v 100 mode print-graph? config) #f]
    [else
     (define ht (make-hasheq))
     (build-graph v ht print-graph? mode config)]))

;; ----------------------------------------

;; Returns a true value if `v` can print without graph annotations and
;; without a constructor form (as opposed to quoted form) in `print`
;; mode
(define (quick-no-graph? v fuel mode print-graph? config)
  (let quick-no-graph? ([v v] [fuel fuel])
    (cond
      [(or (not fuel) (zero? fuel)) #f]
      [(pair? v)
       (and (not print-graph?)
            (quick-no-graph? (cdr v) (quick-no-graph? (car v) (sub1 fuel))))]
      [(vector? v)
       (and (not print-graph?)
            (for/fold ([fuel (sub1 fuel)]) ([e (in-vector v)]
                                            #:break (not fuel))
              (quick-no-graph? e fuel)))]
      [(and (box? v)
            (config-get config print-box))
       (and (not print-graph?)
            (quick-no-graph? (unbox v) (sub1 fuel)))]
      [(and (hash? v)
            (not (hash-weak? v))
            (config-get config print-hash-table))
       (and (not print-graph?)
            ;; Using `(in-hash-keys v #f)` and `(hash-ref v k #f)`
            ;; defends against misbehaving hash tables
            (for/fold ([fuel (sub1 fuel)]) ([k (in-hash-keys v #f)]
                                            #:break (not fuel))
              (define val (hash-ref v k #f))
              (quick-no-graph? val (quick-no-graph? k fuel))))]
      [(mpair? v)
       (and (not print-graph?)
            (not (eq? mode PRINT-MODE/UNQUOTED))
            (quick-no-graph? (mcdr v) (quick-no-graph? (mcar v) (sub1 fuel))))]
      [(custom-write? v)
       #f]
      [(and (struct? v)
            (config-get config print-struct))
       (and (not print-graph?)
            (prefab-struct-key v) ; can quote a prefab in `print` mode
            (quick-no-graph? (struct->vector v) (sub1 fuel)))]
      [else fuel])))

;; ----------------------------------------

(struct as-constructor (tag)) ; `tag` is #f or a number for graph printing

;; Create a hash table that maps some values to a number,
;; which indicates that that the value should be printed with
;; a `#<n>=` prefix and referenced with `#<n>#` thereafter.
;; The hash table records the <n> to be used as a number,
;; and the printer mutates the table to turn that into `#<n>=`
;; after the first reference.
;;
;; In addition, the table indicates whether an item needs
;; to be printed in constructor form, as opposed to quoted
;; form. Printing in constructor form is indicated by
;; mapping to a wrapped `as-constructor` wapper on an integer
;; or `#f`.
;;
;; During `build-graph`, the table maps a value to one of
;;   - 'checking: currently checking, so finding again
;;     implies a cycle
;;   - 'checked: finished checking, but might be referenced
;;     again, which is relevant if graph printing is one of
;;     we go into graph-printing mode
;;   - number: graph-rereference detected, and assigned
;;     the number via `counter`
;;   - (as-constructor #f): like 'checked, but should be printed in
;;     constructor mode, as opposed to quoted
;;   - (as-constructor number): graph-rereference detected, and
;;     initial reference print in construcor mode
;; If no cycle is detected and `(print-graph)` is false, then
;; values other than `as-constructor` are removed. All
;; 'checked entries will be cleared out before the hash table
;; is returned.
(define (build-graph v ht print-graph? mode config)
  (define counter 0)
  (define cycle? #f)
  (define constructor? #f)
  (define checking-port #f)
  (define (checking! v)
    (hash-set! ht v 'checking))
  (define (done! v unquoted?)
    (when (eq? 'checking (hash-ref ht v #f))
      (hash-set! ht v 'checked))
    (when unquoted?
      (define c (hash-ref ht v #f))
      (hash-set! ht v (as-constructor (and (integer? c) c)))
      (set! constructor? #t))
    unquoted?)
  ;; Returns #t if `v` needs to be unquoted
  (let build-graph ([v v] [mode mode])
    (cond
      [(not v) #f]
      [(hash-ref ht v #f)
       => (lambda (g)
            (when (or (eq? g 'checking)
                      (eq? g 'checked)
                      (and (as-constructor? g)
                           (not (as-constructor-tag g))))
              (hash-set! ht v (if (as-constructor? g)
                                  (as-constructor counter)
                                  counter))
              (set! counter (add1 counter))
              (when (eq? g 'checking)
                (set! cycle? #t)))
            #f)]
      [(pair? v)
       (checking! v)
       (define car-unquoted? (build-graph (car v) mode))
       (define unquoted?
         (or (build-graph (cdr v) mode)
             car-unquoted?))
       (done! v unquoted?)]
      [(vector? v)
       (checking! v)
       (define unquoted?
         (for/fold ([unquoted? #f]) ([e (in-vector v)])
           (or (build-graph e mode)
               unquoted?)))
       (done! v unquoted?)]
      [(and (box? v)
            (config-get config print-box))
       (checking! v)
       (define unquoted? (build-graph (unbox v) mode))
       (done! v unquoted?)]
      [(and (hash? v)
            (not (hash-weak? v))
            (config-get config print-hash-table))
       (checking! v)
       (define unquoted?
         (for/fold ([unquoted? #f]) ([k (in-hash-keys v)])
           (define val (hash-ref v k #f))
           (define k-unquoted? (build-graph k mode))
           (or (build-graph val mode)
               k-unquoted?
               unquoted?)))
       (done! v unquoted?)]
      [(mpair? v)
       (checking! v)
       (build-graph (mcar v) mode)
       (build-graph (mcdr v) mode)
       (done! v (eq? mode PRINT-MODE/UNQUOTED))]
      [(custom-write? v)
       (define print-quotable (if (eq? mode PRINT-MODE/UNQUOTED)
                                  (custom-print-quotable-accessor v 'self)
                                  'self))
       (define unquoted? (eq? print-quotable 'never))
       (unless checking-port
         (set! checking-port (open-output-nowhere))
         (set-port-handlers-to-recur!
          checking-port
          (lambda (e p mode)
            (cond
              [(or (eq? mode PRINT-MODE/QUOTED)
                   (eq? mode PRINT-MODE/UNQUOTED))
               (define e-unquoted? (build-graph e mode))
               (unless (or (eq? print-quotable 'always)
                           (eq? print-quotable 'self))
                 (set! unquoted? (or e-unquoted? unquoted?)))]
              [else (build-graph e mode)]))))
       (checking! v)
       ((custom-write-accessor v) v checking-port mode)
       (done! v unquoted?)]
      [(and (struct? v)
            (config-get config print-struct))
       (checking! v)
       (define unquoted?
         (or (for/fold ([unquoted? #f]) ([e (in-vector (struct->vector v))])
               (or (build-graph e mode)
                   unquoted?))
             (and (eq? mode PRINT-MODE/UNQUOTED)
                  (not (prefab-struct-key v)))))
       (done! v unquoted?)]
      [else #f]))
  ;; Clean out unwanted entries
  (cond
    [(and (not cycle?) (not constructor?) (not print-graph?))
     ;; No table needed after all
     #f]
    [(and (not cycle?) (not print-graph?))
     (for ([k (in-list (hash-keys ht))])
       (define v (hash-ref ht k))
       (cond
         [(not (as-constructor? v))
          (hash-remove! ht k)]
         [(as-constructor-tag v)
          (hash-set! ht k (as-constructor #f))]))
     ht]
    [else
     (for ([k (in-list (hash-keys ht))])
       (when (eq? 'checked (hash-ref ht k))
         (hash-remove! ht k)))
     ht]))
