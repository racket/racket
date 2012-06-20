#lang racket/unit

(require "../utils/utils.rkt"
	 "signatures.rkt" "constraint-structs.rkt"
	 (utils tc-utils) racket/contract
	 unstable/sequence unstable/hash racket/match)

(import constraints^)
(export dmap^)

;; dcon-meet : dcon dcon -> dcon
(define/cond-contract (dcon-meet dc1 dc2)
  (dcon/c dcon/c . -> . dcon/c)
  (match* (dc1 dc2)
    [((struct dcon-exact (fixed1 rest1)) (or (struct dcon (fixed2 rest2))
                                             (struct dcon-exact (fixed2 rest2))))
     (unless (and rest2 (= (length fixed1) (length fixed2)))
       (fail! fixed1 fixed2))
     (make-dcon-exact
      (for/list ([c1 fixed1]
                 [c2 fixed2])
        (c-meet c1 c2 (c-X c1)))
      (c-meet rest1 rest2 (c-X rest1)))]
    ;; redo in the other order to call the first case
    [((struct dcon (fixed1 rest1)) (struct dcon-exact (fixed2 rest2)))
     (dcon-meet dc2 dc1)]
    [((struct dcon (fixed1 #f)) (struct dcon (fixed2 #f)))
     (unless (= (length fixed1) (length fixed2))
       (fail! fixed1 fixed2))
     (make-dcon
      (for/list ([c1 fixed1]
                 [c2 fixed2])
        (c-meet c1 c2 (c-X c1)))
      #f)]
    [((struct dcon (fixed1 #f)) (struct dcon (fixed2 rest)))
     (unless (>= (length fixed1) (length fixed2))
       (fail! fixed1 fixed2))
     (make-dcon
      (for/list ([c1 fixed1]
                 [c2 (in-sequence-forever fixed2 rest)])
        (c-meet c1 c2 (c-X c1)))
      #f)]
    [((struct dcon (fixed1 rest)) (struct dcon (fixed2 #f)))
     (dcon-meet dc2 dc1)]
    [((struct dcon (fixed1 rest1)) (struct dcon (fixed2 rest2)))
     (let-values ([(shorter longer srest lrest)
                   (if (< (length fixed1) (length fixed2))
                       (values fixed1 fixed2 rest1 rest2)
                       (values fixed2 fixed1 rest2 rest1))])
       (make-dcon
        (for/list ([c1 longer]
                   [c2 (in-sequence-forever shorter srest)])
          (c-meet c1 c2 (c-X c1)))
        (c-meet lrest srest (c-X lrest))))]
    [((struct dcon-dotted (fixed1 c1 bound1)) (struct dcon-dotted (fixed2 c2 bound2)))
     (unless (and (= (length fixed1) (length fixed2))
                  (eq? bound1 bound2))
       (fail! bound1 bound2))
     (make-dcon-dotted (for/list ([c1 fixed1] [c2 fixed2])
                         (c-meet c1 c2 (c-X c1)))
                       (c-meet c1 c2 bound1) bound1)]
    [((struct dcon _) (struct dcon-dotted _))
     (fail! dc1 dc2)]
    [((struct dcon-dotted _) (struct dcon _))
     (fail! dc1 dc2)]
    [(_ _) (int-err "Got non-dcons: ~a ~a" dc1 dc2)]))

(define (dmap-meet dm1 dm2)
  (make-dmap
   (hash-union (dmap-map dm1) (dmap-map dm2) #:combine dcon-meet)))
