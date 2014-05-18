#lang racket/unit

(require "../utils/utils.rkt" "fail.rkt"
         "signatures.rkt" "constraint-structs.rkt"
         (utils tc-utils) 
         (contract-req)
         unstable/sequence unstable/hash racket/match)

(import constraints^)
(export dmap^)


;; dcon-meet : dcon dcon -> dcon or #f
(define/cond-contract (dcon-meet dc1 dc2)
  (dcon/c dcon/c . -> . (or/c #f dcon/c))
  (match*/early (dc1 dc2)
    [((struct dcon-exact (fixed1 rest1)) (or (struct dcon (fixed2 rest2))
                                             (struct dcon-exact (fixed2 rest2))))
     #:return-unless (and rest2 (= (length fixed1) (length fixed2)))
     #f
     (% make-dcon-exact
        (for/list/fail ([c1 (in-list fixed1)]
                        [c2 (in-list fixed2)])
           (c-meet c1 c2))
        (c-meet rest1 rest2))]
    ;; redo in the other order to call the previous case
    [((struct dcon (fixed1 rest1)) (struct dcon-exact (fixed2 rest2)))
     (dcon-meet dc2 dc1)]
    [((struct dcon (fixed1 #f)) (struct dcon (fixed2 #f)))
     #:return-unless (= (length fixed1) (length fixed2))
     #f
     (%1 make-dcon
         (for/list/fail ([c1 (in-list fixed1)]
                         [c2 (in-list fixed2)])
           (c-meet c1 c2))
         #f)]
    [((struct dcon (fixed1 #f)) (struct dcon (fixed2 rest)))
     #:return-unless (>= (length fixed1) (length fixed2))
     #f
     (%1 make-dcon
      (for/list/fail ([c1 (in-list fixed1)]
                      [c2 (in-sequence-forever fixed2 rest)])
        (c-meet c1 c2))
      #f)]
    ;; redo in the other order to call the previous case
    [((struct dcon (fixed1 rest)) (struct dcon (fixed2 #f)))
     (dcon-meet dc2 dc1)]
    [((struct dcon (fixed1 rest1)) (struct dcon (fixed2 rest2)))
     (let-values ([(shorter longer srest lrest)
                   (if (< (length fixed1) (length fixed2))
                       (values fixed1 fixed2 rest1 rest2)
                       (values fixed2 fixed1 rest2 rest1))])
       (% make-dcon
        (for/list/fail ([c1 (in-list longer)]
                   [c2 (in-sequence-forever shorter srest)])
          (c-meet c1 c2))
        (c-meet lrest srest)))]
    [((struct dcon-dotted (fixed1 c1 bound1)) (struct dcon-dotted (fixed2 c2 bound2)))
     #:return-unless (and (= (length fixed1) (length fixed2))
                          (eq? bound1 bound2))
     #f
     (% make-dcon-dotted (for/list/fail ([c1 (in-list fixed1)] [c2 (in-list fixed2)])
                           (c-meet c1 c2))
                       (c-meet c1 c2) bound1)]
    [((struct dcon _) (struct dcon-dotted _))
     #f]
    [((struct dcon-dotted _) (struct dcon _))
     #f]
    [(_ _) (int-err "Got non-dcons: ~a ~a" dc1 dc2)]))

;; dmap dmap -> dmap or #f
(define (dmap-meet dm1 dm2)
  (% make-dmap
   (hash-union/fail (dmap-map dm1) (dmap-map dm2) #:combine dcon-meet)))
