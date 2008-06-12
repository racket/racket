#lang scheme/unit

(require "signatures.ss" "utils.ss" "tc-utils.ss" scheme/match)

(import constraints^)
(export dmap^)

;; map : hash mapping variable to dcon
(define-struct dmap (map))

;; fixed : Listof[c]
;; rest : option[c]
(define-struct dcon (fixed rest))

;; dcon-meet : dcon dcon -> dcon
(define (dcon-meet dc1 dc2)
  (match* (dc1 dc2)
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
                 [c2 (in-list-forever fixed2 rest)])
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
                   [c2 (in-list-forever shorter srest)])
          (c-meet c1 c2 (c-X c1)))
        (c-meet lrest srest (c-X lrest))))]))

(define (dmap-meet dm1 dm2)
  (hash-union dm1 dm2
              (lambda (k dc1 dc2) (dcon-meet dc1 dc2))))

