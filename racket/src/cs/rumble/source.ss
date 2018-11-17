
;; Duplicating internal definitions from Chez Scheme, which is not a
;; good idea. A better alternative is to extend Chez Scheme to provide
;; something like `continuation->trace`).

(define-record-type code-info
  (fields (immutable src) (immutable sexpr) (immutable free) (immutable live) (immutable rpis))
  (nongenerative #{code-info gr886ae7iuw4wt9ft4vxym-2})
  (sealed #t))

(define-record-type rp-info
  (fields (immutable offset) (immutable src) (immutable sexpr) (immutable mask))
  (nongenerative #{rp-info gr886ae7iuw4wt9ft4vxym-1})
  (sealed #t))

(define (find-rpi offset ci)
  (let ([rpis (code-info-rpis ci)])
    (and
     rpis
     (let loop ([start 0] [end (fx1- (vector-length rpis))])
       (cond
        [(fx< end start)
         #f]
        [else
         (let* ([curr (fx+ (fx/ (fx- end start) 2) start)]
                [rpi (vector-ref rpis curr)]
                [rpi-offset (rp-info-offset rpi)])
           (cond
            [(fx= offset rpi-offset) (rp-info-src rpi)]
            [(fx< offset rpi-offset) (loop start (fx1- curr))]
            [else (loop (fx1+ curr) end)]))])))))
