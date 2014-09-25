#lang racket/base
(require racket/class
         racket/draw/private/dc
         racket/draw/private/bitmap-dc
         racket/draw/private/bitmap
         racket/draw/private/local
         racket/draw/private/record-dc
         racket/draw/unsafe/cairo
         "../../lock.rkt"
         "queue.rkt")

(provide 
 (protect-out backing-dc%
              backing-draw-bm
              
              ;; scoped method names:
              clean-slate
              get-backing-size
              queue-backing-flush
              on-backing-flush
              start-backing-retained
              end-backing-retained
              reset-backing-retained
              make-backing-bitmap
              request-delay
              cancel-delay
              end-delay))

(define-local-member-name
  clean-slate
  get-backing-size
  queue-backing-flush
  on-backing-flush
  start-backing-retained
  end-backing-retained
  reset-backing-retained
  make-backing-bitmap
  request-delay
  cancel-delay
  end-delay)

(define backing-dc%
  (class (record-dc-mixin (dc-mixin bitmap-dc-backend%))
    (init transparent?)

    (inherit internal-get-bitmap
             internal-set-bitmap
             reset-cr
             set-recording-limit
             get-recorded-command
             get-clear-operator)

    (super-new)

    (set-recording-limit (if transparent? 1024 -1))

    (define/override (ok?) #t)

    ;; Override this method to get the right size
    (define/public (get-backing-size xb yb)
      (set-box! xb 1)
      (set-box! yb 1))

    ;; override this method to set up a callback to
    ;;  `on-backing-flush' when the backing store can be rendered
    ;;  to the screen; called atomically (expecting no exceptions)
    (define/public (queue-backing-flush)
      (void))

    (define retained-cr #f)
    (define retained-counter 0)
    (define needs-flush? #f)
    (define nada? #t)

    ;; called with a procedure that is applied to a bitmap;
    ;;  returns #f if there's nothing to flush
    (define/public (on-backing-flush proc)
      (cond
       [(not retained-cr) #f]
       [(positive? retained-counter) 
        (unless nada?
          (proc (or (get-recorded-command)
                    (internal-get-bitmap))))
        #t]
       [else 
        (reset-backing-retained proc)
        #t]))
    
    (define/public (can-backing-flush?)
      (and retained-cr #t))

    (define/public (reset-backing-retained [proc void])
      (let ([cr retained-cr])
        (when cr 
          (let ([bm (internal-get-bitmap)])
            (set! retained-cr #f)
            (internal-set-bitmap #f #t)
            (super release-cr retained-cr)
            (proc bm)
            (release-backing-bitmap bm)))))

    (define/public (start-backing-retained)
      (as-entry
       (lambda () 
         (set! retained-counter (add1 retained-counter)))))

    (define/public (end-backing-retained)
      (as-entry
       (lambda () 
         (if (zero? retained-counter)
             (log-error "unbalanced end-on-paint")
             (set! retained-counter (sub1 retained-counter))))))

    (define/public (make-backing-bitmap w h)
      (make-object bitmap% w h #f #t))

    (define/public (ensure-ready) (get-cr))

    (define/override (get-cr)
      (or retained-cr
          (let ([w (box 0)]
                [h (box 0)])
            (get-backing-size w h)
            (let ([bm (get-backing-bitmap (lambda (w h) (make-backing-bitmap w h)) (unbox w) (unbox h))])
              (internal-set-bitmap bm #t))
            (let ([cr (super get-cr)])
              (set! retained-cr cr)
              (reset-cr cr)
              cr))))

    (define/override (release-cr cr)
      (set! nada? #f)
      (when (zero? flush-suspends)
        (queue-backing-flush)))

    (define/override (erase)
      (super erase)
      (when (= (get-clear-operator)
               CAIRO_OPERATOR_CLEAR)
        (set! nada? #t)))

    (define/public (clean-slate)
      (super erase)
      (set! nada? #t))

    (define flush-suspends 0)
    (define req #f)

    (define/public (request-delay) (void))
    (define/public (cancel-delay req) (void))

    (define/override (suspend-flush) 
      (atomically
       (when (zero? flush-suspends)
         (when req (cancel-delay req))
         (set! req (request-delay)))
       (set! flush-suspends (add1 flush-suspends))))

    (define/override (resume-flush)  
      (atomically 
       (unless (zero? flush-suspends)
         (set! flush-suspends (sub1 flush-suspends))
         (when (zero? flush-suspends)
           (queue-backing-flush)))))

    (define/public (end-delay)
      ;; call in atomic mode
      (when (and (zero? flush-suspends) req)
        (cancel-delay req)
        (set! req #f)))))

(define (get-backing-bitmap make-bitmap w h)
  (make-bitmap w h))

(define (release-backing-bitmap bm)
  (send bm release-bitmap-storage))

(define (scale-mixin %)
  (class %
    (define backing-scale 1.0)

    (super-new)
    
    (define/override (init-cr-matrix cr)
      (unless (= backing-scale 1.0)
        (cairo_scale cr backing-scale backing-scale))
      (super init-cr-matrix cr))

    (define/override (init-effective-matrix mx)
      (unless (= backing-scale 1.0)
        (cairo_matrix_scale mx backing-scale backing-scale))
      (super init-effective-matrix mx))
    
    (define/override (reset-config s)
      (set! backing-scale s)
      (super reset-config))))

(define cairo-dc
  (make-object (scale-mixin
		(dc-mixin
		 (class default-dc-backend%
                   (inherit reset-cr)

                   (define cr #f)
                   (define w 0)
                   (define h 0)

                   (super-new)

                   (define/public (set-cr new-cr new-w new-h)
                     (set! cr new-cr)
                     (set! w new-w)
                     (set! h new-h)
                     (when cr
                       (reset-cr cr)))

                   (define/override (get-cr) cr)

                   (define/override (reset-clip cr)
                     (super reset-clip cr)
                     (cairo_rectangle cr 0 0 w h)
                     (cairo_clip cr)))))))

(define (backing-draw-bm bm cr w h [dx 0] [dy 0] [backing-scale 1.0])
  (if (procedure? bm)
      (begin
        (send cairo-dc reset-config backing-scale)
        (send cairo-dc set-cr cr w h)
	(unless (and (zero? dx) (zero? dy))
	  (send cairo-dc translate dx dy))
        (bm cairo-dc)
        (send cairo-dc set-cr #f 0 0))
      (let ([s (cairo_get_source cr)])
	(unless (and (zero? dx) (zero? dy))
	  (cairo_translate cr dx dy))
        (cairo_pattern_reference s)
        (cairo_set_source_surface cr (send bm get-cairo-surface) 0 0)
        (let ([sc (send bm get-cairo-device-scale)])
          (unless (= sc 1)
            (let ([m (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0)])
              (cairo_matrix_init_translate m 0 0)
              (cairo_matrix_scale m sc sc)
              (cairo_pattern_set_matrix (cairo_get_source cr) m))))
        (cairo_paint cr)
        (cairo_set_source cr s)
        (cairo_pattern_destroy s))))
