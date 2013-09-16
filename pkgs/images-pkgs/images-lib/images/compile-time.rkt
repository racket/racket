#lang racket/base

(require (for-syntax racket/base
                     racket/class racket/draw racket/math)
         racket/class racket/draw)

(provide compiled-bitmap compiled-bitmap-list)

(begin-for-syntax
  (define (save-png bm)
    (define p (open-output-bytes))
    (send bm save-file p 'png)
    (define bs (get-output-bytes p))
    ;(printf "Wrote PNG: ~v bytes~n" (bytes-length bs))
    bs)
  
  (define (save-jpeg bm quality)
    (define w (send bm get-width))
    (define h (send bm get-height))
    (define bs (make-bytes (* 4 w h)))
    
    (send bm get-argb-pixels 0 0 w h bs #t)
    (for ([i  (in-range 0 (* 4 w h) 4)])
      (define a (bytes-ref bs i))
      (bytes-set! bs i 255)
      (bytes-set! bs (+ i 1) a)
      (bytes-set! bs (+ i 2) a)
      (bytes-set! bs (+ i 3) a))
    
    (define alpha-bm (make-bitmap w h #f))
    (send alpha-bm set-argb-pixels 0 0 w h bs)
    (define alpha-p (open-output-bytes))
    (send alpha-bm save-file alpha-p 'jpeg quality)
    
    (send bm get-argb-pixels 0 0 w h bs #f)
    (define rgb-bm (make-bitmap w h #f))
    (send rgb-bm set-argb-pixels 0 0 w h bs #f)
    (define rgb-p (open-output-bytes))
    (send rgb-bm save-file rgb-p 'jpeg quality)
    
    (define alpha-bs (get-output-bytes alpha-p))
    (define rgb-bs (get-output-bytes rgb-p))
    ;(printf "Wrote JPEG: ~v bytes~n" (+ (bytes-length alpha-bs) (bytes-length rgb-bs)))
    
    (values alpha-bs rgb-bs))
  
  (define (make-3d-bitmap ctxt bm quality)
    (unless (is-a? bm bitmap%)
      (raise-type-error 'make-3d-bitmap "bitmap%" 0 bm quality))
    (unless (and (exact-integer? quality) (<= 0 quality 100))
      (raise-type-error 'make-3d-bitmap "(integer-in 0 100)" 1 bm quality))
    (cond [(= quality 100)
           (with-syntax ([bs  (datum->syntax ctxt (save-png bm))])
             (syntax/loc ctxt (load-png bs)))]
          [else
           (define-values (alpha-bs rgb-bs) (save-jpeg bm quality))
           (with-syntax ([alpha-bs  (datum->syntax ctxt alpha-bs)]
                         [rgb-bs    (datum->syntax ctxt rgb-bs)])
             (syntax/loc ctxt (load-jpeg alpha-bs rgb-bs)))]))
  )

(define (load-png bs)
  (read-bitmap (open-input-bytes bs) 'png/alpha))

(define (load-jpeg alpha-bs rgb-bs)
  (define alpha-bm (read-bitmap (open-input-bytes alpha-bs) 'jpeg))
  (define rgb-bm (read-bitmap (open-input-bytes rgb-bs) 'jpeg))
  (define w (send rgb-bm get-width))
  (define h (send rgb-bm get-height))
  
  (define new-bs (make-bytes (* 4 w h)))
  (send rgb-bm get-argb-pixels 0 0 w h new-bs #f)
  
  (define bs (make-bytes (* 4 w h)))
  (send alpha-bm get-argb-pixels 0 0 w h bs #f)
  (for ([i  (in-range 0 (* 4 w h) 4)])
    (define a (bytes-ref bs (+ i 2)))
    (bytes-set! new-bs i a))
  
  (define new-bm (make-bitmap w h))
  (send new-bm set-argb-pixels 0 0 w h new-bs #f)
  new-bm)

(define-syntax (compiled-bitmap stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx (compiled-bitmap expr 100))]
    [(_ expr quality)
     (syntax/loc stx
       (let-syntax ([maker  (λ (inner-stx)
                              (define bm expr)
                              (unless (is-a? bm bitmap%)
                                (raise-syntax-error
                                 'compiled-bitmap
                                 (format "expected argument of type <bitmap%>; given ~e" bm)
                                 #'expr))
                              (make-3d-bitmap inner-stx bm quality))])
         (maker)))]))

(define-syntax (compiled-bitmap-list stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx (compiled-bitmap-list expr 100))]
    [(_ expr quality)
     (syntax/loc stx
       (let-syntax ([maker  (λ (inner-stx)
                              (define bms expr)
                              (unless (and (list? bms) (andmap (λ (bm) (is-a? bm bitmap%)) bms))
                                (raise-syntax-error
                                 'compiled-bitmap-list
                                 (format "expected argument of type <list of bitmap%>; given ~e" bms)
                                 #'expr))
                              (with-syntax ([(bm (... ...))
                                             (map (λ (e) (make-3d-bitmap inner-stx e quality)) bms)])
                                #'(list bm (... ...))))])
         (maker)))]))
