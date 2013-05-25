#lang racket/base

(require racket/draw racket/class racket/match racket/list ffi/unsafe
         (for-syntax racket/base)
         "flomap.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Caching flomaps with a hash table of weak box values

(define num-callbacks 0)
(define (get-num-callbacks) num-callbacks)
 
(define (register-gc-callback proc)
  (register-finalizer (malloc 4) (λ (val)
                                  (set! num-callbacks (+ 1 num-callbacks))
                                  (printf "here~n")
                                  (when (proc) (register-gc-callback proc)))))

(define (weak-value-hash-clean! h)
  (define ks (for*/list ([(k bx)  (in-hash h)]
                         [val  (in-value (weak-box-value (car bx)))]
                         #:when (not val))
               k))
  (for ([k  (in-list ks)]) (hash-remove! h k)))

(define total-time-saved 0)
(define total-time-spent 0)

;; Can't simply wrap hash-ref! with weak-box-value and thnk with make-weak-box, because
;; 1. If weak-box-value returns #f, we need to regenerate the value
;; 2. We need to keep a handle to the generated value while it's being stored in the hash
(define (weak-value-hash-ref! h k thnk)
  (define (cache-ref!)
    (define start (current-milliseconds))
    (define val (thnk))
    (define time (- (current-milliseconds) start))
    (set! total-time-spent (+ total-time-spent time))
    ;(printf "total-time-spent = ~v~n" total-time-spent)
    (hash-set! h k (cons (make-weak-box val) time))
    val)
  (cond [(hash-has-key? h k)  (define bx (hash-ref h k))
                              (define val (weak-box-value (car bx)))
                              (cond [val   (set! total-time-saved (+ total-time-saved (cdr bx)))
                                           ;(printf "total-time-saved = ~v~n" total-time-saved)
                                           val]
                                    [else  (cache-ref!)])]
        [else  (cache-ref!)]))

(define flomap-cache (make-hash))

(define (clean-flomap-cache!)
  (weak-value-hash-clean! flomap-cache)
  #t)

(register-gc-callback clean-flomap-cache!)

(define (get-flomap-cache)
  (for/list ([(k bx)  (in-hash flomap-cache)])
    (cons k (cons (weak-box-value (car bx)) (cdr bx)))))

(define (get-total-time-saved) total-time-saved)
(define (get-total-time-spent) total-time-spent)

(define (make-cached-flomap* name proc size . args)
  (define rendered-size (if (size . < . 32) 32 size))
  (define fm (weak-value-hash-ref! flomap-cache (list name rendered-size args)
                        (λ () (apply proc rendered-size args))))
  (flomap-scale fm (/ size rendered-size)))

(define-syntax (make-cached-flomap stx)
  (syntax-case stx ()
    [(_ (size args ...) expr0 expr ...)
     ;; for some reason, generate-temporaries doesn't work here
     (with-syntax ([name  (gensym)])
       (syntax/loc stx
         (make-cached-flomap* 'name (λ (size args ...) expr0 expr ...) size args ...)))]))

;; ===================================================================================================
;; Drawing

(define (->color% c)
  (match c
    [(list r g b)  (make-object color% r g b)]
    [(? (is-a?/c color%))  c]
    [(? string?)  (send the-color-database find-color c)]
    [else  (raise-type-error '->color% "list, color% or string" c)]))

(define (apply-path-commands p cmds)
  (let loop ([x 0] [y 0] [cmds cmds])
    (cond
      [(empty? cmds)  (values x y)]
      [else
       (define cmd (first cmds))
       (match cmd
         ;; absolute commands
         [`(M)  (loop x y (rest cmds))]
         [`(L)  (loop x y (rest cmds))]
         [`(C)  (loop x y (rest cmds))]
         [`(M ,ax ,ay ,as ...)  (send p move-to ax ay)
                                (loop ax ay (cons `(M ,@as) (rest cmds)))]
         [`(L ,ax ,ay ,as ...)  (send p line-to ax ay)
                                (loop ax ay (cons `(L ,@as) (rest cmds)))]
         [`(C ,ax1 ,ay1 ,ax2 ,ay2 ,ax ,ay ,as ...)
          (send p curve-to ax1 ay1 ax2 ay2 ax ay)
          (loop ax ay (cons `(C ,@as) (rest cmds)))]
         ;; relative commands
         [`(m)  (loop x y (rest cmds))]
         [`(l)  (loop x y (rest cmds))]
         [`(c)  (loop x y (rest cmds))]
         [`(m ,dx ,dy ,ds ...)  (send p move-to (+ x dx) (+ y dy))
                                (loop (+ x dx) (+ y dy) (cons `(m ,@ds) (rest cmds)))]
         [`(l ,dx ,dy ,ds ...)  (send p line-to (+ x dx) (+ y dy))
                                (loop (+ x dx) (+ y dy) (cons `(l ,@ds) (rest cmds)))]
         [`(c ,dx1 ,dy1 ,dx2 ,dy2 ,dx ,dy ,ds ...)
          (send p curve-to (+ dx1 x) (+ dy1 y) (+ dx2 x) (+ dy2 y) (+ dx x) (+ dy y))
          (loop (+ x dx) (+ y dy) (cons `(c ,@ds) (rest cmds)))]
         [_  (error 'apply-path-commands "unknown path command ~e" cmd)])]))
  (void))

(define (draw-path-commands dc cmds x y)
  (define p (new dc-path%))
  (apply-path-commands p cmds)
  (define t (send dc get-transformation))
  (send dc translate x y)
  (send dc draw-path p)
  (send dc set-transformation t))

(define (list->pairs lst)
  (match lst
    [(list x y xs ...)  (cons (cons x y) (list->pairs xs))]
    [(list)  (list)]))

(define (scale-path-commands cmds sx sy)
  (match cmds
    [(list `(,sym ,xys ...) cmds ...)
     (cons
      `(,sym ,@(flatten (map (λ (xy)
                               (match-define (cons x y) xy)
                               (list (* x sx) (* y sy)))
                             (list->pairs xys))))
      (scale-path-commands cmds sx sy))]
    [(list)  (list)]))

(define (relativize-path-commands cmds)
  (let loop ([x 0] [y 0] [cmds cmds])
    (cond
      [(empty? cmds)  empty]
      [else
       (define cmd (first cmds))
       (match cmd
         ;; absolute commands
         [`(M)  (loop x y (rest cmds))]
         [`(L)  (loop x y (rest cmds))]
         [`(C)  (loop x y (rest cmds))]
         [`(M ,ax ,ay ,as ...)  (cons `(m ,(- ax x) ,(- ay y))
                                      (loop ax ay (cons `(M ,@as) (rest cmds))))]
         [`(L ,ax ,ay ,as ...)  (cons `(l ,(- ax x) ,(- ay y))
                                      (loop ax ay (cons `(L ,@as) (rest cmds))))]
         [`(C ,ax1 ,ay1 ,ax2 ,ay2 ,ax ,ay ,as ...)
          (cons `(c ,(- ax1 x) ,(- ay1 y) ,(- ax2 x) ,(- ay2 y) ,(- ax x) ,(- ay y))
                (loop ax ay (cons `(C ,@as) (rest cmds))))]
         ;; relative commands
         [`(m)  (loop x y (rest cmds))]
         [`(l)  (loop x y (rest cmds))]
         [`(c)  (loop x y (rest cmds))]
         [`(m ,dx ,dy ,ds ...)  (cons `(m ,dx ,dy) (loop (+ x dx) (+ y dy)
                                                         (cons `(m ,@ds) (rest cmds))))]
         [`(l ,dx ,dy ,ds ...)  (cons `(l ,dx ,dy) (loop (+ x dx) (+ y dy)
                                                         (cons `(l ,@ds) (rest cmds))))]
         [`(c ,dx1 ,dy1 ,dx2 ,dy2 ,dx ,dy ,ds ...)
          (cons `(c ,dx1 ,dy1 ,dx2 ,dy2 ,dx ,dy)
                (loop (+ x dx) (+ y dy) (cons `(c ,@ds) (rest cmds))))]
         [_  (error 'apply-path-commands "unknown path command ~e" cmd)])])))

(define (get-text-size str font)
  (define bm (make-bitmap 1 1))
  (define dc (make-object bitmap-dc% bm))
  (define-values (w h _1 _2) (send dc get-text-extent str font #t))
  (values (inexact->exact (ceiling w))
          (inexact->exact (ceiling h))))
