#lang racket/base

(provide relative-path-elements->path
         path->relative-path-elements)

(define (relative-path-elements->path elems)
  (define wrt-dir (current-load-relative-directory))
  (define rel-elems (for/list ([p (in-list elems)])
                      (if (bytes? p) (bytes->path-element p) p)))
  (cond
    [wrt-dir (apply build-path wrt-dir rel-elems)]
    [(null? rel-elems) (build-path 'same)]
    [else (apply build-path rel-elems)]))

(define (path->relative-path-elements v
                                      #:exploded-base-dir [exploded-base-dir (box 'not-ready)]
                                      #:exploded-wrt-rel-dir [exploded-wrt-rel-dir (box 'not-ready)])
  (when (and (eq? (unbox exploded-base-dir) 'not-ready)
             (path? v))
    (define wr-dir (current-write-relative-directory))
    (define wrt-dir (and wr-dir (if (pair? wr-dir) (car wr-dir) wr-dir)))
    (define base-dir (and wr-dir (if (pair? wr-dir) (cdr wr-dir) wr-dir)))
    (set-box! exploded-base-dir (and base-dir (explode-path base-dir)))
    (set-box! exploded-wrt-rel-dir
              (if (eq? base-dir wrt-dir)
                  '()
                  (list-tail (explode-path wrt-dir)
                             (length (unbox exploded-base-dir))))))
  (and (unbox exploded-base-dir)
       (path? v)
       (let ([exploded (explode-path v)])
         (and (for/and ([base-p (in-list (unbox exploded-base-dir))]
                        [p (in-list exploded)])
                (equal? base-p p))
              (let loop ([exploded-wrt-rel-dir (unbox exploded-wrt-rel-dir)]
                         [rel (list-tail exploded (length (unbox exploded-base-dir)))])
                (cond
                  [(null? exploded-wrt-rel-dir) rel]
                  [(and (pair? rel)
                        (equal? (car rel) (car exploded-wrt-rel-dir)))
                   (loop (cdr exploded-wrt-rel-dir) (cdr rel))]
                  [else (append (for/list ([p (in-list exploded-wrt-rel-dir)])
                                  'up)
                                rel)]))))))
