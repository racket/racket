#lang racket/base

(provide encoded-link-path?
         encode-link-path
         decode-link-path)

(define (encoded-link-path? p)
  (or (path-string? p)
      (path-bytes? p)
      (and (pair? p)
           (list? p)
           (andmap path-element-bytes/same/up? p))))

(define (path-bytes? p)
  (and (bytes? p)
       (positive? (bytes-length p))
       (for/and ([c (in-bytes p)])
         (not (eqv? c 0)))))

(define (path-element-bytes/same/up? p)
  (or (eq? p 'up)
      (eq? p 'same)
      (and (path-bytes? p)
           (let ([p (bytes->path-element p (system-path-convention-type) #t)])
             (and p (relative-path? p))))))

(define (encode-link-path p)
  (if (absolute-path? p)
      (path->bytes p)
      (let loop ([p p] [accum '()])
        (define-values (base name dir?) (split-path p))
        (define new-accum (cons (if (path? name)
                                    (path-element->bytes name)
                                 name)
                                accum))
        (cond
          [(eq? base 'relative) new-accum]
          [else (loop base new-accum)]))))

(define (decode-link-path p)
  (cond
    [(path-string? p) p]
    [(bytes? p) (bytes->path p)]
    [else
     (let loop ([path #f] [p p])
       (cond
         [(null? p) path]
         [else
          (define elem (if (bytes? (car p))
                           (bytes->path-element (car p))
                           (car p)))
          (loop (if path (build-path path elem) elem)
                (cdr p))]))]))
