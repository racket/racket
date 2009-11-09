#lang scheme
(require unstable/list
         unstable/contract)

; explode-path* : path? -> (listof path?)
(define (explode-path* p)
  (let loop ([p p] [r empty])
    (cond 
      [(eq? 'relative p) r]
      [(not p) r]
      [else
       (let-values ([(base name dir?) (split-path p)])
         (loop base (list* name r)))])))

; strip-prefix-ups : (listof path-element?) -> (listof path-element?)
(define (strip-prefix-ups l)
  (define prefix? (box #t))
  (filter (lambda (p)
            (if (unbox prefix?)
                (if (eq? 'up p)
                    #f
                    (begin #t
                           (set-box! prefix? #f)))
                #t))
          l))

; path-without-base : path? path? -> (listof path-element?)
(define (path-without-base base path)
  (define b (explode-path* base))
  (define p (explode-path* path))
  (if (list-prefix? b p)
      (list-tail p (length b))
      (error 'path-without-base "~a is not a prefix of ~a" base path)))

;; build-path-unless-absolute : path-string? path-string? -> path?
(define (build-path-unless-absolute base path)
  (if (absolute-path? path)
      (build-path path)
      (build-path base path)))

(define (directory-part path)
  (let-values ([(base name must-be-dir) (split-path path)])
    (cond
      [(eq? 'relative base) (current-directory)]
      [(not base) (error 'directory-part "~a is a top-level directory" path)]
      [(path? base) base])))

(provide/contract
 [explode-path* (path-string? . -> . (listof path-element?))]
 [path-without-base (path-string? path-string? . -> . (listof path-element?))]
 [strip-prefix-ups ((listof path-element?) . -> . (listof path-element?))] 
 [directory-part (path-string? . -> . path?)]
 [build-path-unless-absolute (path-string? path-string? . -> . path?)])
