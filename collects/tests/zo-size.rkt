#lang racket/base
(require racket/path
         racket/match)

(define (indent i)
  (for ([t (in-range i)])
    (printf "|")))

(define (dir-zo-size i p)
  (parameterize ([current-directory p])
    (define subdir? #f)
    (define (has-sub-dir!)
      (unless subdir?
        (indent i) (printf "~a:\n" p)
        (set! subdir? #t)))
    (define tot
      (for/fold ([size 0])
        ([p (in-list (directory-list))])
        (+
         (match p
           [(? directory-exists?) (has-sub-dir!) (dir-zo-size (add1 i) p)]
           [(app filename-extension #"zo") (file-size p)]
           [else 0])
         size)))
    (unless (zero? tot)
      (indent i) (printf "~a: ~a\n" p tot))
    tot))

(void (dir-zo-size 0 (simplify-path (build-path (collection-path "racket") 'up))))