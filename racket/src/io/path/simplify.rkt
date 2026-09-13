#lang racket/base
(require racket/fixnum
         "path.rkt"
         "check.rkt"
         "check-path.rkt"
         "relativity.rkt"
         "../file/main.rkt"
         "split.rkt"
         "build.rkt"
         "parameter.rkt"
         "complete.rkt"
         (submod "../file/main.rkt" for-simplify)
         "simplify-nofs.rkt"
         "../error/message.rkt")

(provide simplify-path)

(define/who (simplify-path p-in [use-filesystem? #t])
  (check-path-argument who p-in)
  (define p (->path p-in))
  (simplify-path-syntactically who
                               p
                               (and use-filesystem? use-filesystem)))

(define (use-filesystem who l)
  ;; Use the filesystem, which requires building
  ;; a full path
  (define (combine base accum)
    (if (null? accum)
        base
        (apply build-path base (reverse accum))))
  (let loop ([l (if (path? (car l)) (cdr l) l)]
             [base (if (path? (car l))
                       ;; convert starting point absolute as needed
                       (path->complete-path (car l) (current-directory))
                       ;; original must be relative
                       (current-directory))]
             [accum '()]
             [seen #hash()])
    (define (path-bytes* p) (if (eq? p 'up) p (path-bytes p)))
    (cond
      [(null? l) (combine base accum)]
      [(eq? 'same (car l))
       (loop (cdr l) base accum seen)]
      [(eq? 'up (car l))
       (define new-base (combine base accum))
       (define target (resolve-path-for-simplify new-base))
       (cond
         [(eq? target new-base)
          ;; not a link
          (define-values (next-base name dir?) (split-path new-base))
          (cond
            [(not next-base)
             ;; discard ".." after a root
             (loop (cdr l) new-base '() seen)]
            [else
             (loop (cdr l) next-base '() seen)])]
         [else
          (when (hash-ref seen new-base #f)
            (raise
             (exn:fail:filesystem
              (error-message->string
               who
               (string-append "cycle detected at link"
                              "\n  link path: " (path->string new-base)))
              (current-continuation-marks))))
          (define new-seen (hash-set seen new-base #t))
          (cond
            [(complete-path? target)
             (define new-l (explode-path target))
             (loop (append (cdr new-l) l) (car new-l) null new-seen)]
            [else
             (define-values (base-dir name dir?) (split-path new-base))
             (loop (append (explode-path target) l) base-dir null new-seen)])])]
      [else (loop (cdr l) base (cons (car l) accum) seen)])))

(void (set-simplify-path-for-directory-list! simplify-path))
