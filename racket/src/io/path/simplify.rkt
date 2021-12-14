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
    (cond
      [(null? l) (combine base accum)]
      [(eq? 'same (car l))
       (loop (cdr l) base accum seen)]
      [(eq? 'up (car l))
       (define new-base (combine base accum))
       (define target (resolve-path-for-simplify new-base))
       (define-values (from-base new-seen)
         (cond
           [(eq? target new-base) (values new-base seen)]
           [else
            (define from-base
              (cond
                [(complete-path? target) target]
                [else
                 (define-values (base-dir name dir?) (split-path new-base))
                 (path->complete-path target base-dir)]))
            (when (hash-ref seen from-base #f)
              (raise
               (exn:fail:filesystem
                (error-message->string
                 who
                 (string-append "cycle detected at link"
                                "\n  link path: " (path->string new-base)))
                (current-continuation-marks))))
            (values from-base (hash-set seen from-base #t))]))
       (define-values (next-base name dir?) (split-path from-base))
       (cond
         [(not next-base)
          ;; discard ".." after a root
          (loop (cdr l) from-base '() new-seen)]
         [else
          (loop (cdr l) next-base '() new-seen)])]
      [else (loop (cdr l) base (cons (car l) accum) seen)])))

(void (set-simplify-path-for-directory-list! simplify-path))
