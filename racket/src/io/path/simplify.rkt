#lang racket/base
(require "../file/main.rkt"
         "path.rkt"
         "check.rkt"
         "check-path.rkt"
         "sep.rkt"
         "relativity.rkt"
         "split.rkt"
         "build.rkt"
         "cleanse.rkt"
         "directory-path.rkt"
         "complete.rkt"
         "parameter.rkt")

(provide simplify-path)

(define/who (simplify-path p-in [use-filesystem? #t])
  (check-path-argument who p-in)
  (define p (->path p-in))
  (define convention (path-convention p))
  (cond
   [(simple? p convention) p]
   [else
    (define clean-p (cleanse-path p))
    (cond
     [(simple? clean-p convention) clean-p]
     [else
      (define l (explode-path clean-p))
      (define simple-p
        (cond
          [use-filesystem?
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
                (define target (resolve-path new-base))
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
                         (string-append (symbol->string who) ": cycle detected at link"
                                        "\n  link path: " (path->string new-base))
                         (current-continuation-marks))))
                     (values from-base (hash-set seen from-base #t))]))
                (define-values (next-base name dir?) (split-path from-base))
                (cond
                  [(not next-base)
                   ;; discard ".." after a root
                   (loop (cdr l) from-base '() new-seen)]
                  [else
                   (loop (cdr l) next-base '() new-seen)])]
               [else (loop (cdr l) base (cons (car l) accum) seen)]))]
          [else
           ;; Don't use the filesystem, so just remove
           ;; "." and ".." syntactically
           (define simpler-l
             (let loop ([l l] [accum null])
               (cond
                 [(null? l) (reverse accum)]
                 [(eq? 'same (car l)) (loop (cdr l) accum)]
                 [(and (eq? 'up (car l)) (pair? accum))
                  (loop (cdr l) (cdr accum))]
                 [else (loop (cdr l) (cons (car l) accum))])))
           (apply build-path simpler-l)]))
      (if (directory-path? p)
          (path->directory-path simple-p)
          simple-p)])]))

;; ----------------------------------------

;; Quick check for whether the path is already simple:
(define (simple? p convention)
  (define bstr (path-bytes p))
  (define len (bytes-length bstr))
  (let loop ([i 0])
    (cond
     [(= i len) #t]
     [(is-sep? (bytes-ref bstr i) convention)
      (cond
       [(= (add1 i) len) #t]
       [(is-sep? (bytes-ref bstr (add1 i)) convention)
        #f]
       [(and (eq? (bytes-ref bstr (add1 i)) (char->integer #\.))
             (or (= (+ i 2) len)
                 (is-sep? (bytes-ref bstr (+ i 2)) convention)
                 (and (eq? (bytes-ref bstr (+ i 2)) (char->integer #\.))
                      (or (= (+ i 3) len)
                          (is-sep? (bytes-ref bstr (+ i 3)) convention)))))
        #f]
       [else (loop (add1 i))])]
     [(and (zero? i)
           (eq? (bytes-ref bstr 0) (char->integer #\.))
           (or (= 1 len)
               (is-sep? (bytes-ref bstr 1) convention)
               (and (eq? (bytes-ref bstr 1) (char->integer #\.))
                    (or (= 2 len)
                        (is-sep? (bytes-ref bstr 2) convention)))))
      #f]
     [else (loop (add1 i))])))
