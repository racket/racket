#lang racket/base
(require "../file/main.rkt"
         (submod "../file/main.rkt" for-simplify)
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
         "parameter.rkt"
         "windows.rkt")

(provide simplify-path)

(define/who (simplify-path p-in [use-filesystem? #t])
  (check-path-argument who p-in)
  (define p (->path p-in))
  (define convention (path-convention p))
  (when use-filesystem?
    (unless (eq? convention (system-path-convention-type))
      (raise-arguments-error who
                             "in use-filesystem mode, path is not for the current platform"
                             "path" p)))
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
                  [(eq? 'up (car l))
                   (cond
                     [(pair? accum)
                      (loop (cdr l) (cdr accum))]
                     [else
                      (cons 'up (loop (cdr l) null))])]
                  [else (loop (cdr l) (cons (car l) accum))])))
            (apply build-path/convention-type convention (if (null? simpler-l) '(same) simpler-l))]))
       (define simpler-p (if (eq? convention 'windows)
                             (simplify-backslash-backslash-questionmark simple-p)
                             simple-p))
       (if (or (directory-path? p)
               (and (eq? convention 'windows)
                    (unc-without-trailing-separator? simpler-p)))
           (path->directory-path simpler-p)
           simpler-p)])]))

(void (set-simplify-path-for-directory-list! simplify-path))

;; ----------------------------------------

;; Quick check for whether the path is already simple:
(define (simple? p convention)
  (define bstr (path-bytes p))
  (define len (bytes-length bstr))
  (define (is-a-sep? b)
    (if (eq? convention 'windows)
        (eqv? b (char->integer #\\))
        (is-sep? b convention)))
  (cond
    [(and (eq? convention 'windows)
          (cond
            [(and
              (= len 2)
              (letter-drive-start? bstr 2))
             ;; Letter drive without trailing separator
             #t]
            [(non-normal-backslash-backslash-questionmark? bstr)
             #t]
            [else #f]))
     #f]
    [else
     (let loop ([i 0])
       (cond
         [(= i len) #t]
         [(is-a-sep? (bytes-ref bstr i))
          (cond
            [(= (add1 i) len) #t]
            [(is-a-sep? (bytes-ref bstr (add1 i)))
             #f]
            [(and (eqv? (bytes-ref bstr (add1 i)) (char->integer #\.))
                  (or (= (+ i 2) len)
                      (is-a-sep? (bytes-ref bstr (+ i 2)))
                      (and (eqv? (bytes-ref bstr (+ i 2)) (char->integer #\.))
                           (or (= (+ i 3) len)
                               (is-a-sep? (bytes-ref bstr (+ i 3)))))))
             #f]
            [else (loop (add1 i))])]
         [(and (zero? i)
               (eqv? (bytes-ref bstr 0) (char->integer #\.))
               (or (= 1 len)
                   (is-sep? (bytes-ref bstr 1) convention)
                   (and (eqv? (bytes-ref bstr 1) (char->integer #\.))
                        (or (= 2 len)
                            (is-sep? (bytes-ref bstr 2) convention)))))
          #f]
         [(and (eq? convention 'windows)
               (eqv? (bytes-ref bstr i) (char->integer #\/)))
          #f]
         [else (loop (add1 i))]))]))

(define (non-normal-backslash-backslash-questionmark? bstr)
  (define-values (kind drive-len orig-drive-len clean-start-pos sep-bstr)
    (parse-backslash-backslash-questionmark bstr))
  ;; We could try harder to recognize normal forms, but for now
  ;; we assume that some normalization is needed in a \\?\ path.
  kind)

;; ----------------------------------------

(define (unc-without-trailing-separator? p)
  (define bstr (path-bytes p))
  (eqv? (parse-unc bstr 0) (bytes-length bstr)))

;; Strip away "\\?\" when possible from an otherwise simplified `p`
(define (simplify-backslash-backslash-questionmark p)
  (define bstr (path-bytes p))
  (define len (bytes-length bstr))
  (define-values (kind drive-len orig-drive-len clean-start-pos sep-bstr)
    (parse-backslash-backslash-questionmark bstr))
  (define (special-element? elem-start i at-end?)
    (and (elem-start . < . i)
         (or (let ([b (bytes-ref bstr (sub1 i))])
               (or (and (eqv? b (char->integer #\.))
                        (or at-end?
                            (= elem-start (- i 1))
                            (and (= elem-start (- i 2))
                                 (eqv? (bytes-ref bstr elem-start) (char->integer #\.)))))
                   (and at-end?
                        (eqv? b (char->integer #\space)))))
             (special-filename? (subbytes bstr elem-start i)))))
  (define (no-special-in-content? start-pos #:len [len len])
    (let loop ([i start-pos] [elem-start start-pos])
      (cond
        [(= i len) (not (special-element? elem-start i #t))]
        [else
         (define b (bytes-ref bstr i))
         (cond
           [(eqv? b (char->integer #\\))
            (cond
              [(special-element? elem-start i #f) #f]
              [else (loop (add1 i) (add1 i))])]
           [(or (eqv? b (char->integer #\/))
                (eqv? b (char->integer #\:))
                (eqv? b (char->integer #\"))
                (eqv? b (char->integer #\|))
                (eqv? b (char->integer #\<))
                (eqv? b (char->integer #\>)))
            #f]
           [else (loop (add1 i) elem-start)])])))
  (case kind
    [(abs)
     (cond
       [(and (= drive-len 7)
             (drive-letter? (bytes-ref bstr 4))
             (eqv? (bytes-ref bstr 5) (char->integer #\:))
             (no-special-in-content? orig-drive-len))
        (path (subbytes bstr 4) 'windows)]
       [else p])]
    [(unc)
     (define norm-bstr (normalize-backslash-backslash-unc bstr))
     (cond
       [(no-special-in-content? 4 ; check UNC machine and drive, too
                                #:len (if (= orig-drive-len len)
                                          (sub1 len) ; stop before ending "\\"
                                          len))
        (path (bytes-append #"\\" (subbytes norm-bstr 7)) 'windows)]
       [(eq? norm-bstr bstr) p]
       [else (path norm-bstr 'windows)])]
    [(red)
     (cond
       [(no-special-in-content? 9)
        (path (subbytes bstr 8) 'windows)]
       [else p])]
    [(rel)
     (define-values (dots-end literal-start) (backslash-backslash-questionmark-dot-ups-end bstr len))
     (cond
       [(no-special-in-content? literal-start)
        ;; Remove any extra backslash for `dots-end`
        (path (bytes-append (if dots-end (subbytes bstr 8 (add1 dots-end)) #"")
                            (subbytes bstr literal-start))
              'windows)]
       [else p])]
    [else p]))

(define (normalize-backslash-backslash-unc bstr)
  ;; Normalize "UNC" case and single \ after \\?
  (cond
    [(and (eqv? (bytes-ref bstr 4) (char->integer #\U))
          (eqv? (bytes-ref bstr 5) (char->integer #\N))
          (eqv? (bytes-ref bstr 6) (char->integer #\C)))
     bstr]
    [(eqv? (bytes-ref bstr 4) (char->integer #\\))
     (bytes-append #"\\\\?\\UNC" (subbytes bstr 8))]
    [else
     (bytes-append #"\\\\?\\UNC" (subbytes bstr 7))]))
