#lang racket/base
(require racket/fixnum
         "path.rkt"
         "sep.rkt"
         "split.rkt"
         "build.rkt"
         "cleanse.rkt"
         "directory-path.rkt"
         "complete.rkt"
         "parameter.rkt"
         "windows.rkt"
         "relativity.rkt")

(provide simplify-path-syntactically)

(define (simplify-path-syntactically who p [use-filesystem #f])
  (define convention (path-convention p))
  (when use-filesystem
    (unless (eq? convention (system-path-convention-type))
      (raise-arguments-error who
                             "in use-filesystem mode, path is not for the current platform"
                             "path" p)))
  (cond
   [(simple? p convention) p]
   [else
    (define clean-p (cleanse-path/convert-slashes p))
    (cond
      [(simple? clean-p convention)
       ;; The choice of creating a complete path in this case seems like
       ;; it was probably the wrong one. The special treatement for Windows
       ;; reflects a compromise between consistency and old behavior (where
       ;; the conversion to a complete path did not happen in the old behavior)
       (if (or (not use-filesystem)
               (and (eq? 'windows (system-type))
                    (same-modulo-slashes? p clean-p)))
           clean-p
           (path->complete-path clean-p (current-directory)))]
      [else
       (define l (explode-path clean-p))
       (define simple-p
         (cond
           [use-filesystem
            ;; Call argument to handle filesystem interaction
            (use-filesystem who l)]
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
                      (cond
                        [(and (null? (cdr accum))
                              (absolute-path? (car accum)))
                         ;; for 'up at root, just keep the root
                         (loop (cdr l) accum)]
                        [else
                         (loop (cdr l) (cdr accum))])]
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
          (and (= len 2)
               (letter-drive-start? bstr 2)))
     ;; Letter drive without trailing separator
     #f]
    [(and (eq? convention 'windows)
          (backslash-backslash-questionmark-simple-status bstr))
     => (lambda (status)
          (eq? status 'simple))]
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

(define (backslash-backslash-questionmark-simple-status bstr)
  (define-values (kind drive-len orig-drive-len clean-start-pos sep-bstr)
    (parse-backslash-backslash-questionmark bstr))
  (cond
    [(not kind) #f]
    [(and (fx= (bytes-ref bstr 4) (char->integer #\R))
          (fx= (bytes-ref bstr 5) (char->integer #\E)))
     ;; For \\?\REL\ and \\?\RED\ paths, in use-filesystem mode,
     ;; we don't want to convert to a complete path unless there's
     ;; some simplification possible. Rebuild the path to see
     ;; whether it's already normalized and simple.
     (let loop ([p (path bstr 'windows)] [accum '()] [as-dir? #f])
       (define-values (base-dir name dir?) (split-path p))
       (cond
         [(symbol? name) 'non-simple]
         [else
          (define new-accum (cons name accum))
          (define new-as-dir? (if (null? accum) dir? as-dir?))
          (cond
            [(path? base-dir) (loop base-dir new-accum new-as-dir?)]
            [else
             (define rebuilt0-p (apply build-path/convention-type 'windows new-accum))
             (define rebuilt-p (if new-as-dir?
                                   (path (bytes-append (path-bytes rebuilt0-p) #"\\") 'windows)
                                   rebuilt0-p))
             (define rebuilt-bstr (path-bytes (simplify-backslash-backslash-questionmark rebuilt-p)))
             (if (bytes=? bstr rebuilt-bstr)
                 'simple
                 'non-simple)])]))]
     
    [else
     ;; Conservatively assume non-simple
     'non-simple]))

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

(define (same-modulo-slashes? p1 p2)
  (define bstr1 (path-bytes p1))
  (define bstr2 (path-bytes p2))
  (define len (bytes-length bstr1))
  (and (fx= len (bytes-length bstr2))
       (let loop ([i 0])
         (or (fx= i len)
             (and (let ([b1 (bytes-ref bstr1 i)]
                        [b2 (bytes-ref bstr2 i)])
                    (or (fx= b1 b2)
                        (and (fx= b1 (char->integer #\\))
                             (fx= b2 (char->integer #\/)))
                        (and (fx= b1 (char->integer #\/))
                             (fx= b2 (char->integer #\\)))))
                  (loop (fx+ i 1)))))))
