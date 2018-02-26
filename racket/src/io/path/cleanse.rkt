#lang racket/base
(require "../common/check.rkt"
         "../common/internal-error.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt"
         "windows.rkt")

(provide cleanse-path
         clean-double-slashes)

(define/who (cleanse-path p-in)
  (check-path-argument who p-in)
  (define p (->path p-in))
  (define convention (path-convention p))
  (define (return bstr)
    (if (eq? bstr (path-bytes p))
        p
        (path bstr convention)))
  (define bstr (path-bytes p))
  (case convention
    [(unix)
     (return (clean-double-slashes bstr 'unix 0))]
    [(windows)
     (cond
       [(backslash-backslash-questionmark? bstr)
        (define-values (kind drive-len orig-drive-len clean-start-pos sep-bstr)
          (parse-backslash-backslash-questionmark (path-bytes p)))
        (cond
          [clean-start-pos
           (return (clean-double-slashes bstr 'windows clean-start-pos
                                         #:only-backslash? #t))]
          [else
           ;; Must be \\?\REL or \\?\RED
           (define-values (dots-end literal-start)
             (backslash-backslash-questionmark-dot-ups-end bstr (bytes-length bstr)))
           (define new-bstr (clean-double-slashes bstr 'windows literal-start
                                                  #:only-backslash? #t))
           (define has-extra-backslash?
             (and (eqv? (bytes-ref bstr (- literal-start 1)) (char->integer #\\))
                  (eqv? (bytes-ref bstr (- literal-start 2)) (char->integer #\\))))
           (cond
             [has-extra-backslash? (return new-bstr)]
             [else
              (return (bytes-append (subbytes new-bstr 0 literal-start)
                                    #"\\"
                                    (subbytes new-bstr literal-start)))])])]
       [(parse-unc bstr 0)
        => (lambda (drive-len)
             (return (clean-double-slashes bstr 'windows drive-len)))]
       [(letter-drive-start? bstr (bytes-length bstr))
        (cond
          [(and ((bytes-length bstr) . > . 2)
                (is-sep? (bytes-ref bstr 2) 'windows))
           (return (clean-double-slashes bstr 'windows 2))]
          [else
           (return (bytes-append (subbytes bstr 0 2)
                                 #"\\"
                                 (clean-double-slashes (subbytes bstr 2) 'windows 0)))])]
       [else
        (return (clean-double-slashes bstr 'windows 0))])]))

;; ----------------------------------------

(define (clean-double-slashes bstr convention allow-double-before
                              #:only-backslash? [only-backslash? #f])
  (define (is-a-sep? b)
    (if only-backslash?
        (eqv? b (char->integer #\\))
        (is-sep? b convention)))
  (define extra-count
    (let loop ([i (sub1 (bytes-length bstr))])
      (cond
       [(i . <= . allow-double-before) 0]
       [(and (is-a-sep? (bytes-ref bstr i))
             (is-a-sep? (bytes-ref bstr (sub1 i))))
        (add1 (loop (sub1 i)))]
       [else (loop (sub1 i))])))
  (cond
   [(zero? extra-count)
    bstr]
   [else
    (define new-bstr (make-bytes (- (bytes-length bstr) extra-count)))
    (let loop ([i (sub1 (bytes-length bstr))] [j (sub1 (bytes-length new-bstr))])
      (unless (i . <= . allow-double-before)
        (cond
         [(and (is-a-sep? (bytes-ref bstr i))
               (is-a-sep? (bytes-ref bstr (sub1 i))))
          (loop (sub1 i) j)]
         [else
          (bytes-set! new-bstr j (bytes-ref bstr i))
          (loop (sub1 i) (sub1 j))])))
    (bytes-copy! new-bstr 0 bstr 0 (add1 allow-double-before))
    new-bstr]))
