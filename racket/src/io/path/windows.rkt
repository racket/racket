#lang racket/base
(require "sep.rkt")

(provide special-filename?
         drive-letter?
         letter-drive-start?
         backslash-backslash-questionmark?
         backslash-backslash-questionmark-kind
         parse-backslash-backslash-questionmark
         parse-unc
         backslash-backslash-questionmark-dot-ups-end
         split-drive
         strip-trailing-spaces
         strip-backslash-backslash-rel)

(define special-filenames
  ;; and "CLOCK$" on NT --- but not traditionally detected by Racket
  '("NUL" "CON" "PRN" "AUX"
    "COM1" "COM2" "COM3" "COM4" "COM5" 
    "COM6" "COM7" "COM8" "COM9"
    "LPT1" "LPT2" "LPT3" "LPT4" "LPT5"
    "LPT6" "LPT7" "LPT8" "LPT9"))

(define (special-filename? in-bstr #:immediate? [immediate? #t])
  (define bstr (cond
                 [immediate? in-bstr]
                 [(backslash-backslash-questionmark? in-bstr) #""]
                 [else
                  ;; Extract bytes after last sep or after drive letter:
                  (define len (bytes-length in-bstr))
                  (let loop ([i+1 len])
                    (cond
                      [(zero? i+1)
                       (if (letter-drive-start? bstr len)
                           (subbytes in-bstr 2)
                           in-bstr)]
                      [else
                       (define i (sub1 i+1))
                       (if (is-sep? (bytes-ref in-bstr i) 'windows)
                           (subbytes in-bstr i+1)
                           (loop i))]))]))
  (define len (bytes-length bstr))
  (cond
    [(zero? len) #f]
    [(backslash-backslash-questionmark? bstr) #f]
    [else
     (for/or ([fn (in-list special-filenames)])
       ;; check for case-insensitive `fn` match followed by
       ;; '.' or ':' or (whitespace|'.')*
       (define fn-len (string-length fn))
       (and (len . >= . fn-len)
            (for/and ([c (in-string fn)]
                      [b (in-bytes bstr)])
              (or (eqv? (char->integer c) b)
                  (eqv? (char->integer (char-downcase c)) b)))
            (or (= len fn-len)
                (eqv? (bytes-ref bstr fn-len) (char->integer #\.))
                (eqv? (bytes-ref bstr fn-len) (char->integer #\:))
                (for/and ([b (in-bytes bstr fn-len)])
                  (or (eqv? b (char->integer #\space))
                      (eqv? b (char->integer #\.)))))))]))

(define (drive-letter? c)
  (or (<= (char->integer #\a) c (char->integer #\z))
      (<= (char->integer #\A) c (char->integer #\Z))))

(define (letter-drive-start? bstr len)
  (and (len . >= . 2)
       (drive-letter? (bytes-ref bstr 0))
       (eqv? (bytes-ref bstr 1) (char->integer #\:))))

(define (backslash-backslash-questionmark? bstr)
  (define len (bytes-length bstr))
  (and (len . >= . 4)
       (eqv? (bytes-ref bstr 0) (char->integer #\\))
       (eqv? (bytes-ref bstr 1) (char->integer #\\))
       (eqv? (bytes-ref bstr 2) (char->integer #\?))
       (eqv? (bytes-ref bstr 3) (char->integer #\\))))

;; Returns #f, 'rel, 'red, 'unc, or 'abs
(define (backslash-backslash-questionmark-kind bstr)
  (define-values (kind drive-end-pos orig-drive-end-pos clean-start-pos add-sep-pos)
    (parse-backslash-backslash-questionmark bstr))
  kind)
  
;; Returns (values kind drive-len orig-drive-len clean-start-pos sep-bstr)
;;  where `kind` is #f, 'rel, 'red, or 'abs
;;
;; For 'abs, then `drive-len` is set to the length of the root
;; specification. For example, if the drive is terminated by \\\ (a
;; weird "root"), then `drive-len` is after the third \. If the drive
;; is \\?\C:\, then `drive-len` is after the last slash. In the case
;; of \\?\UNC\..., `drive-len` is after the UNC part as in
;; `parse-unc` (so it doesn't include a slash after the volume name).
;;
;; The `orig-drive-len` result is almost the same as `drive-len`,
;; but maybe longer. It preserves an artifact of the given specification:
;; a backslash after a \\?\UNC\<mahine>\<volume> drive, an extra
;; backslash after a \\?\<letter>:\ drive, etc.
;;
;; For 'abs, `clean-start-pos` is the position where it's ok to start
;; removing extra slashes. It's usually the same as `drive-len`. In
;; the case of a \\?\UNC\ path, `clean-start` is 7 (i.e., just after
;; that prefix). In the case of a \\?\REL\ or \\?\RED\ path,
;; `clean-start-pos` is the end of the string.
;;
;; For 'abs, the sep-bstr result is a byte string to insert after
;; the root to add further elements.
(define (parse-backslash-backslash-questionmark bstr)
  (cond
    [(not (backslash-backslash-questionmark? bstr))
     (values #f #f #f #f #f)]
    [else
     (define len (bytes-length bstr))
     ;; Allow one extra "\":
     (define base
       (if (and (len . >= . 5)
                (eqv? (bytes-ref bstr 4) (char->integer #\\)))
           5
           4))
     ;; If there are two backslashes in a row at the end, count everything
     ;; as the drive; there are two exceptions: two backslashes are ok
     ;; at the end in the form \\?\C:\\, and \\?\\\ is \\?\
     (define two-backslashes?
       (and (len . > . 5)
             (eqv? (bytes-ref bstr (sub1 len)) (char->integer #\\))
             (eqv? (bytes-ref bstr (- len 2)) (char->integer #\\))))
     (cond
       [(and two-backslashes?
             (= len 6))
        ;; \\?\ is the root
        (values 'abs 4 4 3 #"\\\\")]
       [(and two-backslashes?
             (or (not (= len (+ base 4)))
                 (not (and (len . > . base)
                           (drive-letter? (bytes-ref bstr base))))
                 (not (and (len . > . (add1 base))
                           (eqv? (bytes-ref bstr (add1 base)) (char->integer #\:))))))
        ;; Not the special case \\?\C:\\
        (values 'abs len len len
                ;; If not already three \s, preserve this root when
                ;; adding more:
                (if (not (eqv? (bytes-ref bstr (- len 3)) (char->integer #\\)))
                    #"\\"
                    #""))]
       ;; If there are three backslashes in a row, count everything
       ;; up to the slashes as the drive
       [(and (len . > . 6)
             (let loop ([i+1 len])
               (cond
                 [(= i+1 6) #f]
                 [else
                  (define i (sub1 i+1))
                  (if (and (eqv? (bytes-ref bstr i) (char->integer #\\))
                           (eqv? (bytes-ref bstr (- i 1)) (char->integer #\\))
                           (eqv? (bytes-ref bstr (- i 2)) (char->integer #\\)))
                      i
                      (loop i))])))
        => (lambda (i)
             (define i+1 (add1 i))
             (values 'abs i i+1 i+1 #""))]
       ;; Check for drive-letter case
       [(and (len . > . 6)
             (drive-letter? (bytes-ref bstr base))
             (eqv? (bytes-ref bstr (add1 base)) (char->integer #\:))
             (len . > . (+ 2 base))
             (eqv? (bytes-ref bstr (+ 2 base)) (char->integer #\\)))
        (define drive-len (+ base 3))
        (define orig-drive-len (if (and (len . > . drive-len)
                                        (eqv? (bytes-ref bstr drive-len) (char->integer #\\)))
                                   (add1 drive-len)
                                   drive-len))
        (values 'abs drive-len orig-drive-len (+ base 2) #"")]
       ;; Check for UNC
       [(and (len . > . (+ base 3))
             (let ([b (bytes-ref bstr base)])
               (or (eqv? b (char->integer #\U)) (eqv? b (char->integer #\u))))
             (let ([b (bytes-ref bstr (add1 base))])
               (or (eqv? b (char->integer #\N)) (eqv? b (char->integer #\n))))
             (let ([b (bytes-ref bstr (+ base 2))])
               (or (eqv? b (char->integer #\C)) (eqv? b (char->integer #\c))))
             (eqv? (bytes-ref bstr (+ 3 base)) (char->integer #\\))
             (parse-unc bstr #:no-forward-slash? #t
                        (if (and (len . > . (+ base 4))
                                 (eqv? (bytes-ref bstr (+ 4 base)) (char->integer #\\)))
                            (+ base 5)
                            (+ base 4))))
        => (lambda (drive-len)
             (define orig-drive-len
               (if (and (len . > . drive-len)
                        (eqv? (bytes-ref bstr drive-len) (char->integer #\\)))
                   (add1 drive-len)
                   drive-len))
             (values 'unc drive-len orig-drive-len (+ base 3) #"\\"))]
       ;; Check for REL and RED
       [(and (= base 4)
             (len . > . 8)
             (eqv? (bytes-ref bstr 4) (char->integer #\R))
             (eqv? (bytes-ref bstr 5) (char->integer #\E))
             (let ([b (bytes-ref bstr 6)])
               (or (eqv? b (char->integer #\L))
                   (eqv? b (char->integer #\D))))
             (eqv? (bytes-ref bstr 7) (char->integer #\\))
             (or (not (eqv? (bytes-ref bstr 8) (char->integer #\\)))
                 (len . > . 9)))
        (values (if (eqv? (bytes-ref bstr 6) (char->integer #\L))
                    'rel
                    'red)
                #f
                #f
                #f
                #f)]
       ;; Otherwise, \\?\ is the (non-existent) drive
       [else
        ;; Can have up to two separators between the drive and first element
        (define orig-drive-len (if (and (len . > . 4)
                                        (eqv? (bytes-ref bstr 4) (char->integer #\\)))
                                   (if (and (len . > . 5)
                                            (eqv? (bytes-ref bstr 5) (char->integer #\\)))
                                       6
                                       5)
                                   4))
        (define clean-start-pos
          (if (or (and (= len 5)
                       (eqv? (bytes-ref bstr 4) (char->integer #\\)))
                  (and (= len 6)
                       (eqv? (bytes-ref bstr 4) (char->integer #\\))
                       (eqv? (bytes-ref bstr 5) (char->integer #\\))))
              3
              orig-drive-len))
        (values 'abs 4 orig-drive-len clean-start-pos #"\\\\")])]))

;; Returns an integer if this path is a UNC path, #f otherwise.
;; If `delta` is non-0, then `delta` is after a leading \\.
;; (It starts by checking for \\?\ paths, so they won't be
;; treated as UNC. Unless delta is non-0, in which case the
;; check isn't necessary, presumably because the original
;; `next' already started with \\?\UNC\.)
;; An integer result is set to the length (including offset) of
;; the \\server\vol part; which means that it's either the length of
;; the given byte string or a position that has a separator.
;; If `exact?`, then an integer is returned only if `bstr' is just the
;; drive; that is, only if only slashes are
;; in `bstr' starting with the result integer.
;; If `no-forward-slash?', then only backslashes are recognized.
(define (parse-unc bstr delta
                   #:exact? [exact? #f]
                   #:no-forward-slash? [no-forward-slash? #f])
  (cond
    [(and (zero? delta)
          (backslash-backslash-questionmark? bstr))
     #f]
    ;; Bail out fast on an easy non-match:
    [(and (zero? delta)
          (not
           (and ((bytes-length bstr) . > . 2)
                (is-sep? (bytes-ref bstr 0) 'windows)
                (is-sep? (bytes-ref bstr 1) 'windows))))
     #f]
    [else
     ;; Check for a drive form: //x/y
     (define (is-a-sep? c) (if no-forward-slash?
                               (eqv? c (char->integer #\\))
                               (is-sep? c 'windows)))
     (define len (bytes-length bstr))
     (define j (if (zero? delta) 2 delta))
     (and
      (not (and (len . > . j)
                (is-a-sep? (bytes-ref bstr j))))
      ;; Found non-sep; skip over more
      (let loop ([j j])
        (cond
          [(= j len)
           ;; Didn't find a sep, so not //x/
           #f]
          [(not (is-a-sep? (bytes-ref bstr j)))
           (cond
             [(and no-forward-slash?
                   (eqv? (bytes-ref bstr j) (char->integer #\/)))
              ;; Found / when only \ is allowed as separator
              #f]
             [else
              ;; Keep looking
              (loop (add1 j))])]
          [else
           ;; Found sep again, so we have //x/:
           (let* ([j (add1 j)]
                  [j (if (and no-forward-slash?
                              (j . < . len)
                              (is-a-sep? (bytes-ref bstr j)))
                         ;; two backslashes ok in \\?\UNC mode
                         (add1 j)
                         j)])
             (cond
               [(and (= j (if (zero? delta) 4 (+ delta 2)))
                     (eqv? (bytes-ref bstr (- j 2)) (char->integer #\?)))
                ;; We have //?/, with up to 2 backslashes.
                ;; This doesn't count as UNC, to avoid confusion with \\?\.
                #f]
               [(and (not no-forward-slash?)
                     (j . < . len)
                     (is-a-sep? (bytes-ref bstr j)))
                ;; Extra backslash not allowed after //<machine>/<drive> when not in \\?\ mode
                #f]
               [else
                (let loop ([j j])
                  (cond
                    [(= j len)
                     ;; Didn't find a non-sep, so not UNC
                     #f]
                    [(is-a-sep? (bytes-ref bstr j))
                     ;; Keep looking for non-sep
                     (loop (add1 j))]
                    [else
                     ;; Found non-sep again; this is UNC
                     (let loop ([j j])
                       (cond
                         [(= j len)
                          ;; Whole string is drive
                          len]
                         [(is-a-sep? (bytes-ref bstr j))
                          ;; Found sep that ends UNC drive
                          (and (or (not exact?)
                                   ;; Make sure there are no more separators:
                                   (for/and ([b (in-bytes bstr (add1 j))])
                                     (not (is-a-sep? b))))
                               j)]
                         [else (loop (add1 j))]))]))]))])))]))

;; Assumes `bstr` is of the form \\?\REL or \\?\RED and returns
;;   (values dots-end literal-start)
;; If `bstr` is \\?\REL\..\..\.., the  `dots-end` result is the index just
;;  past the last "\..". This might be the first "\" of a "\\"
;;  separator, the "\" before a non-".." element, or the end of the
;;  string. For a \\?\RED\ path, it's as if there are no ".."s
;;  (because ".." is not special in "RED" paths). Otherwise, `dots-end`
;;  is #f.
;; The `literal-start` result is the starting index of the literal part of
;;  the path (i.e., after one or two slashes, possibly after dots).
(define (backslash-backslash-questionmark-dot-ups-end bstr len)
  (define pos
    (and (eqv? (bytes-ref bstr 6) (char->integer #\L))
         (let loop ([pos #f]
                    [j 7]) ;; \\?\REL\
           (cond
             [((+ j 3) . > . len)
              pos]
             [(and (eqv? (bytes-ref bstr j) (char->integer #\\))
                   (eqv? (bytes-ref bstr (+ j 1)) (char->integer #\.))
                   (eqv? (bytes-ref bstr (+ j 2)) (char->integer #\.))
                   (or (= len (+ j 3))
                       (eqv? (bytes-ref bstr (+ j 3)) (char->integer #\\))))
              (define j+3 (+ j 3))
              (loop j+3 j+3)]
             [else pos]))))
  (cond
    [pos
     (cond
       [(= pos len)
        (values pos len)]
       [(and ((+ pos 2) . < . len)
             (eqv? (bytes-ref bstr (add1 pos)) (char->integer #\\)))
        (values pos (+ pos 2))]
       [else
        (values pos (+ pos 1))])]
    [(len . > . 8)
     (cond
       [(eqv? (bytes-ref bstr 8) (char->integer #\\))
        (values #f 9)]
       [else
        (values #f 8)])]
    [else
     (values #f 8)]))

(define (split-drive bstr)
  (cond
    [(backslash-backslash-questionmark? bstr)
     (define-values (kind drive-len orig-drive-len clean-start-pos add-sep-pos)
       (parse-backslash-backslash-questionmark bstr))
     (subbytes bstr 0 drive-len)]
    [(parse-unc bstr 0)
     => (lambda (pos) (subbytes bstr 0 pos))]
    [else
     (subbytes bstr 0 (min 3 (bytes-length bstr)))]))


(define (strip-trailing-spaces bstr)
  (cond
    [(backslash-backslash-questionmark? bstr)
     ;; all spaces are significant, so don't strip them
     bstr]
    [else
     (define len (bytes-length bstr))
     ;; ignore/keep trailing separators
     (define len-before-seps
       (let loop ([i+1 len])
         (define i (sub1 i+1))
         (cond
           [(is-sep? (bytes-ref bstr i) 'windows)
            (if (zero? i)
                0
                (loop i))]
           [else i+1])))
     (let loop ([i+1 len-before-seps])
       (cond
         [(zero? i+1)
          ;; A path element that's all spaces; don't trim
          bstr]
         [else
          (define i (sub1 i+1))
          (define b (bytes-ref bstr i))
          (cond
            [(is-sep? b 'windows)
             ;; A path element that's all spaces; don't trim
             bstr]
            [(or (eqv? b (char->integer #\.))
                 (eqv? b (char->integer #\space)))
             (loop i)]
            [(= i+1 len-before-seps)
             ;; Nothing to trim
             bstr]
            [else
             ;; Trim
             (bytes-append (subbytes bstr 0 i+1)
                           (subbytes bstr len-before-seps len))])]))]))

(define (strip-backslash-backslash-rel bstr)
  (define-values (kind drive-end-pos orig-drive-end-pos clean-start-pos add-sep-pos)
    (parse-backslash-backslash-questionmark bstr))
  (case kind
    [(rel) (subbytes bstr (if (eqv? (bytes-ref bstr 8) (char->integer #\\))
                              9
                              8))]
    [else bstr]))
