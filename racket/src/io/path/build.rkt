#lang racket/base
(require "../locale/string.rkt"
         "check.rkt"
         "path.rkt"
         "sep.rkt"
         "windows.rkt")

(provide build-path
         build-path/convention-type)

(define (build-path base . subs)
  (build 'build-path #f base subs))

(define (build-path/convention-type convention base . subs)
  (build 'build-path/convention-type convention base subs))

(define (build who init-convention base subs)
  (check-build-path-arg who base)
  (define convention
    (let loop ([convention (argument->convention base init-convention who #:first? #t)]
               [subs subs])
      (cond
       [(null? subs) convention]
       [else
        (define sub (car subs))
        (check-build-path-arg who sub)
        (loop (argument->convention sub convention who #:first? #f)
              (cdr subs))])))
  (path (append-path-parts convention who base subs)
        convention))

;; ----------------------------------------

(define (check-build-path-arg who p)
  (check who
         (lambda (p) (or (path-string? p)
                    (path-for-some-system? p)
                    (eq? p 'up)
                    (eq? p 'same)))
         #:contract "(or/c path-string? path-for-some-system? 'up 'same)"
         p))

(define (argument->convention p convention who #:first? first?)
  (define (check c)
    (when (and convention (not (eq? c convention)))
      (raise-arguments-error who
                             (format
                              (if first?
                                  "specified convention incompatible with ~a path element"
                                  "preceding path's convention incompatible with ~a path element")
                              (if (string? p)
                                  "string"
                                  "given"))
                             "path element" p
                             (if first? "convention" "preceding path's convention")
                             convention))
    c)
  (cond
   [(path? p) (check (path-convention p))]
   [(string? p) (check (system-path-convention-type))]
   [else (or convention (system-path-convention-type))]))

;; ----------------------------------------

(define (append-path-parts convention who base subs)
  (define result-is-backslash-backslash-questionmark?
    (and (eq? convention 'windows)
         (for/or ([sub (in-list (cons base subs))])
           (backslash-backslash-questionmark? (as-bytes sub)))))
  (define base-accum
    (let ([bstr (as-bytes base)])
      (cond
        [(eq? convention 'windows)
         (if result-is-backslash-backslash-questionmark?
             (convert-to-initial-backslash-backslash-questionmark bstr)
             (list (strip-trailing-spaces bstr)))]
        [else (list bstr)])))
  ;; The `accum` list accumulates byte strings in reverse order to be
  ;; appended. On Windows in \\?\ mode, each byte string corresponds
  ;; to a single path element with a leading backslash, except that
  ;; the last item is a arting-point`; otherwise, the byte strings can
  ;; be a mixture of compound path elements and separators
  (let loop ([accum base-accum] [subs subs] [first? #t])
    (cond
      [(null? subs)
       (define elems (reverse accum))
       (combine-build-elements elems)]
      [else
       (define sub (car subs))
       (define bstr (as-bytes sub))
       (case convention
         [(unix)
          ;; Unix is fairly straightforward
          (when (is-sep? (bytes-ref bstr 0) 'unix)
            (raise-arguments-error who
                                   "absolute path cannot be added to a path"
                                   "absolute path" sub))
          (define prev (car accum))
          (cond
            [(is-sep? (bytes-ref prev (sub1 (bytes-length prev))) 'unix)
             (loop (cons bstr accum) (cdr subs) #f)]
            [else
             (loop (list* bstr #"/" accum) (cdr subs) #f)])]
         [(windows)
          ;; For Windows, the implementation immediately here is
          ;; mostly error checking, and actual combining work is in
          ;; `combine-windows-path`
          (define len (bytes-length bstr))
          (define (combine is-rel? is-complete? is-drive?)
            (when (or is-complete?
                      (and (not is-rel?)
                           (not first?)
                           (not (and (null? (cdr accum))
                                     (drive? (car accum))))))
              (define what (if is-drive? "drive" "absolute path"))
              (raise-arguments-error who
                                     (string-append what " cannot be added to a base path")
                                     what sub
                                     "base path" (path (combine-build-elements (reverse accum))
                                                       'windows)))
            (loop (combine-windows-path (if (and (null? subs)
                                                 ;; because \\?\ mode does its own stripping:
                                                 (not result-is-backslash-backslash-questionmark?))
                                            bstr
                                            (strip-trailing-spaces bstr))
                                        accum
                                        result-is-backslash-backslash-questionmark?)
                  (cdr subs)
                  #f))
          (cond
            [(is-sep? (bytes-ref bstr 0) 'windows)
             (cond
               [(backslash-backslash-questionmark? bstr)
                (define-values (kind drive-len orig-drive-len clean-start-pos add-sep-pos)
                  (parse-backslash-backslash-questionmark bstr))
                (combine (eq? kind 'rel)
                         (eq? kind 'abs)
                         (and (eq? kind 'abs)
                              (just-backslashes-after? bstr drive-len)))]
               [(parse-unc bstr 0)
                => (lambda (drive-len)
                     (combine #t #t (just-separators-after? bstr drive-len)))]
               [else
                (combine #f #f #f)])]
            [(letter-drive-start? bstr len)
             (combine #f #t (just-separators-after? bstr 2))]
            [else
             (combine #t #f #f)])])])))

(define (combine-windows-path bstr accum result-is-backslash-backslash-questionmark?)
  (cond
    [result-is-backslash-backslash-questionmark?
     ;; Split `bstr` into pieces, and handle the pieces one-by-one
     (let loop ([elems (windows-split-into-path-elements bstr)] [accum accum])
       (cond
         [(null? elems) accum]
         [else
          (define sub (car elems))
          (cond
            [(eq? 'same sub)
             ;; Ignore 'same for \\?\ mode
             (loop (cdr elems) accum)]
            [(eq? 'up sub)
             ;; Drop previous element for 'up in \\?\ mode
             (loop (cdr elems)
                   (if (null? (cdr accum))
                       (list (starting-point-add-up (car accum)))
                       (cdr accum)))]
            [else
             (loop (cdr elems) (cons sub accum))])]))]
    [else
     ;; Not in \\?\ mode, so `bstr` must not be a \\?\ path.
     ;; In case `accum` is drive-relative, start by dropping any
     ;; leading slashes.
     (define len (bytes-length bstr))
     (define sub (let loop ([i 0])
                   (cond
                     [(= i len) #""]
                     [(is-sep? (bytes-ref bstr i) 'windows)
                      (loop (add1 i))]
                     [(zero? i) bstr]
                     [else (subbytes bstr i)])))
     ;; Now, relatively simple: add a slash if needed between the parts
     (define prev-bstr (car accum))
     (define new-accum (if (is-sep? (bytes-ref prev-bstr (sub1 (bytes-length prev-bstr))) 'windows)
                           accum
                           (cons #"\\" accum)))
     (if (equal? sub #"") ; in case the argument was just "/"
         new-accum
         (cons sub new-accum))]))

(define (windows-split-into-path-elements bstr)
  (cond
    [(backslash-backslash-questionmark? bstr)
     ;; It must be REL or RED (with only a drive to build on)
     (define-values (dots-end literal-start)
       (backslash-backslash-questionmark-dot-ups-end bstr (bytes-length bstr)))
     (append (extract-dot-ups bstr 8 (or dots-end 8))
             (extract-separate-parts bstr literal-start #:bbq-mode? #t))]
    [else
     (extract-separate-parts bstr 0)]))

(define (as-bytes p)
  (cond
   [(eq? p 'up) #".."]
   [(eq? p 'same) #"."]
   [(path? p) (path-bytes p)]
   [else (string->bytes/locale p (char->integer #\?))]))

(define (just-separators-after? bstr drive-len)
  (for/and ([b (in-bytes bstr drive-len)])
    (is-sep? b 'windows)))

(define (just-backslashes-after? bstr drive-len)
  (for/and ([b (in-bytes bstr drive-len)])
    (eqv? b (char->integer #\\))))

;; Check whether `s`, a byte string or a `starting-point`,
;; is just a drive, in which case we can add a non-complete
;; absolute path
(define (drive? s)
  (cond
    [(starting-point? s) (starting-point-drive? s)]
    ;; must be a byte string
    [(parse-unc s 0)
     => (lambda (drive-len) (just-separators-after? s drive-len))]
    [(letter-drive-start? s (bytes-length s))
     (just-separators-after? s 2)]))

(struct starting-point (bstr        ; byte string that contains the starting path
                        len         ; number of bytes to use when adding more element
                        orig-len    ; number of bytes to use when not adding more elements
                        extra-sep   ; extra separator before first added element
                        add-ups?    ; whether to add `up`s to the base string, as opposed to dropping them
                        drive?))    ; is bstr an absolute root?

(define (make-starting-point bstr
                             len
                             #:orig-len [orig-len len]
                             #:extra-sep [extra-sep #""]
                             #:add-ups? [add-ups? #f]
                             #:drive? [drive? #t])
  (list
   (starting-point bstr len orig-len extra-sep add-ups? drive?)))

(define (combine-build-elements elems)
  (cond
    [(starting-point? (car elems))
     ;; in \\?\ mode for Windows
     (define s (car elems))
     (cond
       [(null? (cdr elems))
        (let ([bstr (subbytes (starting-point-bstr s)
                              0
                              (starting-point-orig-len s))])
          (cond
            [(equal? bstr #"\\\\?\\REL")
             #"."]
            [(equal? bstr #"\\\\?\\RED")
             #"\\"]
            [else bstr]))]
       [else
        (define init-bstr (subbytes (starting-point-bstr s)
                                    0
                                    (starting-point-len s)))
        (define rel-..-special-case? (and (bytes=? init-bstr #"\\\\?\\REL")
                                          (bytes=? (cadr elems) #"\\..")))
        (apply bytes-append
               init-bstr
               (if rel-..-special-case? ; => need extra `\` to indicate that ".." is not 'up
                   #"\\"
                   #"")
               (starting-point-extra-sep s)
               (cdr elems))])]
    [else
     ;; simple case
     (apply bytes-append elems)]))

(define (convert-to-initial-backslash-backslash-questionmark bstr)
  (cond
    [(backslash-backslash-questionmark? bstr)
     (define-values (kind drive-len orig-drive-len clean-start-pos add-sep)
       (parse-backslash-backslash-questionmark bstr))
     (case kind
       [(abs)
        (append (reverse (extract-separate-parts bstr drive-len #:bbq-mode? #t))
                (if (equal? add-sep #"")
                    ;; drop implicit terminator in drive:
                    (make-starting-point bstr (sub1 drive-len) #:orig-len orig-drive-len)
                    (make-starting-point bstr drive-len #:orig-len orig-drive-len #:extra-sep (subbytes add-sep 1))))]
       [else
        ;; We can't back up over any dots before `dots-end`,
        ;; so keep those toegether with \\?\REL
        (define-values (dots-end literal-start)
          (backslash-backslash-questionmark-dot-ups-end bstr (bytes-length bstr)))
        (append (reverse (extract-separate-parts bstr literal-start #:bbq-mode? #t))
                (make-starting-point bstr (or dots-end 7) #:add-ups? (eq? kind 'rel) #:drive? #f))])]
    [(parse-unc bstr 0)
     => (lambda (root-len)
          (define-values (machine volume)
            (let ([l (extract-separate-parts (subbytes bstr 0 root-len) 0)])
              (values (car l) (cadr l))))
          (append (reverse (simplify-dots (extract-separate-parts bstr root-len) #:drop-leading? #t))
                  (let* ([unc-bstr (bytes-append #"\\\\?\\UNC" machine volume)]
                         [unc-len (bytes-length unc-bstr)])
                    (make-starting-point unc-bstr unc-len))))]
    [(bytes=? #"." bstr)
     (make-starting-point #"\\\\?\\REL" 7 #:add-ups? #t #:drive? #f)]
    [(bytes=? #".." bstr)
     (make-starting-point #"\\\\?\\REL\\.." 10  #:add-ups? #t #:drive? #f)]
    [(is-sep? (bytes-ref bstr 0) 'windows)
     (append (reverse (extract-separate-parts bstr 0))
             (make-starting-point #"\\\\?\\RED" 7 #:drive? #f))]
    [(and ((bytes-length bstr) . >= . 2)
          (drive-letter? (bytes-ref bstr 0))
          (eqv? (bytes-ref bstr 1) (char->integer #\:)))
     (append (reverse (simplify-dots (extract-separate-parts bstr 2) #:drop-leading? #t))
             (let ([drive-bstr (bytes-append #"\\\\?\\" (subbytes bstr 0 2) #"\\")])
               (make-starting-point drive-bstr 6 #:orig-len 7)))]
    [else
     ;; Create \\?\REL, combinding any leading dots into the \\?\REL part:
     (define elems (simplify-dots (extract-separate-parts bstr 2) #:drop-leading? #f))
     (let loop ([dots null] [elems elems])
       (cond
         [(or (null? elems)
              (not (equal? (car elems) 'up)))
          (append (reverse elems)
                  (let* ([rel-bstr (apply bytes-append #"\\\\?\\REL" dots)]
                         [rel-len (bytes-length rel-bstr)])
                    (make-starting-point rel-bstr rel-len #:add-ups? #t #:drive? #f)))]
         [else
          (loop (cons (car elems) dots) (cdr elems))]))]))

;; Split on separators, removing trailing whitespace from the last
;; element, and prefix each extracted element with a backslash:
(define (extract-separate-parts bstr pos #:bbq-mode? [bbq-mode? #f])
  (define (is-a-sep? b)
    (if bbq-mode?
        (eqv? b (char->integer #\\))
        (is-sep? b 'windows)))
  (define len (bytes-length bstr))
  (let loop ([pos pos])
    (cond
      [(= pos len) null]
      [(is-a-sep? (bytes-ref bstr pos))
       (loop (add1 pos))]
      [else
       (let e-loop ([end-pos (add1 pos)])
         (cond
           [(or (= end-pos len)
                (is-a-sep? (bytes-ref bstr end-pos)))
            (define rest (loop end-pos))
            (define elem-bstr (subbytes bstr pos end-pos))
            (define new-bstr (if (and (null? rest)
                                      (not bbq-mode?))
                                 (strip-trailing-spaces elem-bstr)
                                 elem-bstr))
            (define new-sub (cond
                              [(and (not bbq-mode?)
                                    (bytes=? new-bstr #"."))
                               'same]
                              [(and (not bbq-mode?)
                                    (bytes=? new-bstr #".."))
                               'up]
                              [else
                               (bytes-append #"\\" new-bstr)]))
            (cons new-sub rest)]
           [else (e-loop (add1 end-pos))]))])))

;; Create a list containing one 'up for each ".." in the range:
(define (extract-dot-ups bstr start dots-end)
  (if (= start dots-end)
      '()
      (let loop ([i (add1 start)])
        (cond
          [(i . >= . dots-end) '()]
          [(and (eqv? (bytes-ref bstr i) (char->integer #\.))
                (eqv? (bytes-ref bstr (sub1 i)) (char->integer #\.)))
           (cons 'up (loop (add1 i)))]
          [else (loop (add1 i))]))))

;; For \\?\REL paths, add an 'up at the start to the initial path.
;; Otherwise, at a root, just drop an 'up.
(define (starting-point-add-up s)
  (cond
    [(starting-point-add-ups? s)
     (define bstr (bytes-append (subbytes (starting-point-bstr s)
                                          0
                                          (starting-point-len s))
                                #"\\.."))
     (define len (bytes-length bstr))
     (struct-copy starting-point s
                  [bstr bstr]
                  [len len]
                  [orig-len len])]
    [else s]))

(define (simplify-dots bstrs #:drop-leading? [drop-leading? #t])
  (let loop ([bstrs bstrs] [accum null])
    (cond
      [(null? bstrs) (reverse accum)]
      [(eq? 'up (car bstrs)) (loop (cdr bstrs) accum)]
      [(eq? 'same (car bstrs)) (if (null? accum)
                                   (if drop-leading?
                                       (loop (cdr bstrs) accum)
                                       (loop (cdr bstrs) (cons (car bstrs) accum)))
                                   (loop (cdr bstrs) (cdr accum)))]
      [else (loop (cdr bstrs) (cons (car bstrs) accum))])))
