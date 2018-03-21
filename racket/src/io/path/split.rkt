#lang racket/base
(require "../common/check.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt"
         "cleanse.rkt"
         "windows.rkt"
         "protect.rkt")

(provide split-path
         explode-path)

(define/who (split-path p)
  (check-path-argument who p)
  (split (->path p)))

(define/who (explode-path p)
  (check-path-argument who p)
  (reverse (split (->path p) #:explode? #t)))

;; ----------------------------------------

(define (split p #:explode? [explode? #f])
  (cond
   [(not (eq? (path-convention p) 'windows))
    (split-after-drive p #:explode? explode?)]
   [else
    ;; Look for a Windows drive spec, then (usually) continue
    ;; to `split-after-drive`:
    (define bstr (path-bytes p))
    (cond
     [(and ((bytes-length bstr) . > . 2)
           (is-sep? (bytes-ref bstr 0) 'windows)
           (is-sep? (bytes-ref bstr 1) 'windows))
      (define-values (//?-kind //?-drive-end //?-orig-drive-end) (parse-//?-drive bstr))
      (cond
       [//?-kind
        (define allow-double-before //?-drive-end)
        (cond
         [(or (eq? //?-kind 'rel)
              (eq? //?-kind 'red))
          ;; `\\?\REL\` or `\\?\RED\` path. Handle it directly as a special case
          (split-reld bstr #:explode? explode?)]
         [else
          (split-after-drive p
                             #:drive-end //?-orig-drive-end
                             #:keep-drive-end (if (eq? //?-kind 'unc)
                                                  //?-orig-drive-end
                                                  //?-drive-end)
                             #:allow-double-before //?-orig-drive-end
                             #:no-slash-sep? #t
                             #:no-up? #t
                             #:explode? explode?)])]
       [else
        (define //-drive-end (parse-//-drive bstr))
        (cond
          [//-drive-end
           (split-after-drive p
                              #:drive-end (cond
                                            [(and (//-drive-end . < . (bytes-length bstr))
                                                  (is-sep? (bytes-ref bstr //-drive-end) 'windows))
                                             (add1 //-drive-end)]
                                            [else //-drive-end])
                              #:allow-double-before 1
                              #:explode? explode?)]
          [else
           (split-after-drive p #:explode? explode?)])])]
     [(and ((bytes-length bstr) . > . 2)
           (drive-letter? (bytes-ref bstr 0))
           (eq? (bytes-ref bstr 1) (char->integer #\:)))
      (split-after-drive p
                         #:drive-end (cond
                                      [(and (2 . < . (bytes-length bstr))
                                            (is-sep? (bytes-ref bstr 2) 'windows))
                                       3]
                                      [else 2])
                         #:explode? explode?)]
     [else (split-after-drive p #:explode? explode?)])]))

;; ----------------------------------------

;; Find a separator to split on, avoiding the Windows drive portion of
;; a path
(define (split-after-drive p
                           #:len [in-len #f]
                           #:drive-end [drive-end 0]
                           #:keep-drive-end [keep-drive-end drive-end]
                           #:no-slash-sep? [no-slash-sep? #f]
                           #:no-up? [no-up? #f]
                           #:allow-double-before [allow-double-before 0]
                           #:explode? explode?)
  (define convention (path-convention p))
  ;; Consecutive slashes can cause all sorts of mischief, both for
  ;; finding a separtor and making an unintended result after splitting,
  ;; so clean them up as a first step
  (define bstr (if in-len
                   (path-bytes p)
                   (clean-double-slashes (path-bytes p) convention allow-double-before)))
  (define len (or in-len (bytes-length bstr)))
  
  (define-values (split-pos ends-sep?)
    (let loop ([i (sub1 len)] [ends-sep? #f])
      (cond
        [(i . < . drive-end)
         (if (and (positive? i)
		  (i . < . (sub1 len)))
             (values i ends-sep?)
             (values #f ends-sep?))]
	[else
	 (define sep?
	   (cond
	     [no-slash-sep? (eq? (bytes-ref bstr i) (char->integer #\\))]
	     [else (is-sep? (bytes-ref bstr i) convention)]))
	 (cond
	   [sep?
	    (if (i . < . (sub1 len))
		(values i ends-sep?)
		(loop (sub1 i) #t))]
	   [else
	    (loop (sub1 i) ends-sep?)])])))
  ;; The `split-pos` argument is #f or less than `(sub1 len)`

  (cond
   [(not split-pos)
    ;; No splitting available: relative or exactly a root
    (cond
     [(or (is-sep? (bytes-ref bstr 0) convention)
          (positive? drive-end))
      ;; root
      (define new-p (path (subbytes bstr 0 len) convention))
      (if explode?
          (list new-p)
          (values #f new-p #t))]
     [else
      ;; relative
      (define-values (name is-dir?) (split-tail bstr len 0
                                                convention
                                                #:ends-sep? ends-sep?
                                                #:no-up? no-up?))
      (if explode?
          (list name)
          (values 'relative name is-dir?))])]
   [else
    ;; Split at the discovered separator
    (define-values (name is-dir?) (split-tail bstr len (add1 split-pos)
                                              convention
                                              #:ends-sep? ends-sep?
                                              #:no-up? no-up?))
    (cond
     [(zero? split-pos)
      (define base (if (eq? (bytes-ref bstr 0) #\/)
                       (path #"/" convention)
                       (path (subbytes bstr 0 1) convention)))
      (cond
       [explode?
        (list name base)]
       [else
        (values base name is-dir?)])]
     [else
      ;; Is it possible that by removing the last path element, we'll
      ;; leave a directory path that needs conversion to \\?\ on
      ;; Windows? No: even if the remaining path ends in spaces and
      ;; "."s, the path separator will stay in place to make the
      ;; trailing spaces and "."s significant.
      (define-values (exposed-bstr exposed-len) (values bstr
                                                        (let ([len (add1 split-pos)])
                                                          (if (= len drive-end)
                                                              keep-drive-end
                                                              len))))
      (cond
       [explode?
        (cons name
              (split-after-drive (path exposed-bstr convention)
                                 #:explode? #t
                                 #:len exposed-len
                                 #:drive-end drive-end
                                 #:keep-drive-end keep-drive-end
                                 #:no-slash-sep? no-slash-sep?
                                 #:no-up? no-up?
                                 #:allow-double-before allow-double-before))]
       [else
        (define base (path (subbytes exposed-bstr 0 exposed-len) convention))
        (values base name is-dir?)])])]))

;; ----------------------------------------

;; Extract a name and `is-dir?` result from the end of a path:
(define (split-tail bstr len start-pos
                    convention
                    #:ends-sep? ends-sep?
                    #:no-up? no-up?)
  (cond
   ;; check for 'up
   [(and (not no-up?)
         ((+ start-pos 2) . <= . len)
         (eq? (bytes-ref bstr start-pos) (char->integer #\.))
         (eq? (bytes-ref bstr (+ start-pos 1)) (char->integer #\.))
         (or ((+ start-pos 2) . = . len)
             (and ((+ start-pos 3) . = . len)
                  ends-sep?)))
    (values 'up #t)]
   ;; check for 'same
   [(and (not no-up?)
         ((+ start-pos 1) . <= . len)
         (eq? (bytes-ref bstr start-pos) (char->integer #\.))
         (or ((+ start-pos 1) . = . len)
             (and ((+ start-pos 2) . = . len)
                  ends-sep?)))
    (values 'same #t)]
   ;; other relative
   [else
    (define new-bstr (cond
                      [ends-sep?
                       (subbytes bstr start-pos (sub1 len))]
                      [(zero? start-pos)
                       (bytes->immutable-bytes bstr)]
                      [else  
                       (subbytes bstr start-pos)]))
    (define prot-bstr (if (or no-up? ends-sep?)
                          (protect-path-element new-bstr convention)
                          new-bstr))
    (values (path prot-bstr convention)
            ends-sep?)]))

;; ----------------------------------------

(define (parse-//?-drive bstr)
  (define-values (kind drive-len orig-drive-len clean-start-pos add-sep-pos)
    (parse-backslash-backslash-questionmark bstr))
  (values kind drive-len orig-drive-len))

(define (parse-//-drive bstr)
  (parse-unc bstr 0))

;; Splits a \\?\REL or \\?\RED path
(define (split-reld bstr #:explode? explode?)
  (let explode-loop ([bstr bstr])
    (define-values (len is-dir?)
      (let ([len (bytes-length bstr)])
        (cond
          [(eqv? (bytes-ref bstr (sub1 len)) (char->integer #\\))
           (values (sub1 len) #t)]
          [else
           (values len #f)])))
    (define-values (dots-end literal-start)
      (backslash-backslash-questionmark-dot-ups-end bstr len))
    (cond
      [(literal-start . < . len)
       ;; There's at least one literal path
       (let loop ([p (sub1 len)])
         (cond
           [(p . < . (if dots-end (sub1 literal-start) literal-start))
            ;; One one element and no dots
            (cond
              [(eqv? (bytes-ref bstr 6) (char->integer #\L))
               ;; keep \\?\REL\ on path, and report 'relative as base */
               (define elem (path (if is-dir? (subbytes bstr 0 len) bstr) 'windows))
               (cond
                 [explode? (list elem)]
                 [else (values 'relative
                               elem
                               is-dir?)])]
              [else
               ;; Switch "D" to "L", and simplify base to just "\\"
               (define base (path #"\\" 'windows))
               (define elem (path
                             (bytes-append #"\\\\?\\REL\\"
                                           (if (eqv? (bytes-ref bstr 8) (char->integer #\\))
                                               #""
                                               #"\\")
                                           (subbytes bstr 8))
                             'windows))
               (cond
                 [explode? (list elem base)]
                 [else (values base elem is-dir?)])])]
           [(eqv? (bytes-ref bstr p) (char->integer #\\))
            ;; Prefix path element with \\?\REL\\
            (define elem-bstr
              (bytes-append #"\\\\?\\REL\\\\"
                            (subbytes bstr (add1 p) len)))
            (define nsep
              (cond
                [(or (eqv? dots-end p) (eqv? dots-end (sub1 p)))
                 ;; stripping the only element: drop reundant separator(s) after ..
                 (if (eqv? dots-end p) 0 -1)]
                [(eqv? (bytes-ref bstr 6) (char->integer #\L))
                 ;; preserve separator
                 1]
                ;; preserve one separator, but not two
                [(eqv? (bytes-ref bstr (sub1 p)) (char->integer #\\))
                 0]
                [else 1]))
            (define base-bstr (subbytes bstr 0 (+ p nsep)))
            (define elem (path elem-bstr 'windows))
            (cond
              [explode? (cons elem (explode-loop base-bstr))]
              [else (values (path base-bstr 'windows) elem is-dir?)])]
           [else (loop (sub1 p))]))]
      [else
       ;; There are no literals --- just dots
       (cond
         [explode?
          (let loop ([dots-end dots-end])
            (cond
              [(dots-end . > . 9) (cons 'up (loop (- dots-end 3)))]
              [else '()]))]
         [((- dots-end 3) . > . 8)
          (values (path (subbytes bstr 0 (- dots-end 3)) 'windows)
                  'up
                  #t)]
         [else
          (values 'relative 'up #t)])])))
