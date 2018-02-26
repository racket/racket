#lang racket/base
(require "chyte.rkt"
         "chyte-case.rkt"
         "ast.rkt"
         "config.rkt"
         "error.rkt"
         "../common/range.rkt"
         "class.rkt"
         "unicode.rkt"
         "range.rkt"
         "case.rkt")

(provide parse)

(define (parse p #:px? [px? #f])
  (define config (make-parse-config #:px? px?))
  (define-values (rx pos) (parse-regexp p 0 config))
  (values rx
          (config-group-number config)
          (unbox (parse-config-references?-box config))))

;; Returns (values rx position)
(define (parse-regexp s pos config #:parse-regexp [parse-regexp (lambda (s pos config)
                                                                  (parse-regexp s pos config))])
  (define-values (rxs pos2) (parse-pces s pos config))
  (chyte-case/eos
   s pos2
   [(#\|)
    (define-values (rx pos3) (parse-regexp s (add1 pos2) config))
    (values (rx-alts (rx-sequence rxs) rx (chytes-limit s)) pos3)]
   [else
    (values (rx-sequence rxs) pos2)]))

(define (parse-regexp/maybe-empty s pos config)
  (chyte-case/eos
   s pos
   [(#\))
    (values rx:empty pos)]
   [else
    (parse-regexp s pos config #:parse-regexp parse-regexp/maybe-empty)]))

;; Returns (values list-of-rx position)
(define (parse-pces s pos config)
  (cond
   [(= pos (chytes-length s))
    (values null pos)]
   [else
    (define-values (rx pos2) (parse-pce s pos config))
    (chyte-case/eos
     s pos2
     [(eos)
      (values (list rx) pos2)]
     [(#\| #\))
      (values (list rx) pos2)]
     [else
      (define-values (rxs pos3) (parse-pces s pos2 config))
      (values (cons rx rxs) pos3)])]))

;; Returns (values rx position)
(define (parse-pce s pos config)
  (define-values (rx pos2) (parse-atom s pos config))
  (chyte-case/eos
   s pos2
   [(#\*)
    (define-values (non-greedy? pos3) (parse-non-greedy s (add1 pos2) config))
    (values (rx:repeat rx 0 +inf.0 non-greedy?) pos3)]
   [(#\+)
    (define-values (non-greedy? pos3) (parse-non-greedy s (add1 pos2) config))
    (values (rx:repeat rx 1 +inf.0 non-greedy?) pos3)]
   [(#\?)
    (define-values (non-greedy? pos3) (parse-non-greedy s (add1 pos2) config))
    (values (rx:maybe rx non-greedy?) pos3)]
   [(#\{)
    (cond
     [(parse-config-px? config)
      (define-values (n1 pos3) (parse-integer 0 s (add1 pos2) config))
      (chyte-case/eos
       s pos3
       [(#\,)
        (define-values (n2 pos4) (parse-integer 0 s (add1 pos3) config))
        (chyte-case/eos
         s pos4
         [(#\})
          (define n2* (if (= pos4 (add1 pos3)) +inf.0 n2))
          (define-values (non-greedy? pos5) (parse-non-greedy s (add1 pos4) config))
          (values (rx:repeat rx n1 n2* non-greedy?) pos5)]
         [else
          (parse-error s pos3 config "expected digit or `}` to end repetition specification started with `{`")])]
       [(#\})
        (define-values (non-greedy? pos4) (parse-non-greedy s (add1 pos3) config))
        (values (rx:repeat rx n1 n1 non-greedy?) pos4)]
       [else
        (parse-error s pos3 config "expected digit, `,`, or `}' for repetition specification started with `{`")])]
     [else
      (values rx pos2)])]
   [else
    (values rx pos2)]))

(define (parse-non-greedy s pos config)
  (chyte-case/eos
   s pos
   [(#\?)
    (values #t (check-not-nested s (add1 pos) config))]
   [else
    (values #f (check-not-nested s pos config))]))

(define (check-not-nested s pos config)
  (chyte-case/eos
   s pos
   [(#\? #\* #\+) 
    (parse-error s pos config
                 "nested `~a` in patten"
                 (integer->char (chytes-ref s pos)))]
   [(#\{)
    (when (parse-config-px? config)
      (parse-error s pos config
                   "nested `{` in pattern"))])
  pos)

;; Returns (values rx position)
(define (parse-atom s pos config)
  ;; Assumes at least one character
  (chyte-case
   (chytes-ref s pos)
   [(#\|)
    (values rx:empty pos)]
   [(#\()
    (parse-parenthesized-atom s (add1 pos) config)]
   [(#\[)
    (define-values (range pos2) (parse-range/not s (add1 pos) config))
    (values (rx-range range (chytes-limit s)) pos2)]
   [(#\.)
    (define rx (if (parse-config-multi-line? config)
                   (rx-range (range-invert (range-add empty-range (chyte #\newline))
                                           (chytes-limit s))
                             (chytes-limit s))
                   rx:any))
    (values rx (add1 pos))]
   [(#\^)
    (values (if (parse-config-multi-line? config) rx:line-start rx:start)
            (add1 pos))] 
   [(#\$)
    (values (if (parse-config-multi-line? config) rx:line-end rx:end)
            (add1 pos))]
   [else
    ;; Literal or (for px mode) `\` character class
    (parse-literal s pos config)]))

;; Returns (values rx position)
(define (parse-parenthesized-atom s pos config)
  (chyte-case/eos
   s pos
   [(eos)
    (missing-closing-error s pos config)]
   [(#\?)
    (define pos2 (add1 pos))
    (chyte-case/eos
     s pos2
     [(eos)
      (bad-?-sequence-error s pos2 config)]
     [(#\>)
      (define pre-num-groups (config-group-number config))
      (define-values (rx pos3) (parse-regexp/maybe-empty s (add1 pos2) config))
      (define post-num-groups (config-group-number config))
      (values (rx-cut rx pre-num-groups (- post-num-groups pre-num-groups))
              (check-close-paren s pos3 config))]
     [(#\()
      (parse-conditional s (add1 pos2) config)]
     [(#\i #\s #\m #\- #\:)
      (define-values (config2 pos3) (parse-mode s pos2 config))
      (chyte-case/eos
       s pos3
       [(#\:)
        (define-values (rx pos4) (parse-regexp/maybe-empty s (add1 pos3) config2))
        (values rx (check-close-paren s pos4 config2))]
       [else
        (parse-error s pos3 config2 (string-append
                                     "expected `:` or another mode after `(?` and a mode sequence;\n"
                                     " a mode is `i`, `-i`, `m`, `-m`, `s`, or `-s`"))])]
     [else
      (parse-look s pos2 config)])]
   [else
    (define group-number (config-group-number config))
    (define-values (rx pos2) (parse-regexp/maybe-empty s pos (config-group-number+1 config)))
    (values (rx-group rx group-number)
            (check-close-paren s pos2 config))]))

;; Returns (values rx position)
(define (parse-look s pos2 config)
  ;; known that one character is available
  (define pre-num-groups (config-group-number config))
  (define (span-num-groups) (- (config-group-number config) pre-num-groups))
  (chyte-case
   (chytes-ref s pos2)
   [(#\=)
    (define-values (rx pos3) (parse-regexp/maybe-empty s (add1 pos2) config))
    (values (rx:lookahead rx #t pre-num-groups (span-num-groups))
            (check-close-paren s pos3 config))]
   [(#\!)
    (define-values (rx pos3) (parse-regexp/maybe-empty s (add1 pos2) config))
    (values (rx:lookahead rx #f pre-num-groups (span-num-groups))
            (check-close-paren s pos3 config))]
   [(#\<)
    (define pos2+ (add1 pos2))
    (chyte-case/eos
     s pos2+
     [(eos)
      (bad-?-sequence-error s pos2+ config)]
     [(#\=)
      (define-values (rx pos3) (parse-regexp/maybe-empty s (add1 pos2+) config))
      (values (rx:lookbehind rx #t 0 0 pre-num-groups (span-num-groups))
              (check-close-paren s pos3 config))]
     [(#\!)
      (define-values (rx pos3) (parse-regexp/maybe-empty s (add1 pos2+) config))
      (values (rx:lookbehind rx #f 0 0 pre-num-groups (span-num-groups))
              (check-close-paren s pos3 config))]
     [else
      (bad-?-sequence-error s pos2+ config)])]
   [else
    (bad-?-sequence-error s pos2 config)]))

;; Returns (values rx position)
(define (parse-conditional s pos config)
  (define tst-pre-num-groups (config-group-number config))
  (define-values (tst pos2) (parse-test s pos config))
  (define tst-span-num-groups (- (config-group-number config) tst-pre-num-groups))
  (define-values (pces pos3) (parse-pces s pos2 config))
  (chyte-case/eos
   s pos3
   [(eos)
    (missing-closing-error s pos3 config)]
   [(#\|)
    (define-values (pces2 pos4) (parse-pces s (add1 pos3) config))
    (chyte-case/eos
     s pos4
     [(eos)
      (missing-closing-error s pos4 config)]
     [(#\))
      (values (rx-conditional tst (rx-sequence pces) (rx-sequence pces2)
                              tst-pre-num-groups tst-span-num-groups)
              (add1 pos4))]
     [else
      (parse-error s pos4 config "expected `)` to close `(?(...)...` after second branch")])]
   [(#\))
    (values (rx-conditional tst (rx-sequence pces) rx:empty
                            tst-pre-num-groups tst-span-num-groups)
            (add1 pos3))]))

;; Returns (values rx position)
(define (parse-test s pos config)
  (chyte-case/eos
   s pos
   [(eos)
    (missing-closing-error s pos config)]
   [(#\?)
    (parse-look s (add1 pos) config)]
   [else
    (define c (chytes-ref s pos))
    (cond
     [(and (>= c (chyte #\0)) (<= c (chyte #\9)))
      (set-box! (parse-config-references?-box config) #t)
      (define-values (n pos3) (parse-integer 0 s pos config))
      (unless (and (pos3 . < . (chytes-length s))
                   (= (chytes-ref s pos3) (chyte #\))))
        (parse-error s pos3 config "expected `)` after `(?(` followed by digits"))
      (values (rx:reference n #f) (add1 pos3))]
     [else
      (parse-error s pos config "expected `(?=`, `(?!`, `(?<`, or digit after `(?(`")])]))

;; Returns (values n position)
(define (parse-integer n s pos config)
  (cond
   [(= pos (chytes-length s))
    (values n pos)]
   [else
    (define c (chytes-ref s pos))
    (cond
     [(and (>= c (chyte #\0)) (<= c (chyte #\9)))
      (define n2 (+ (* n 10) (- c (chyte #\0))))
      (parse-integer n2 s (add1 pos) config)]
     [else
      (values n pos)])]))

;; Returns (values rx position)
(define (parse-literal s pos config)
  ;; Assumes at least one character;
  ;; we don't get here for `(`, `[`, `.`, `^`, `$`, or `|`
  (define c (chytes-ref s pos))
  (chyte-case
   c
   [(#\* #\+ #\?)
    (parse-error s pos config "`~a` follows nothing in pattern" (integer->char c))]
   [(#\{)
    (cond
     [(parse-config-px? config)
      (parse-error s pos config "`{` follows nothing in pattern")]
     [else (values c (add1 pos))])]
   [(#\\)
    ;; escaped character
    (parse-backslash-literal s (add1 pos) config)]
   [(#\))
    (parse-error s pos config "unmatched `)` in pattern")]
   [(#\] #\})
    (cond
     [(parse-config-px? config)
      (parse-error s pos config "unmatched `~a` in pattern" (integer->char c))]
     [else (values c (add1 pos))])]
   [else
    (cond
     [(parse-config-case-sensitive? config)
      (values c (add1 pos))]
     [else
      ;; case-insensitive char match
      (values (rx-range (range-add* empty-range c config) (chytes-limit s))
              (add1 pos))])]))

(define (parse-backslash-literal s pos2 config)
  (cond
   [(= pos2 (chytes-length s))
    ;; An "expected character after `\`" error would make more sense,
    ;; but the old expander produced a match against the nul character
    (values (chyte #\u0) pos2)]
   [else
    (define c2 (chytes-ref s pos2))
    (cond
     [(and (parse-config-px? config)
           (and (>= c2 (chyte #\0)) (<= c2 (chyte #\9))))
      (set-box! (parse-config-references?-box config) #t)
      (define-values (n pos3) (parse-integer 0 s pos2 config))
      (values (rx:reference n (parse-config-case-sensitive? config)) pos3)]
     [(and (parse-config-px? config)
           (or (and (>= c2 (chyte #\a)) (<= c2 (chyte #\z)))
               (and (>= c2 (chyte #\A)) (<= c2 (chyte #\Z)))))
      (chyte-case
       c2
       [(#\p #\P)
        (parse-unicode-categories c2 s (add1 pos2) config)]
       [(#\b)
        (values rx:word-boundary (add1 pos2))]
       [(#\B)
        (values rx:not-word-boundary (add1 pos2))]
       [else
        (define-values (success? range pos3) (parse-class s pos2 config))
        (if success?
            (values (rx-range range (chytes-limit s)) pos3)
            (parse-error s pos2 config "illegal alphabetic escape"))])]
     [else
      (values c2 (add1 pos2))])]))

;; Returns (values config position)
(define (parse-mode s pos config)
  (chyte-case/eos
   s pos
   [(eos)
    (values config pos)]
   [(#\i)
    (parse-mode s (add1 pos) (config-case-sensitive config #f))]
   [(#\s)
    (parse-mode s (add1 pos) (config-multi-line config #f))]
   [(#\m)
    (parse-mode s (add1 pos) (config-multi-line config #t))]
   [(#\-)
    (define pos2 (add1 pos))
    (chyte-case/eos
     s pos2
     [(eos)
      (values config pos)]
     [(#\i)
      (parse-mode s (add1 pos2) (config-case-sensitive config #t))]
     [(#\s)
      (parse-mode s (add1 pos2) (config-multi-line config #t))]
     [(#\m)
      (parse-mode s (add1 pos2) (config-multi-line config #f))]
     [else
      (values config pos)])]
   [else
    (values config pos)]))


(define (check-close-paren s pos config)
  (unless (and (pos . < . (chytes-length s))
               (= (chyte #\)) (chytes-ref s pos)))
    (parse-error s pos config "expected a closing `)`"))
  (add1 pos))
   
(define (missing-closing-error s pos config)
  (parse-error s pos config "missing closing parenthesis in pattern"))

(define (bad-?-sequence-error s pos config)
  (parse-error s pos config
               "expected `:`, `=`, `!`, `<=`, `<!`, `i`, `-i`, `m`, `-m`, `s`, or `-s` after `(?`"))
