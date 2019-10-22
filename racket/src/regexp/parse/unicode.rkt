#lang racket/base
(require (only-in racket/bool xor)
         "chyte.rkt"
         "chyte-case.rkt"
         "ast.rkt"
         "config.rkt"
         "error.rkt")

(provide parse-unicode-categories)

(define (parse-unicode-categories p-c s pos config)
  (chyte-case/eos
   s pos
   [(#\{)
    (define-values (cat-negated? next-pos)
      (chyte-case/eos
       s (add1 pos)
       [(#\^) (values #t (+ pos 2))]
       [else  (values #f (add1 pos))]))
    (define-values (l pos2)
      (let loop ([accum null] [pos next-pos])
        (chyte-case/eos
         s pos
         [(eos)
          (parse-error s pos config
                       "missing `}` to close `\\~a{`"
                       (integer->char p-c))]
         [(#\}) (values (reverse accum) (add1 pos))]
         [else
          (loop (cons (chytes-ref s pos) accum) (add1 pos))])))
    (define categories
      (case (list->bytes l)
        [(#"Ll") 'll]
        [(#"Lu") 'lu]
        [(#"Lt") 'lt]
        [(#"Lm") 'lm]
        [(#"L&") '(ll lu lt lm)]
        [(#"Lo") 'lo]
        [(#"L") '(ll lu lt lm lo)]
        [(#"Nd") 'nd]
        [(#"Nl") 'nl]
        [(#"No") 'no]
        [(#"N") '(nd nl no)]
        [(#"Ps") 'ps]
        [(#"Pe") 'pe]
        [(#"Pi") 'pi]
        [(#"Pf") 'pf]
        [(#"Pc") 'pc]
        [(#"Pd") 'pd]
        [(#"Po") 'po]
        [(#"P") '(ps pe pi pf pc pd po)]
        [(#"Mn") 'mn]
        [(#"Mc") 'mc]
        [(#"Me") 'me]
        [(#"M") '(mn mc me)]
        [(#"Sc") 'sc]
        [(#"Sk") 'sk]
        [(#"Sm") 'sm]
        [(#"So") 'so]
        [(#"S") '(sc sk sm so)]
        [(#"Zl") 'zl]
        [(#"Zp") 'zp]
        [(#"Zs") 'zs]
        [(#"Z") '(zl zp zs)]
        [(#"Cc") 'cc]
        [(#"Cf") 'cf]
        [(#"Cs") 'cs]
        [(#"Cn") 'cn]
        [(#"Co") 'co]
        [(#"C") '(cc cf cs cn so)]
        [(#".") #t]
        [else (parse-error s pos2 config
                           "unrecognized property name in `\\~a{}`: `~a`"
                           (integer->char p-c)
                           (list->string (map integer->char l)))]))
    (define prop-negated? (= p-c (char->integer #\P)))
    (values (rx:unicode-categories categories (not (xor prop-negated? cat-negated?)))
            pos2)]
   [else
    (parse-error s pos config
                 "expected `{` after `\\~a`"
                 (integer->char p-c))]))
