#lang racket/base
(require "chyte.rkt"
         "chyte-case.rkt"
         "ast.rkt"
         "config.rkt"
         "error.rkt")

(provide parse-unicode-categories)

(define (parse-unicode-categories p-c s pos config)
  (chyte-case/eos
   s pos
   [(#\{)
    (define-values (l pos2)
      (let loop ([accum null] [pos (add1 pos)])
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
    (values (rx:unicode-categories categories (= p-c (char->integer #\p)))
            pos2)]
   [else
    (parse-error s pos config
                 "expected `{` after `\\~a`"
                 (integer->char p-c))]))
