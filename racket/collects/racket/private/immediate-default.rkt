(module kw '#%kernel
  (#%require "define.rkt"
             "small-scheme.rkt"
             "stxcase-scheme.rkt"
             (for-template '#%kernel))

  (#%provide immediate-default?)

  ;; A default-argument expression counts as an "immediate default"
  ;; if it syntactically (before expansion) matches
  ;;
  ;;    <immediate-default> = <immediate-literal>    [*]
  ;;                        | '<immediate-literal>
  ;;                        | '<id> | '()
  ;;                        | (void) | null | eof
  ;;    <immediate-literal> = #t | #f | <number> | <char>
  ;;                        | <small-string> | <small-byte-string>
  ;;
  ;; where the plain <immediate-literal> [*] possibility matches only
  ;; if the literal's syntax transferred to '#%datum is bound to
  ;; `#%datum` from `racket/base`.

  (define (immediate-default? expr)
    (let ([immediate-literal?
           (lambda (v)
             (or (boolean? v)
                 (number? v)
                 (char? v)
                 (and (string? v)
                      ((string-length v) . < . 8))
                 (and (bytes? v)
                      ((bytes-length v) . < . 8))))])
      (or (and (immediate-literal? (syntax-e expr))
               (free-identifier=? (quote-syntax #%datum) (datum->syntax expr '#%datum)))
          (syntax-case expr (quote void null eof)
            [(quote s-exp) (let ([v (syntax-e #'s-exp)])
                             (or (and (symbol? v)
                                      (or (symbol-interned? v)
                                          (symbol-unreadable? v)))
                                 (null? v)
                                 (immediate-literal? v)))]
            [(void) #t]
            [null #t]
            [eof #t]
            [_ #f])))))
