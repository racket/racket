
(module trace-raw mzscheme
  (require "../syntax-browser.ss"
           (lib "class.ss")
           (lib "lex.ss" "parser-tools")
           "deriv-tokens.ss"
           "deriv-parser.ss")
  (provide (all-defined))

  (define current-expand-observe
    (dynamic-require '#%expobs 'current-expand-observe))
  
  (define (go-trace sexpr)
    (define browser
      (parameterize (#;(identifier=-choices
                        (list (cons "related by table"
                                    (lambda (a b) (related-by-table table a b))))))
        (make-syntax-browser)))
    (define table #f)
    (define pos 0)
    (parameterize ((current-expand-observe
                    (lambda (sig val)
                      (define t (tokenize sig val pos))
                      (send browser add-text
                            (format "Signal: ~s: ~s~n"
                                    pos
                                    (token-name (position-token-token t))))
                      (send browser add-syntax
                            (datum->syntax-object #f val))
                      (set! pos (add1 pos)))))
      (expand sexpr)))
  
  (define (related-by-table table a b)
    (or (eq? a b)
        #;(and table '...)))
  
  )