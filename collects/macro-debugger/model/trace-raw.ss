
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
    (define events null)
    (define pos 0)
    (define browser (make-syntax-browser))
    (define (show sig+val)
      (define sig (car sig+val))
      (define val (cdr sig+val))
      (define t (tokenize sig val pos))
      (send browser add-text
            (format "Signal: ~s: ~s~n"
                    pos
                    (token-name (position-token-token t))))
      (when val
            (send browser add-syntax
                  (datum->syntax-object #f val)))
      (set! pos (add1 pos)))
    (parameterize ((current-expand-observe
                    (lambda (sig val)
                      (define t (tokenize sig val pos))
                      (set! events (cons (cons sig val) events))
                      #;(show (cons sig val)))))
      (expand sexpr)
      (for-each show (reverse events))))
  
  )