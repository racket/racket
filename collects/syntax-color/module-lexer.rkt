#lang scheme/base
(require scheme/port
         "scheme-lexer.rkt")
(provide module-lexer)

#|

mode : (or/c #f 'before-lang-line
             'no-lang-line
             (cons lexer mode)
             lexer)

the module lexer tracks any white-space and comments before
the #lang line (if any) explicitly by wrapping calls to the 
scheme-lexer (in #f or 'before-lang-line mode). 
Once it finds a non-white-space and non-comment
token, it checks to see if there is a #lang line and, if so
changes the mode to be the lexer that the #lang indicates,
delegating to it (the last two modes listed above).
If there is no #lang line, then it continues
to delegate to the scheme-lexer (in the 'no-lang-line mode).

|#


(define (module-lexer in offset mode)
  (cond
    [(or (not mode) (eq? mode 'before-lang-line))
     (define lexer-port (peeking-input-port in))
     (port-count-lines! lexer-port)
     (define-values (lexeme type data raw-new-token-start raw-new-token-end) (scheme-lexer lexer-port))
     (define new-token-start (and raw-new-token-start (+ raw-new-token-start (file-position in))))
     (define new-token-end (and raw-new-token-end (+ raw-new-token-end (file-position in))))
     (cond
       [(or (eq? type 'comment) (eq? type 'white-space))
        (define lexer-end (file-position lexer-port))
        (read-string lexer-end in) ;; sync ports
        (values lexeme type data new-token-start new-token-end 0 'before-lang-line)]
       [else
        ;; look for #lang:
        (define p (peeking-input-port in))
        (port-count-lines! p)
        (define get-info (with-handlers ([exn:fail:read? values]) (read-language p (λ () 'fail))))
        (cond
          [(procedure? get-info)
           (define end-pos (file-position p))
           (read-string end-pos in) ;; sync ports
           ;; Produce language as first token:
           (values
            "#lang"
            'other
            #f
            1 ;; start-pos
            (+ end-pos 1)
            0
            (or (let ([v (get-info 'color-lexer #f)])
                  (and v
                       (if (procedure-arity-includes? v 3)
                           (cons v #f)
                           v)))
                scheme-lexer))]
          [else
           (read-string (file-position lexer-port) in) ;; sync ports
           (values lexeme type data new-token-start new-token-end 0 'no-lang-line)])])]
    [(eq? mode 'no-lang-line)
     (let-values ([(lexeme type data new-token-start new-token-end) 
                   (scheme-lexer in)])
       (values lexeme type data new-token-start new-token-end 0 'no-lang-line))]
    [(pair? mode)
     ;; #lang-selected language consumes and produces a mode:
     (let-values ([(lexeme type data new-token-start new-token-end backup-delta new-mode) 
                   ((car mode) in offset (cdr mode))])
       (values lexeme type data new-token-start new-token-end backup-delta (cons (car mode) new-mode)))]
    [else
     ;; #lang-selected language (or default) doesn't deal with modes:
     (let-values ([(lexeme type data new-token-start new-token-end) 
                   (mode in)])
       (values lexeme type data new-token-start new-token-end 0 mode))]))
