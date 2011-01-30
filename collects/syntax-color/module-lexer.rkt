#lang racket/base
(require racket/port
         "scheme-lexer.rkt")

(provide module-lexer)

;; mode = (or/c #f 'before-lang-line -- 
;;              'no-lang-line
;;              (cons lexer mode)
;;              lexer)

(define count 0)

(define (module-lexer in offset mode)
  (set! count (+ count 1))
  (printf "~a ~s\n" count (list 'module-lexer in offset mode))
  (cond
    [(or (not mode) (eq? mode 'before-lang-line))
     (define-values (lexeme type data new-token-start new-token-end) (scheme-lexer in))
     (printf "before-lang-line lexeme ~s type ~s\n" lexeme type)
     (cond
       [(or (eq? type 'comment) (eq? type 'whitespace))
        (values lexeme 'other data new-token-start new-token-end 0 'before-lang-line)]
       [else
        ;; look for #lang:
        (define p (peeking-input-port in))
        (port-count-lines! p)
        (define init (file-position p))
        (define get-info (with-handlers ([exn:fail:read? values]) (read-language p (λ () 'fail))))
        (printf "get-info ~s\n" get-info)
        (cond
          [(not (procedure? get-info))
           ;(or (exn? get-info) (eq? get-info 'fail))
           (values lexeme type data new-token-start new-token-end 0 'no-lang-line)]
          [(procedure? get-info)
           (define end-pos (file-position p))
           (read-bytes (- end-pos init) in) ;; sync ports
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
                scheme-lexer))])])]
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
