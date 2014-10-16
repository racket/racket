#lang racket/base
(require racket/port
         "racket-lexer.rkt"
         "lexer-contract.rkt"
         racket/contract
         unstable/options)
(provide 
 (contract-out [module-lexer lexer/c]))

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
     (define lexer-port (peeking-input-port in #:init-position (+ 1 (file-position in))))
     (let-values ([(line col pos) (port-next-location in)])
       (when line 
         (port-count-lines! lexer-port)))
     (set-port-next-location-from in lexer-port)
     (define-values (lexeme type data new-token-start new-token-end) (racket-lexer lexer-port))
     (cond
       [(or (eq? type 'comment) (eq? type 'white-space))
        (define lexer-end (file-position lexer-port))
        ;; sync ports
        (for/list ([i (in-range (file-position in) (file-position lexer-port))])
          (read-byte-or-special in))
        (values lexeme type data new-token-start new-token-end 0 'before-lang-line)]
       [else
        ;; look for #lang:
        (define p (peeking-input-port in #:init-position (+ 1 (file-position in))))
        (define name-p (peeking-input-port in #:init-position (+ 1 (file-position in))))
        (let-values ([(line col pos) (port-next-location in)])
          (when line 
            (port-count-lines! p)
            (port-count-lines! name-p)))
        (set-port-next-location-from in p)
        (set-port-next-location-from in name-p)
        (define-values (_1 _2 start-pos) (port-next-location p))
        (define get-info (with-handlers ([exn:fail? values]) 
                           (or (read-language p (λ () 'fail))
                               (λ (x y)  y))))
        (define-values (_3 _4 end-pos) (port-next-location p))
        (cond
          [(procedure? get-info)
           (define lang-name 
             (apply string
                    (filter
                     char?
                     (for/list ([i (in-range (file-position in) (file-position p))])
                       (read-char name-p)))))
          
           ;; sync ports
           (for ([i (in-range (file-position in) (file-position p))])
             (read-byte-or-special in))
           
           (define the-lexer 
             (let ([raw-lexer (or (get-info 'color-lexer #f) racket-lexer)])
               (if (trusted-lexer? raw-lexer)
                   (waive-option raw-lexer)
                   (exercise-option raw-lexer))))
           
           ;; Produce language as first token:
           (values
            lang-name
            'other
            #f
            start-pos
            end-pos
            0
            (if (procedure-arity-includes? the-lexer 3)
                (cons the-lexer #f)
                the-lexer))]
         
          [(and (eq? type 'other)
                (string? lexeme)
                ;; the read-language docs say that this is all it takes to commit to a #lang
                (regexp-match #rx"^#[!l]" lexeme))
           ;; sync ports
           (for ([i (in-range (file-position in) (file-position p))])
             (read-byte-or-special in))
           (values lexeme 'error data new-token-start end-pos 0 'no-lang-line)]
          [else 
           (for ([i (in-range (file-position in) (file-position lexer-port))])
             (read-byte-or-special in))
           (values lexeme type data new-token-start new-token-end 0 'no-lang-line)])])]
    [(eq? mode 'no-lang-line)
     (let-values ([(lexeme type data new-token-start new-token-end) 
                   (racket-lexer in)])
       (values lexeme type data new-token-start new-token-end 0 'no-lang-line))]
    [(pair? mode)
     ;; #lang-selected language consumes and produces a mode:
     (let-values ([(lexeme type data new-token-start new-token-end backup-delta new-mode) 
                   ((car mode) in offset (cdr mode))])
       (values lexeme type data new-token-start new-token-end backup-delta 
               (if (dont-stop? new-mode)
                   (dont-stop (cons (car mode) (dont-stop-val new-mode)))
                   (cons (car mode) new-mode))))]
    [else
     ;; #lang-selected language (or default) doesn't deal with modes:
     (let-values ([(lexeme type data new-token-start new-token-end) 
                   (mode in)])
       (values lexeme type data new-token-start new-token-end 0 mode))]))

(define (set-port-next-location-from src dest)
  (define-values (line col pos) (port-next-location src))
  (set-port-next-location! dest line col pos))


(define (trusted-lexer? the-lexer)
  (member (object-name the-lexer)
          '(racket-lexer 
            racket-lexer/status 
            racket-nobar-lexer/status
            scribble-inside-lexer
            scribble-lexer)))
