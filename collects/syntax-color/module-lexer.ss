#lang scheme/base
(require scheme/port
         "scheme-lexer.ss")
  
(provide module-lexer)
  
(define (module-lexer in offset mode)
  (cond
   [(not mode)
    ;; Starting out: look for #lang:
    (let*-values ([(p) (peeking-input-port in)]
                  [(init) (file-position p)]
                  [(start-line start-col start-pos) (port-next-location p)])
      (let ([get-info (with-handlers ([exn:fail? (lambda (exn) 'fail)])
                        (read-language p (lambda () #f)))]
            [sync-ports (lambda ()
                          (read-bytes (- (file-position p) init) in))])
        (cond
         [(procedure? get-info)
          ;; Produce language as first token:
          (sync-ports)
          (let-values ([(end-line end-col end-pos) (port-next-location in)])
            (values
             "#lang"
             'other
             #f
             start-pos
             end-pos
             0
             (or (let ([v (get-info 'color-lexer #f)])
                   (and v
                        (if (procedure-arity-includes? v 3)
                            (cons v #f)
                            v)))
                 scheme-lexer)))]
         [(eq? 'fail get-info)
          (sync-ports)
          (let*-values ([(end-line end-col end-pos) (port-next-location in)])
            (values #f 'error #f start-pos end-pos 0 scheme-lexer))]
         [else
          ;; Start over using the Scheme lexer
          (module-lexer in offset scheme-lexer)])))]
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
