#lang scheme/base
(require scheme/port
         "scheme-lexer.ss")
  
(provide module-lexer)
  
(define (module-lexer in mode)
  (cond
   [(not mode)
    ;; Starting out: look for #lang:
    (let*-values ([(p) (peeking-input-port in)]
                  [(init) (file-position p)]
                  [(start-line start-col start-pos) (port-next-location p)])
      (let ([get-info (with-handlers ([exn:fail? (lambda (exn) 'fail)])
                        (parameterize ([current-reader-guard
                                        (let ([old (current-reader-guard)])
                                          (lambda (g)
                                            (if (and (pair? g)
                                                     (eq? (car g) 'planet))
                                                (error "#lang planet disbled")
                                                (old g))))])
                          ;; FIXME: set the reader guard to disable access to 
                          ;; untrusted planet packages.
                          (read-language in (lambda () #f))))])
        (cond
         [(procedure? get-info)
          ;; Produce language as first token:
          (let*-values ([(bytes-len) (- (file-position p) init)]
                        [(bstr) (read-bytes bytes-len in)]
                        [(end-line end-col end-pos) (port-next-location in)])
            (values
             bstr
             'other
             #f
             start-pos
             end-pos
             (or (let ([v (get-info 'color-lexer)])
                   (and v
                        (if (procedure-arity-includes? v 2)
                            (cons v #f)
                            v)))
                 scheme-lexer)))]
         [(eq? 'fail get-info)
          (let*-values ([(end-line end-col end-pos) (port-next-location in)])
            (values #f 'error #f start-pos end-pos
                    (lambda (in) 
                       (let-values ([(line col pos) (port-next-location in)])
                         (if (eof-object? (peek-byte in))
                             (values #f 'eof #f pos pos)
                             (begin
                               (copy-port in (open-output-nowhere))
                               (let-values ([(end-line end-col end-pos) (port-next-location in)])
                                 (values #f 'other #f pos end-pos))))))))]
         [else
          ;; Start over using the Scheme lexer
          (module-lexer in scheme-lexer)])))]
   [(pair? mode)
    ;; #lang-selected language consumes and produces a mode:
    (let-values ([(lexeme type data new-token-start new-token-end new-mode) 
                  ((car mode) in (cdr mode))])
      (values lexeme type data new-token-start new-token-end (cons (car mode) new-mode)))]
   [else
    ;; #lang-selected language (or default) doesn't deal with modes:
    (let-values ([(lexeme type data new-token-start new-token-end) 
                  (mode in)])
      (values lexeme type data new-token-start new-token-end mode))]))
