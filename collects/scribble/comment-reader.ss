
(module comment-reader mzscheme
  (require (lib "kw.ss"))

  (provide (rename *read read)
           (rename *read-syntax read-syntax))

  (define/kw (*read #:optional [inp (current-input-port)])
    (parameterize ([current-readtable (make-comment-readtable)])
      (read/recursive inp)))

  (define/kw (*read-syntax #:optional src [port (current-input-port)])
    (parameterize ([current-readtable (make-comment-readtable)])
      (read-syntax/recursive src port)))

  (define (make-comment-readtable)
    (make-readtable (current-readtable)
                    #\; 'terminating-macro
                    (case-lambda 
                     [(char port)
                      (do-comment port (lambda () (read/recursive port #\@)))]
                     [(char port src line col pos)
                      (let ([v (do-comment port (lambda () (read-syntax/recursive src port #\@)))])
                        (let-values ([(eline ecol epos) (port-next-location port)])
                          (datum->syntax-object
                           #f
                           v
                           (list src line col pos (and pos epos (- epos pos))))))])))

  (define (do-comment port recur)
    (let loop ()
      (when (equal? #\; (peek-char port))
        (read-char port)
        (loop)))
    `(code:comment
      (unsyntax
       (t
        ,@(let loop ()
            (let ([c (read-char port)])
              (cond
               [(or (eof-object? c)
                    (char=? c #\newline))
                null]
               [(char=? c #\@)
                (cons (recur) (loop))]
               [else (cons (string c)
                           (loop))]))))))))
                    