(module comment-reader scheme/base
  (require (only-in racket/port peeking-input-port))

  (provide (rename-out [*read read]
                       [*read-syntax read-syntax])
           make-comment-readtable)

  (define unsyntaxer (make-parameter 'unsyntax))

  (define (*read [inp (current-input-port)])
    (parameterize ([unsyntaxer (read-unsyntaxer inp)]
                   [current-readtable (make-comment-readtable)])
      (read/recursive inp)))

  (define (*read-syntax src [port (current-input-port)])
    (parameterize ([unsyntaxer (read-unsyntaxer port)]
                   [current-readtable (make-comment-readtable)])
      (read-syntax/recursive src port)))
  
  (define (read-unsyntaxer port)
    (let ([p (peeking-input-port port)])
      (if (eq? (read p) '#:escape-id)  
          (begin (read port) (read port))
          'unsyntax)))

  (define (make-comment-readtable #:readtable [rt (current-readtable)])
    (make-readtable rt
                    #\; 'terminating-macro
                    (case-lambda 
                     [(char port)
                      (do-comment port (lambda () (read/recursive port #\@)))]
                     [(char port src line col pos)
                      (let ([v (do-comment port (lambda () (read-syntax/recursive src port #\@)))])
                        (let-values ([(eline ecol epos) (port-next-location port)])
                          (datum->syntax
                           #f
                           v
                           (list src line col pos (and pos epos (- epos pos))))))])))

  (define (do-comment port recur)
    (let loop ()
      (when (equal? #\; (peek-char port))
        (read-char port)
        (loop)))
    (when (equal? #\space (peek-char port))
      (read-char port))
    `(code:comment
      (,(unsyntaxer)
       (t
        ,@(append-strings
           (let loop ()
             (let ([c (read-char port)])
               (cond
                [(or (eof-object? c)
                     (char=? c #\newline))
                 null]
                [(char=? c #\@)
                 (cons (recur) (loop))]
                [else 
                 (cons (string c)
                       (loop))]))))))))
  
  (define (append-strings l)
    (let loop ([l l][s null])
      (cond
       [(null? l) (if (null? s)
                      null
                      (preserve-space (apply string-append (reverse s))))]
       [(string? (car l))
        (loop (cdr l) (cons (car l) s))]
       [else
        (append (loop null s)
                (cons
                 (car l)
                 (loop (cdr l) null)))])))

  (define (preserve-space s)
    (let ([m (regexp-match-positions #rx"  +" s)])
      (if m
          (append (preserve-space (substring s 0 (caar m)))
                  (list `(hspace ,(- (cdar m) (caar m))))
                  (preserve-space (substring s (cdar m))))
          (list s)))))
