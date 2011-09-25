#lang mzscheme

(require (only racket/cmdline parse-command-line))

(provide command-line
         parse-command-line)

(define-syntax (command-line stx)
  (define (id=? x y)
    (eq? (syntax-e x) (syntax-e y)))
  (define (serror msg . detail)
    (apply raise-syntax-error #f msg stx detail))
  (define (extract-one what args . detail)
    (if (null? args)
        (apply serror (format "missing ~a" what) detail)
        (values (car args) (cdr args))))
  (define (extract-list stx/list pred)
    (let loop ([xs null]
               [rest (if (syntax? stx/list) (syntax->list stx/list) stx/list)])
      (if (and (pair? rest) (pred (car rest)))
          (loop (cons (car rest) xs) (cdr rest))
          (values (reverse xs) rest))))
  (define (formal-names l)
    (map (lambda (a)
           (datum->syntax-object
            (quote-syntax here)
            (let ([s (symbol->string (syntax-e a))])
              (if (char=? #\* (string-ref s (sub1 (string-length s))))
                  (substring s 0 (sub1 (string-length s)))
                  s))
            #f))
         l))
  (syntax-case stx ()
    [(_ program-name argv clause ...)
     (let ([clauses
            (let loop ([csrcs (syntax->list #'(clause ...))][clauses null])
              (with-syntax ([(clause ...) clauses])
                (if (null? csrcs)
                    #'((list clause ...) (lambda (accum) (void)) null)
                    (let ([line (car csrcs)]
                          [arest (cdr csrcs)])
                      (syntax-case* line (help-labels => args) id=?
                        [(help-labels s ...)
                         (begin
                           (unless (andmap (lambda (x) (string? (syntax-e x)))
                                           (syntax->list #'(s ...)))
                             (serror "help-labels clause must contain only strings" line))
                           (loop arest #'(clause ... '(help-labels s ...))))]
                        [(tag . rest)
                         (ormap (lambda (x) (id=? #'tag x))
                                (syntax->list #'(once-each once-any multi final)))
                         (with-syntax 
                             ([sublines
                               (let slloop ([sublines (syntax->list #'rest)])
                                 (if (null? sublines)
                                     #'()
                                     (with-syntax 
                                         ([looped (slloop (cdr sublines))]
                                          [subline
                                           (with-syntax 
                                               ([flags 
                                                 (syntax-case (car sublines) ()
                                                   [((flag ...) . rest)
                                                    (begin
                                                      (unless (andmap 
                                                               (lambda (x) (string? (syntax-e x)))
                                                               (syntax->list #'(flag ...)))
                                                        (serror 
                                                         "flag specification is not a string or sequence of strings" 
                                                         #'(flag ...)))
                                                      #'(flag ...))]
                                                   [(flag . rest)
                                                    (string? (syntax-e #'flag))
                                                    #'(flag)]
                                                   [else
                                                    (serror "clause does not start with flags")])])
                                             (syntax-case* (car sublines) (=>) id=?
                                               [(_ => a b)
                                                #'(list 'flags a b)]
                                               [(_ rest ...)
                                                (let*-values ([(formals rest)
                                                               (extract-list #'(rest ...) identifier?)]
                                                              [(helps rest)
                                                               (extract-list
                                                                rest (lambda (x) (string? (syntax-e x))))]
                                                              [(expr1 rest)
                                                               (extract-one
                                                                "handler body expressions" rest line)])
                                                  (when (null? helps)
                                                    (serror "missing help string/s"))
                                                  (with-syntax ([formals formals]
                                                                [formal-names (formal-names formals)]
                                                                [helps helps]
                                                                [expr1 expr1]
                                                                [rest rest])
                                                    #'(list 'flags
                                                            (lambda (flag . formals) expr1 . rest)
                                                            '(helps . formal-names))))]))])
                                       #'(subline . looped))))])
                           (loop arest #'(clause ... (list 'tag . sublines))))]
                        [(=> finish-proc arg-help help-proc unknown-proc)
                         (begin
                           (unless (null? arest)
                             (serror "=> must be the last clause line"))
                           #'((list clause ...)
                              finish-proc arg-help help-proc unknown-proc))]
                        [(=> finish-proc arg-help help-proc)
                         (begin
                           (unless (null? arest)
                             (serror "=> must be the last clause line"))
                           #'((list clause ...)
                              finish-proc arg-help help-proc))]
                        [(=> finish-proc arg-help)
                         (begin
                           (unless (null? arest)
                             (serror "=> must be the last clause line"))
                           #'((list clause ...) finish-proc arg-help))]
                        [(=> . _)
                         (serror "bad => line" line)]
                        [(args arg-formals body1 body ...)
                         (begin
                           (unless (null? arest)
                             (serror "args must be the last clause" line))
                           (let ([formals
                                  (let loop ([f #'arg-formals])
                                    (syntax-case f ()
                                      [() null]
                                      [(arg . rest)
                                       (identifier? #'arg)
                                       (cons #'arg (loop #'rest))]
                                      [arg
                                       (identifier? #'arg)
                                       (list #'arg)]
                                      [else
                                       (serror "bad argument list" line)]))])
                             (with-syntax ([formal-names (formal-names formals)])
                               #'((list clause ...)
                                  (lambda (accume . arg-formals)
                                    body1 body ...)
                                  'formal-names))))]
                        [(args . _)
                         (serror "bad args line" line)]
                        [else (serror "not a once-each, once-any, multi, final, args, or => line" line)])))))])
       (with-syntax ([clauses clauses])
         #'(parse-command-line program-name argv . clauses)))]))
