
(module pread mzscheme
  (require "readline.ss"
           (lib "file.ss"))

  (define MAX-HISTORY 100)
  (define KEEP-DUPLICATES #f)
  (define KEEP-BLANKS     #f)

  (define leftovers null)
  (define counter 1)

  (define local-history
    (let ([hist (get-preference 'mzrl-history (lambda () null))])
      (for-each add-history hist)
      (reverse hist)))

  (define (save-history)
    (put-preferences '(mzrl-history) (list (reverse local-history))))

  (define (readline/hist p)
    (let ([s (readline p)])
      (when (and (string? s)
                 (or KEEP-BLANKS (not (zero? (string-length s))))
                 (or KEEP-DUPLICATES
                     (null? local-history)
                     (not (equal? s (car local-history)))))
        (add-history s)
        (set! local-history (cons s local-history))
        (let loop ([n MAX-HISTORY] [l local-history])
          (cond [(null? l) 'done]
                [(zero? n) (set-cdr! l '())]
                [else (loop (sub1 n) (cdr l))])))
      s))

  (exit-handler (let ([old (exit-handler)])
                  (lambda (v)
                    (save-history)
                    (old v))))

  (define (prompt-read-using-readline get-prompt)
    (if (pair? leftovers)
      (begin0 (car leftovers)
        (set! leftovers (cdr leftovers)))
      (let big-loop ()
        (let loop ([s (readline/hist (get-prompt 0))] [next-pos 1] [force? #f])
          (if (eof-object? s)
            (begin (save-history) s)
            (with-handlers ([(if force? (lambda (x) #f) exn:fail:read:eof?)
                             (lambda (exn)
                               (let ([v (readline/hist (get-prompt next-pos))])
                                 (loop (string-append s "\n"
                                                      (if (eof-object? v) "" v))
                                       (add1 next-pos)
                                       (eof-object? v))))])
              (let ([p (open-input-string (string-append s "\n"))])
                (port-count-lines! p)
                (let ([rs (let loop ()
                            (let ([r (parameterize ([read-accept-reader #t])
                                       (read-syntax
                                        (string->path
                                         (format "repl-~a" counter))
                                        p))])
                              (if (eof-object? r)
                                null
                                (cons r (loop)))))])
                  (if (null? rs)
                    (big-loop)
                    (begin0 (car rs)
                      (set! counter (add1 counter))
                      (set! leftovers (cdr rs))))))))))))

  (provide prompt-read-using-readline))
