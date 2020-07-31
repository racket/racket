(define same-up? #f)
(define exec? #f)
(define no-trailing-sep? #f)

(define-values (orig-p extras)
  (let loop ([l (command-line-arguments)])
    (cond
      [(null? l)
       (error 'absify "missing path")]
      [(equal? "--same-up" (car l))
       (set! same-up? !t)
       (loop (cdr l))]
      [(equal? "--exec" (car l))
       (set! exec? #t)
       (loop (cdr l))]
      [(equal? "--no-trailing-sep" (car l))
       (set! no-trailing-sep? #t)
       (loop (cdr l))]
      [else
       (values (car l) (cdr l))])))

(define (simplify-path p)
  ;; Do nothing, for now
  p)

(define (path->complete-path p wrt)
  (if (path-absolute? p)
      p
      (string-append wrt
                     (string (directory-separator))
                     p)))

(define (find-executable-path p)
  (let ([paths (getenv "PATH")])
    (and paths
         (ormap (lambda (dir)
                  (let ([p (path->complete-path p dir)])
                    (and (file-exists? p)
                         p)))
                (let loop ([l (string->list paths)] [accum '()])
                  (cond
                    [(null? l) (if (null? accum)
                                   '()
                                   (list (list->string (reverse accum))))]
                    [(eqv? #\: (car l))
                     (append (loop '() accum)
                             (loop (cdr l) '()))]
                    [else
                     (loop (cdr l) (cons (car l) accum))]))))))

(cond
  [(and same-up?
        (equal? (path-first p) ".."))
   (display orig-p)]
  [else
   (let ([p (if exec?
                (let ([p orig-p])
                  (if (equal? (path-parent p) "")
                      (or (find-executable-path p)
                          p)
                      p))
                orig-p)])

     (define (fix-trailing p)
       (if no-trailing-sep?
           (if (equal? (path-last p) "")
               (path-parent p)
               p)
           p))
     
     (display (fix-trailing (simplify-path (path->complete-path p (current-directory))))))])
  
;; In case there are extra arguments to an executable, preserve them
(for-each (lambda (e)
            (display " ")
            (display e))
          extras)

(newline)
