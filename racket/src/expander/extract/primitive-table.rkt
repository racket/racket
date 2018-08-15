#lang racket/base
(provide substitute-primitive-table-access)

;; Replace
;;  (hash-ref (or (primitive-table '<table>) ...) <id> [default])
;; with just <id> if <table> is in `primitive-table-directs`.
(define (substitute-primitive-table-access l primitive-table-directs)
  (define (subst s)
    (cond
      [(primitive-table-lookup-match s)
       => (lambda (tables+id)
            (define prefix
              (for/or ([t (in-list (car tables+id))])
                (hash-ref primitive-table-directs t #f)))
            (cond
              [prefix
               (string->symbol (string-append prefix (symbol->string (cdr tables+id))))]
              [else s]))]
      [(pair? s)
       (cons (subst (car s)) (subst (cdr s)))]
      [else s]))
  (let loop ([l l])
    (cond
      [(null? l) null]
      [else
       (let ([s (car l)])
         ;; Watch out for `(define x x)` and drop it
         (cond
           [(and (pair? s)
                 (eq? 'define-values (car s))
                 (pair? (cadr s))
                 (null? (cdadr s)))
            (define rhs (subst (caddr s)))
            (if (eq? rhs (caadr s))
                (loop (cdr l))
                (cons `(define-values ,(cadr s) ,rhs)
                      (loop (cdr l))))]
           [else (cons (subst s) (loop (cdr l)))]))])))

(define (primitive-table-lookup-match s)
  (cond
    [(and (pair? s)
          (eq? (car s) 'hash-ref)
          (list? s)
          (<= 3 (length s) 4)
          (let ([q-id (caddr s)])
            (and (list? q-id)
                 (= (length q-id) 2)
                 (eq? 'quote (car q-id))
                 (symbol? (cadr q-id))
                 (cadr q-id))))
     => (lambda (id)
          (define tables (accessed-primitive-tables (cadr s)))
          (and tables
               (cons tables id)))]
    [else #f]))

;; Recognize expansion of
;;   (or (primitive-table '<table>) ...)
(define (accessed-primitive-tables s)
  (cond
    [(and (list? s)
          (= 2 (length s))
          (eq? 'primitive-table (car s))
          (let ([t (cadr s)])
            (and (list? t)
                 (= 2 (length t))
                 (eq? 'quote (car t))
                 (symbol? (cadr t))
                 (cadr t))))
     => (lambda (table)
          (list table))]
    [(and (list? s)
          (= 3 (length s))
          (eq? (car s) 'let-values)
          (= 1 (length (cadr s)))
          (= 1 (length (caar (cadr s))))
          (let ([id (car (caar (cadr s)))]
                [c (caddr s)])
            (and (list? c)
                 (= (length c) 4)
                 (eq? (car c) 'if)
                 (eq? (cadr c) id)
                 (eq? (caddr c) id)
                 (accessed-primitive-tables (cadddr c)))))
     =>  (lambda (tables)
           (define pre-tables (accessed-primitive-tables (cadar (cadr s))))
           (and pre-tables
                (append tables pre-tables)))]
    [else #f]))
