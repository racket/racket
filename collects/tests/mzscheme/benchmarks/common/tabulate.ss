
(module tabulate mzscheme
  (require (lib "list.ss")
           (lib "xml.ss" "xml"))
  
  (define bm-table (make-hash-table))
  (define impls (make-hash-table))

  (let loop ()
    (let ([l (read)])
      (unless (eof-object? l)
        (hash-table-put! impls (car l) #t)
        (let ([t (hash-table-get bm-table (cadr l)
                                 (lambda ()
                                   (let ([t (make-hash-table)])
                                     (hash-table-put! bm-table (cadr l) t)
                                     t)))])
          (hash-table-put! t (car l) 
                           (cons (cddr l)
                                 (hash-table-get t (car l) null))))
        (loop))))

  (define bm-runs (hash-table-map bm-table cons))

  (define (average sel l)
    (if (andmap sel l)
        (round (/ (apply + (map sel l)) (length l)))
        (if (ormap sel l)
            (error 'tabulate "inconsistent average info")
            #f)))
  
  (define average-runs
    (map (lambda (bm-run)
           (cons
            (car bm-run)
            (map (lambda (runs)
                   (list (car runs)
                         (list (average caar (cdr runs))
                               (average cadar (cdr runs))
                               (average caddar (cdr runs)))
                         (cadadr runs)))
                 (hash-table-map (cdr bm-run) cons))))
         bm-runs))

  (define (symbol<? a b)
    (string<? (symbol->string a)
              (symbol->string b)))

  (define sorted-runs 
    (sort average-runs (lambda (a b)
                         (symbol<? (car a) (car b)))))

  (define sorted-impls
    (sort (hash-table-map impls (lambda (k v) k)) symbol<?))

  (define (ratio->string r)
    (if (integer? r)
        (number->string r)
        (let ([s (format "~a00" (exact->inexact r))])
          (car (regexp-match #rx"^[0-9]*[.].." s)))))

  (empty-tag-shorthand html-empty-tags)
  (write-xml/content 
   (xexpr->xml 
    `(html
      (head (title "Benchmark Results"))
      (body
       (table
        (tr (td nbsp) 
            (td nbsp)
            ,@(map (lambda (impl)
                     `(td (b ,(symbol->string impl)) nbsp))
                   sorted-impls))
        ,@(map (lambda (bm-run)
                 (let ([fastest (apply min (map (lambda (run)
                                                  (or (caadr run) 1000000000))
                                                (cdr bm-run)))])
                   `(tr (td (a ((href ,(format "~a.sch" (car bm-run))))
                               ,(symbol->string (car bm-run))))
                        (td ((align "right"))
                            nbsp
                            ,(format "~a ms" fastest) nbsp nbsp nbsp)
                        ,@(map (lambda (impl)
                                 (let* ([a (assq impl (cdr bm-run))]
                                        [n (and a (caadr a))])
                                   `(td ,(if n
                                             (ratio->string (/ n fastest))
                                             "-"))))
                               sorted-impls))))
               sorted-runs))))))
  (newline))
