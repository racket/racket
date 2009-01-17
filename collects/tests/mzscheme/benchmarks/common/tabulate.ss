#!/bin/sh
#|
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module tabulate mzscheme
  (require mzlib/list
           xml/xml
           mzlib/cmdline)

  (define base-link-filename (make-parameter #f))
  (define full-page-mode (make-parameter #f))
  (define include-links (make-parameter #f))
  (define nongc (make-parameter #f))

  (command-line
   "tabulate"
   (current-command-line-arguments)
   (once-each
    [("--no-links") "suppress benchmark links to SVN"
     (include-links #f)]
    [("--multi") name "generate multiple pages for different views of data"
     (base-link-filename name)]
    [("--nongc") "show times not including GC"
     (nongc #t)]
    [("--index") "generate full page with an index.html link"
     (full-page-mode #t)]))
  
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

  (define (small s)
    `(font ((color "gray")
            (size "-2"))
           ,s))

  (define (lookup-color impl)
    (let loop ([impls sorted-impls][odd? #f])
      (if (eq? (car impls) impl)
          (if odd?
              "#EEEEFF"
              "#DDFFDD")
          (loop (cdr impls) (not odd?)))))

  (define (wrap-page relative-to p)
    (if (full-page-mode)
        (let ([title (format "~a normalized to ~a"
                             (or (base-link-filename)
                                 "results")
                             (or relative-to
                                 "fastest"))])
          `(html
            (head (title ,title)
                  (body
                   (h1 ,title) 
                   (p "See also " (a ((href "index.html"))
                                     "about the benchmarks")
                      ".")
                   (p ,p)))))
        p))

  (define forever 1000000000)

  (define (ntime v)
    (and (caadr v) (- (caadr v) (caddr (cadr v)))))

  (define (generate-page relative-to)
    (empty-tag-shorthand html-empty-tags)
    (write-xml/content 
     (xexpr->xml
      (wrap-page
       relative-to
       `(table
         (tr (td nbsp) 
             (td ((colspan "2") (align "right")) 
                 ,(if (and (base-link-filename)
                           relative-to)
                      `(a ((href ,(format "~a.html" (base-link-filename))))
                          "fastest")
                      "fastest"))
             ,@(map (lambda (impl)
                      `(td ((colspan "2") (align "right")) 
                           (b ,(let ([s (symbol->string impl)])
                                 (if (and (base-link-filename)
                                          (not (eq? impl relative-to)))
                                     `(a ((href ,(format "~a-~a.html"
                                                         (base-link-filename)
                                                         impl)))
                                         ,s)
                                     s)))
                           nbsp))
                    sorted-impls))
         ,@(map (lambda (bm-run)
                  (let ([fastest (apply min (map (lambda (run)
                                                   (or (caadr run) forever))
                                                 (cdr bm-run)))]
                        [n-fastest (apply min (map (lambda (run)
                                                     (or (ntime run) forever))
                                                   (cdr bm-run)))]
                        [c-fastest (apply min (map (lambda (run)
                                                     (let ([v (caddr run)])
                                                       (or (and v (positive? v) v)
                                                           forever)))
                                                   (cdr bm-run)))])
                    (let-values ([(base n-base c-base)
                                  (if relative-to
                                      (let ([a (assq relative-to (cdr bm-run))])
                                        (if a
                                            (values (caadr a) (ntime a) (caddr a))
                                            (values #f #f #f)))
                                      (values fastest n-fastest c-fastest))])
                      `(tr (td ,(if (include-links)
				    `(a ((href ,(format (string-append "http://svn.plt-scheme.org/plt/trunk/collects/"
								       "tests/mzscheme/benchmarks/common/~a.sch")
							(car bm-run))))
					,(symbol->string (car bm-run)))
				    (symbol->string (car bm-run))))
                           (td ((align "right"))
                               nbsp
                               ,(small (if (= c-fastest forever)
                                           " "
                                           (number->string c-fastest)))
                               nbsp)
                           (td ((align "right"))
                               ,(format "~a ms" fastest)
                               nbsp nbsp)
                           ,@(apply
                              append
                              (map (lambda (impl)
                                     (let* ([a (assq impl (cdr bm-run))]
                                            [n (and a (caadr a))]
                                            [n2 (and a (ntime a))])
                                       `(,(if (= c-fastest forever)
					      `(td)
					      `(td ((align "right")
						    (bgcolor ,(lookup-color impl)))
						   ,(if (and (caddr a) c-base (positive? c-base))
							(small (ratio->string (/ (caddr a) c-base)))
							'"-")
						   nbsp))
                                         (td ((bgcolor ,(lookup-color impl)))
                                             ,(if (and n base)
                                                  (let ([s (if (= n base)
                                                               "1"
                                                               (if (zero? base)
                                                                   "*"
                                                                   (ratio->string (/ n base))))])
                                                    (if (= n fastest)
                                                        `(font ((color "forestgreen")) (b ,s))
                                                        s))
                                                  "-")
                                             ,@(if (nongc)
                                                   `(" / " 
                                                     ,(if (and n2 n-base)
                                                          (let ([s (if (zero? base)
                                                                          "*"
                                                                          (ratio->string (/ n2 base)))])
                                                            (if (= n2 n-fastest)
                                                                `(font ((color "forestgreen")) (b ,s))
                                                                s))
                                                          "-"))
                                                   null)
                                             nbsp))))
                                   sorted-impls))))))
                sorted-runs)))))
    (newline))

  (if (base-link-filename)
      (for-each (lambda (impl)
                  (with-output-to-file (if impl
                                           (format "~a-~a.html" 
                                                   (base-link-filename)
                                                   impl)
                                           (format "~a.html"
                                                   (base-link-filename)))
                    (lambda () (generate-page impl))
                    'truncate))
                (cons #f sorted-impls))
      (generate-page #f)))

