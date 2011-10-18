#!/bin/sh
#|
exec racket -qu "$0" ${1+"$@"}
|#

;; Input format is a sequence of S-expression forms:
;;  (<impl> <benchmark> (<cpu-msec> <real-msec> <gc-cpu-msec>) <compile-msec>)
;; where
;;   * <impl> is a symbol for an implementation; it can optionally be of the form 
;;     <sys>@<mode>, where each <sys> is tried in each <mode>
;;   * <benchmark> is a symbol for the benchmark
;;   * <cpu-msec> and <real-msec> are the run times (CPU and real) in milliseconds
;;   * <gc-cpu-msec> can be #f, or it can be a portion of <cpu-msec> spent GCing
;;   * <compile-msec> should be the same for each entry of a particular <impl>
;;     and <benchmark> combination; it is the time to compile the benchmark

(module tabulate mzscheme
  (require mzlib/list
           xml/xml
           mzlib/cmdline
           (only scheme/list argmin))

  (define base-link-filename (make-parameter #f))
  (define full-page-mode (make-parameter #f))
  (define include-links (make-parameter #f))
  (define nongc (make-parameter #f))
  (define subtract-nothing (make-parameter #f))
  (define subtract-nothing-run (make-parameter #f))
  (define generate-graph (make-parameter #f))
  (define no-compile-time (make-parameter #f))
  (define coefficient-of-variation (make-parameter #f))

  (command-line
   "tabulate"
   (current-command-line-arguments)
   (once-each
    [("--graph") "generate graphs instead of tables (unless --multi)"
     (generate-graph #t)]
    [("--links") "benchmark links to git"
     (include-links #t)]
    [("--multi") name "generate multiple pages for different views of data"
     (base-link-filename name)]
    [("--no-compile-time") "do not show compile times"
     (no-compile-time #t)]
    [("--nongc") "show times not including GC"
     (nongc #t)]
    [("--index") "generate full page with an index.html link"
     (full-page-mode #t)]
    [("--nothing") "subtract compilation time of nothing benchmark"
     (subtract-nothing #t)]
    [("--nothing-run") "subtract the run time of nothing benchmark"
     (subtract-nothing-run #t)]
    [("--coefficient-of-variation") "show coefficient of variation"
     (coefficient-of-variation #t)]))

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

  (define (average/coefficient-of-variation sel value-to-subtract l)
    (if (andmap sel l)
        (let* ((l (map (lambda (x) (max (- (sel x) value-to-subtract) 0)) l))
               (avg (round (/ (apply + l) (length l)))))
          (list avg
                (/ (round (sqrt (/ (apply + (map (lambda (x) (expt (- x avg) 2)) l))
                                   (length l)
                                   (if (zero? avg) 1.0 avg)))) ; no division by 0
                   100)))
        (if (ormap sel l)
            (error 'tabulate "inconsistent average info")
            #f)))
  
  (define average-runs
    (map (lambda (bm-run)
           (let* ([runss (hash-table-map (cdr bm-run) cons)])
             (cons
              (car bm-run)
              (map (lambda (runs)
                     (let ([nothing-run-times
                            (if (subtract-nothing-run)
                                (let ([a (hash-table-get
                                          (hash-table-get bm-table 'nothing #hash())
                                          (car runs)
                                          #f)])
                                  (if a
                                      ;; compute cpu, real and gc average time for the nothing benchmark
                                      (let ([nothing-runs (map (lambda (x) (map (lambda (y) (or y 0)) x))
                                                               (map car a))])
                                        (map (lambda (x) (exact->inexact (/ x (length nothing-runs))))
                                             (foldl (lambda (x y) (map + x y))
                                                    '(0 0 0)
                                                    nothing-runs)))
                                      '(0 0 0)))
                                '(0 0 0))])
                       (list (car runs)
                             (list (average/coefficient-of-variation caar (car nothing-run-times) (cdr runs))
                                   (average/coefficient-of-variation cadar (cadr nothing-run-times) (cdr runs))
                                   (average/coefficient-of-variation caddar (caddr nothing-run-times) (cdr runs)))
                             (let ([nothing-compile-time
                                    (if (subtract-nothing)
                                        (let ([a (hash-table-get
                                                  (hash-table-get bm-table 'nothing #hash())
                                                  (car runs)
                                                  #f)])
                                          (if a
                                              (cadar a)
                                              0))
                                        0)])
                               (max (- (or (cadadr runs) 0)
                                       nothing-compile-time)
                                    0)))))
                   runss))))
         (if (or (subtract-nothing) (subtract-nothing-run))
             (filter (lambda (v)
                       (not (eq? (car v) 'nothing)))
                     bm-runs)
             bm-runs)))

  (define (symbol<? a b)
    (string<? (symbol->string a)
              (symbol->string b)))

  (define (mode<? a b)
    (let ([am (extract-column a 'mode)]
          [bm (extract-column b 'mode)])
      (if (equal? am bm)
          (symbol<? a b)
          (string<? am bm))))

  (define (extract-column impl grouping)
    (let ([s (symbol->string impl)])
      (cond
       [(regexp-match #rx"^(.*)@(.*)" s)
        => (lambda (m)
             (if (eq? grouping 'impl)
                 (cadr m)
                 (caddr m)))]
       [else s])))

  (define sorted-runs 
    (sort average-runs (lambda (a b)
                         (symbol<? (car a) (car b)))))

  (define sorted-impls
    (sort (hash-table-map impls (lambda (k v) k)) symbol<?))

  (define mode-sorted-impls
    (sort (hash-table-map impls (lambda (k v) k))
          mode<?))

  (define (opposite grouping)
    (if (eq? grouping 'mode)
        'impl
        'mode))

  (define (ratio->string r)
    (if (integer? r)
        (number->string r)
        (let ([s (format "~a00" (exact->inexact r))])
          (car (regexp-match #rx"^[0-9]*[.].." s)))))

  (define (small s)
    `(font ((color "gray")
            (size "-2"))
           ,s))

  (define (wrap-page relative-to . ps)
    (if (full-page-mode)
        (let ([title (format "~a normalized to ~a~a"
                             (or (base-link-filename)
                                 "results")
                             (if (string? relative-to)
                                 "fastest "
                                 "")
                             (or relative-to
                                 "fastest"))])
          `(html
            (head (title ,title)
                  (body
                   (p 
                    (b ,title ".") 
                    " See also " (a ((href "index.html"))
                                   "about the benchmarks")
                    ".")
                   ,@(map (lambda (p) `(p ,p))
                          ps)))))
        `(html (nbody ,@ps))))

  (define forever 1000000000)

  (define (ntime v)
    (and (caadr v) (- (caaadr v) (or (caaddr (cadr v)) 0))))

  (define (grouping->suffix grouping)
    (if (eq? grouping 'impl)
        ""
        (format "-~a" grouping)))

  (define no-modes? (equal? mode-sorted-impls sorted-impls))

  (define (fixup-filename s)
    (regexp-replace* #rx"[^.a-zA-Z0-9-]" s (lambda (s)
                                             (format "_~x" (char->integer (string-ref s 0))))))

  (define (output-name impl grouping graph?)
    (fixup-filename
     (if impl
         (format "~a-~a~a.html" 
                 (base-link-filename)
                 impl
                 (grouping->suffix grouping))
         (format "~a~a~a.html"
                 (base-link-filename)
                 (grouping->suffix grouping)
                 (if graph? "-plot" "")))))

  (define (resolve-relative-to relative-to grouping runs)
    (if (string? relative-to)
        ;; Find fastest among entries matching `relative-to':
        (car (argmin (lambda (run)
                       (or (caaadr run) forever))
                     (cons (list #f (list #f #f #f) #f)
                           (filter (lambda (run)
                                     (equal? relative-to (extract-column (car run) grouping)))
                                   runs))))
        ;; Nothing to resolve:
        relative-to))

  (define (extract-variants grouping impls)
    (let ([ht (make-hash-table 'equal)])
      (for-each (lambda (impl)
                  (hash-table-put! ht (extract-column impl grouping) #t))
                impls)
      (hash-table-map ht (lambda (k v) k))))

  (define just-impls (sort (extract-variants 'impl sorted-impls) string<?))
  (define all-colors (list "#EEEEDD" "#EEEEFF" "#EEDDEE" "#FFEEEE"
                           "#EEEEEE" "#DDEEEE"))

  (define (lookup-color impl)
    (let ([s (extract-column impl 'impl)])
      (let loop ([impls just-impls]
                 [colors all-colors])
        (cond
         [(null? colors) (loop impls all-colors)]
         [(null? impls) (car colors)]
         [(equal? (car impls) s) (car colors)]
         [else (loop (cdr impls) (cdr colors))]))))

  (define (darken c)
    (regexp-replace*
     #rx"F" 
     (regexp-replace* 
      #rx"E" 
      (regexp-replace* 
       #rx"D"
       c
       "A")
      "B")
     "F"))

  (define (call-with-bm-info bm-run relative-to grouping proc)
    (let ([fastest (apply min (map (lambda (run)
                                     (or (and (caadr run) (caaadr run)) forever))
                                   (cdr bm-run)))]
          [n-fastest (apply min (map (lambda (run)
                                       (or (ntime run) forever))
                                     (cdr bm-run)))]
          [c-fastest (apply min (map (lambda (run)
                                       (let ([v (caddr run)])
                                         (or (and v (positive? v) v)
                                             forever)))
                                     (cdr bm-run)))]
          [relative-to (resolve-relative-to relative-to grouping (cdr bm-run))])
      (let-values ([(base n-base c-base)
                    (if relative-to
                        (let ([a (assq relative-to (cdr bm-run))])
                          (if a
                              (values (caadr a) (ntime a) (caddr a))
                              (values #f #f #f)))
                        (values fastest n-fastest c-fastest))])
        (proc fastest n-fastest c-fastest relative-to
              base n-base c-base))))

  (define (bar-group name content)
    `(tr ((style "background-color: #eeeeee"))
         (td ((valign "top")) ,(symbol->string name))
         (td
          (table
           ((style "border-spacing: 0px;"))
           ,@(content)))))

  (define (bar-plot impl n ratio)
    `(tr (td (span ((style "font-size: small;")) 
                   ,(symbol->string impl))
             nbsp)
         (td ((style "padding: 0em;"))
             ,(if (and n ratio)
                  (let ([col (darken (lookup-color impl))])
                    `(span ((style ,(format "background-color: ~a; color: ~a;" col col)))
                           ,(format (make-string (max (floor (* 60 (if (zero? n) 1 ratio)))
                                                      1)
                                                 #\x))))
                  ""))))

  (define (generate-page relative-to grouping graph? has-other?)
    (empty-tag-shorthand html-empty-tags)
    (write-xml/content 
     (xexpr->xml
      (wrap-page
       relative-to
       (if (not graph?)
           `(table
             ,@(if no-modes?
                   null
                   (list
                    `(tr 
                      (td (i ,(if (eq? grouping 'mode)
                                  "mode"
                                  "impl")))
                      (td nbsp)
                      (td nbsp)
                      ,@(let loop ([impls (if (eq? grouping 'mode)
                                              mode-sorted-impls
                                              sorted-impls)])
                          (if (null? impls)
                              null
                              (let* ([impl (car impls)]
                                     [s (extract-column impl grouping)]
                                     [count (let loop ([impls (cdr impls)])
                                              (cond
                                               [(null? impls) 0]
                                               [(not (equal? s (extract-column (car impls) grouping)))
                                                0]
                                               [else (add1 (loop (cdr impls)))]))])
                                (cons
                                 `(td ((colspan ,(number->string (* (if (no-compile-time) 1 2) (+ 1 count))))
                                       (align "center")
                                       (bgcolor "#DDDDFF"))
                                      (b ,(if (equal? s relative-to)
                                              s
                                              `(a ([href ,(fixup-filename
                                                           (format "~a-~a~a.html"
                                                                   (base-link-filename)
                                                                   s
                                                                   (grouping->suffix grouping)))]) 
                                                  ,s))))
                                 (loop (list-tail impls (+ 1 count))))))))))
             (tr (td ,(if no-modes?
                          'nbsp
                          `(i (a ([href ,(output-name #f (opposite grouping) #f)])
                                 ,(if (eq? grouping 'mode)
                                      "impl"
                                      "mode")))))
                 (td ((colspan ,(if (no-compile-time) "1" "2")) (align "right"))
                     ,(if (and (base-link-filename)
                               relative-to)
                          `(a ((href ,(fixup-filename
                                       (format "~a~a.html" 
                                               (base-link-filename)
                                               (grouping->suffix grouping)))))
                              "fastest")
                          "fastest"))
                 ,@(map (lambda (impl)
                          `(td ((colspan ,(if (no-compile-time) "1" "2")) (align "right"))
                               (b ,(let ([s (extract-column impl (opposite grouping))])
                                     (if (and (base-link-filename)
                                              (not (eq? impl relative-to)))
                                         `(a ((href ,(fixup-filename
                                                      (format "~a-~a~a.html"
                                                              (base-link-filename)
                                                              impl
                                                              (grouping->suffix grouping)))))
                                             ,s)
                                         s)))
                               nbsp))
                        (if (eq? grouping 'mode)
                            mode-sorted-impls
                            sorted-impls))
                 ,@(if has-other?
                       `((td nbsp nbsp (a ((href ,(output-name #f 'impl #t))) "To plots")))
                       null))
             ,@(map (lambda (bm-run)
                      (define orig-relative-to relative-to)
                      (call-with-bm-info
                       bm-run
                       relative-to
                       grouping
                       (lambda (fastest n-fastest c-fastest relative-to
                                        base n-base c-base)
                         `(tr (td ,(if (include-links)
                                       `(a ((href ,(format (string-append "http://git.racket-lang.org/plt/tree/HEAD:/collects/"
                                                                          "tests/racket/benchmarks/common/~a.sch")
                                                           (car bm-run))))
                                           ,(symbol->string (car bm-run)))
                                       (symbol->string (car bm-run))))
                              ,@(if (no-compile-time)
                                    null
                                    `((td ((align "right"))
                                          nbsp
                                          ,(small (if (= c-fastest forever)
                                                      " "
                                                      (number->string c-fastest)))
                                          nbsp)))
                              (td ((align "right"))
                                  ,(format "~a ms" fastest)
                                  nbsp nbsp)
                              ,@(apply
                                 append
                                 (map (lambda (impl)
                                        (let* ([a (assq impl (cdr bm-run))]
                                               [n (and a (caadr a) (caaadr a))]
                                               [coeff-var (and a (caadr a) (cadr (caadr a)))]
                                               [n2 (and a (ntime a))])
                                          `(,@(if (no-compile-time)
                                                  null
                                                  (list
                                                   (if (= c-fastest forever)
                                                       `(td)
                                                       `(td ((align "right")
                                                             (bgcolor ,(lookup-color impl)))
                                                            ,(if (and a (caddr a) c-base (positive? c-base))
                                                                 (small (ratio->string (/ (caddr a) c-base)))
                                                                 '"-")
                                                            nbsp))))
                                            (td ((bgcolor ,(if (and n base (= n base)
                                                                    (or (not orig-relative-to)
                                                                        (and (string? orig-relative-to)
                                                                             (equal? (extract-column impl grouping)
                                                                                     orig-relative-to))))
                                                               "white"
                                                               (lookup-color impl)))
                                                 (align "right"))
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
                                                ,@(if (and coeff-var (coefficient-of-variation))
                                                      `(,(small (let ([s (format " ~a" coeff-var)])
                                                                  (if (>= coeff-var 0.10) ; unreliability threshold, arbitrary
                                                                      `(font ((color "red")) (b ,s))
                                                                      s))))
                                                      null)
                                                nbsp))))
                                      (if (eq? grouping 'mode)
                                          mode-sorted-impls
                                          sorted-impls)))))))
                    sorted-runs))
           `(table
             ((style "border-spacing: 0px 3px;"))
             (tr (td ((colspan "2"))
                     "Longer is better."
                     ,@(if has-other?
                           `(nbsp nbsp (a ((href ,(output-name #f 'impl #f))) "Back to tables"))
                           null)))
             ,(let* ([bm-runs (filter (lambda (bm-run)
                                        (andmap (lambda (impl)
                                                  (let ([a (assq impl (cdr bm-run))])
                                                    (and a (caadr a))))
                                                sorted-impls))
                                      sorted-runs)]
                     [rel-vals (map (lambda (bm-run)
                                      (call-with-bm-info
                                       bm-run
                                       relative-to
                                       grouping
                                       (lambda (fastest n-fastest c-fastest relative-to
                                                        base n-base c-base)
                                         (map (lambda (impl)
                                                (let* ([a (assq impl (cdr bm-run))]
                                                       [n (and a (caadr a) (caaadr a))]
                                                       [coeff-var (and a (caadr a) (cadr (caadr a)))]) ; should be used for error bars
                                                  (list impl (if (zero? n) 1 (/ base n)))))
                                              sorted-impls))))
                                    bm-runs)]
                     [avgs (map (lambda (impl)
                                  (let ([vals (map (lambda (rel-val) (cadr (assq impl rel-val)))
                                                   rel-vals)])
                                    (sqrt (apply + (map (lambda (x) (* x x)) vals)))))
                                sorted-impls)]
                     [max-avg (apply max avgs)])
                (bar-group 'geometric-mean
                           (lambda ()
                             (map (lambda (impl avg)
                                    (bar-plot impl 1 (inexact->exact (/ avg max-avg))))
                                  sorted-impls avgs))))
             ,@(map (lambda (bm-run)
                      (call-with-bm-info
                       bm-run
                       relative-to
                       grouping
                       (lambda (fastest n-fastest c-fastest relative-to
                                        base n-base c-base)
                         (bar-group
                          (car bm-run)
                          (lambda () 
                            (map (lambda (impl)
                                   (let* ([a (assq impl (cdr bm-run))]
                                          [n (and a (caadr a) (caaadr a))]
                                          [coeff-var (and a (caadr a) (cadr (caadr a)))]
                                          [n2 (and a (ntime a))])
                                     (bar-plot impl n (and n base (not (zero? n))
                                                           (/ base n)))))
                                 sorted-impls))))))
                    sorted-runs))))))
    (newline))
  
  (if (base-link-filename)
      (begin
        (for-each (lambda (grouping)
                    (for-each 
                     (lambda (impl)
                       (let ([fn (output-name impl grouping #f)])
                         (fprintf (current-error-port) "Generating ~a\n" fn)
                         (with-output-to-file fn
                           (lambda () (generate-page impl grouping #f #t))
                           'truncate)))
                     (append (cons #f sorted-impls)
                             (if no-modes?
                                 null
                                 (extract-variants grouping sorted-impls)))))
                  (if no-modes?
                      '(impl)
                      '(impl mode)))
        (with-output-to-file (output-name #f 'impl #t)
          (lambda () (generate-page #f 'impl #t #t))
          'truncate))
      (generate-page #f 'impl (generate-graph) #f)))

