#lang racket/base
(require racket/format)

(provide parse-times)

(define (parse-times in report
                     #:init-impl [init-impl #f]
                     #:drop-runs [drop-runs 0]
                     #:accum [accum cons]
                     #:base [base '()]
                     #:rel-stdev? [rel-stdev? #f]
                     #:gc? [gc? #f])
  (let loop ([group #f] [impl init-impl] [times null])
    (define l (read-line in))
    (cond
      [(eof-object? l)
       (accum (summary report group impl times drop-runs rel-stdev?)
              base)]
      [else
       (cond
         [(regexp-match #rx"== ([a-zA-Z0-9-]) ==" l)
          => (lambda (m)
               (define first (when group
                               (summary report group impl times drop-runs rel-stdev?)))
               (define rest (loop #f (cadr m) '()))
               (if group
                   (accum first rest)
                   rest))]
         [(regexp-match #rx"'([#:+a-zA-Z0-9-]+)" l)
          => (lambda (m)
               (define first (when group
                               (summary report group impl times drop-runs rel-stdev?)))
               (define rest (loop (cadr m) impl '()))
               (if group
                   (accum first rest)
                   rest))]
         [(and gc?
               (regexp-match #rx"elapsed cpu time, including ([0-9.]+)s collecting" l))
          => (lambda (m)
               (loop group impl (cons (inexact->exact (floor (* 1000 (string->number (cadr m)))))
                                      times)))]
         [(regexp-match #rx"cpu (?:time|msec): ([0-9]+)" l)
          => (lambda (m)
               (loop group impl (cons (string->number (cadr m)) times)))]
         [(or (regexp-match #rx"([0-9.]+)(?:s| secs) (?:CPU time|elapsed cpu time|cpu time)" l)
              (regexp-match #rx"elapsed ([0-9.]+)s" l))
          => (lambda (m)
               (loop group impl (cons (inexact->exact (floor (* 1000 (string->number (cadr m)))))
                                      times)))]
         [(regexp-match #rx"([0-9.]+)u ([0-9.]+)s" l) ;  [0-9]+:[0-9.]+ [0-9.]+%" l)
          => (lambda (m)
               (loop group impl (cons (inexact->exact (floor (* 1000 (+ (string->number (cadr m))
                                                                        (string->number (caddr m))))))
                                      times)))]
         [else (loop group impl times)])])))

(define (summary report group impl times drop-runs rel-stdev?)
  (let* ([times (reverse times)]
         [times (if ((length times) . > . drop-runs)
                    (list-tail times drop-runs)
                    '())])
    (define median (if (null? times)
                       "???"
                       (list-ref (sort times <) (quotient (length times) 2))))
    (define avg (if (null? times)
                    "???"
                    (quotient (apply + times) (length times))))
    (define dev (cond
                  [(null? times) "???"]
                  [(null? (cdr times)) (if rel-stdev?
                                           "0%"
                                           "±0")]
                  [else
                   (let ([stdev (sqrt (/ (for/sum ([t (in-list times)])
                                           (expt (- t avg) 2))
                                         (sub1 (length times))))])
                     (cond
                       [rel-stdev?
                        (string-append
                         (~r #:precision '(= 2)
                             (* 100
                                (/ stdev
                                   avg)))
                         "%")]
                       [else
                        (format "±~a" (inexact->exact (round stdev)))]))]))
    (report group impl (number->string median) (number->string avg) dev)))

(module+ main
  (require racket/cmdline)

  (define NAME-WIDTH 25)
  (define TIME-WIDTH 8)
  (define RSD-WIDTH 10)

  (define line-format "~a~a ~a ~a\n")

  (define init-impl #f)
  (define drop-runs 0)
  (define gc? #f)
  (define sort? #f)
  (define chars-per-unit #f)
  (define inputs null)

  (define lines null)

  (command-line
   #:once-each
   [("--impl") name "Input is for implementation <name>"
               (set! init-impl name)]
   [("--drop") n "Drop first <n> runs"
               (set! drop-runs (string->number n))]
   [("--width") w "Name width as <w>"
                (set! NAME-WIDTH (string->number w))]
   [("--gc") "Collect GC times"
             (set! gc? #t)]
   [("--sort") "Sort by average"
               (set! sort? #t)]
   [("--bars") n "Plot comparative with <n> characters per unit"
               (set! chars-per-unit (string->number n))]
   #:multi
   [("++in") path "Read from <path> for comparative"
             (set! inputs (cons path inputs))]
   #:args ()
   (void))

  (define (pad s n
               #:right? [right? #f])
    (let ([s (format "~a" s)])
      (define padding (make-string (max 0 (- n (string-length s))) #\space))
      (if right?
          (string-append s padding)
          (string-append padding s))))

  (cond
    [(null? inputs)
     ;; stdin, single-implement mode
     
     (define (report group impl median avg dev)
       (printf line-format
               (pad (if impl
                        (format "~a ~a:" group impl)
                        (format "~a:" group))
                    NAME-WIDTH)
               (pad median TIME-WIDTH)
               (pad avg TIME-WIDTH)
               (pad dev RSD-WIDTH)))

     (define (save-or-report group impl median avg dev)
       (cond
         [sort?
          (set! lines (cons (list group impl median avg dev)
                            lines))]
         [else (report group impl median avg dev)]))

     (printf line-format
             (pad "" NAME-WIDTH)
             (pad "median" TIME-WIDTH)
             (pad "mean" TIME-WIDTH)
             (pad "stdev" RSD-WIDTH))

     (parse-times (current-input-port)
                  #:init-impl init-impl
                  #:drop-runs drop-runs
                  save-or-report
                  #:accum void
                  #:gc? gc?)

     (when sort?
       (for ([l (in-list (sort lines <
                               #:key (lambda (p)
                                       (string->number (list-ref p 3)))))])
         (apply report l)))]
    [else
     ;; multi-implementation comparison mode
     (struct entry (median average stddev))

     (define groups (make-hash))
     (define list-groups null)
     
     (define (report group impl med avg dev)
       (unless (hash-ref groups group #f)
         (hash-set! groups group #t)
         (set! list-groups (cons group list-groups)))
       (cons group (entry med avg dev)))
  
     (define name+timess
       (for/list ([input (in-list (reverse inputs))])
         (define-values (base name-path dir?) (split-path input))
         (define name (path->string name-path))
         (define times
           (call-with-input-file*
            input
            (lambda (i)
              (parse-times i
                           #:init-impl name
                           #:drop-runs drop-runs
                           report
                           #:accum (lambda (p ht) (hash-set ht (car p) (cdr p)))
                           #:base #hash()
                           #:gc? gc?))))
         (list name times)))

     (define (format-number n)
       (if chars-per-unit
           (let ([n (inexact->exact (round (* n chars-per-unit)))])
             (if (n . < . chars-per-unit)
                 (make-string n #\=)
                 (string-append (make-string (sub1 chars-per-unit) #\=)
                                "|"
                                (make-string (- n chars-per-unit) #\=))))
           (~r #:precision '(= 2) n)))

     (define SEP 2)

     (define widths
       (for/list ([name+times (in-list name+timess)])
         (for/fold ([n (string-length (car name+times))]) ([group (in-list list-groups)])
           (define base-e (hash-ref (list-ref (car name+timess) 1) group))
           (define base (string->number (entry-median base-e)))
           (define v (string->number (entry-median (hash-ref (list-ref name+times 1) group))))
           (max n (string-length (format-number (/ v base)))))))

     (display (pad "" NAME-WIDTH))
     (for ([name+times (in-list name+timess)]
           [width (in-list widths)])
       (define name (car name+times))
       (display (make-string SEP #\space))
       (display (pad name width #:right? chars-per-unit)))
     (newline)

     (for ([group (in-list (reverse list-groups))])
       (define e (hash-ref (list-ref (car name+timess) 1) group))
       (define base (string->number (entry-median e)))

       (display (pad (format "~a:" group)
                     NAME-WIDTH))
       (display (make-string SEP #\space))
       (display (pad (format-number 1.0)
                     (car widths)
                     #:right? chars-per-unit))
       (for ([name+times (in-list (cdr name+timess))]
             [width (in-list (cdr widths))])
         (define name (car name+times))
         (define v (string->number (entry-median (hash-ref (list-ref name+times 1) group))))
         (display (make-string SEP #\space))
         (display (pad (format-number (/ v base))
                       width
                       #:right? chars-per-unit)))
       (newline))]))

     
