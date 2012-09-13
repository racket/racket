#lang racket/base

;;;; Processing of mzc inliner logs.

(require typed-racket/optimizer/logging
         unstable/syntax racket/match unstable/match racket/list racket/string
         unstable/list)

(provide mzc-opt-log-message->log-entry
         post-process-inline-log)


;;; Low-level log parsing. Goes from strings to log-entry structs.

(define success-regexp       "inlining: ")
(define failure-regexp       "no inlining: ")
(define out-of-fuel-regexp   "no inlining, out of fuel: ")
(define any-inlining-event-regexp
  (format "^optimizer: (~a)" (string-join (list success-regexp
                                                failure-regexp
                                                out-of-fuel-regexp)
                                          "|")))


(struct inliner-log-entry log-entry (inlining-event) #:prefab)

;; String (message from the mzc optimizer) -> log-entry
(define (mzc-opt-log-message->log-entry l)
  (define evt (parse-inlining-event l))
  (define forged-stx (inlining-event->forged-stx evt))
  (define kind
    (match (inlining-event-kind evt)
      [(and k (== success-regexp))     success-regexp]
      [(and k (== failure-regexp))     failure-regexp]
      [(and k (== out-of-fuel-regexp)) out-of-fuel-regexp]
      [_ (error "Unknown log message type" l)]))
  (inliner-log-entry kind kind
                     forged-stx forged-stx
                     (syntax-position forged-stx)
                     'mzc
                     evt))

(define inlining-event-regexp
  ;; Last bit is `generated?'. We don't care about that.
  ;; The middle elements of the vector are numbers of #f.
  (string-append
   ;; Attempt at making this thing readable.
   any-inlining-event-regexp
   "involving: "
   ;; _What_ gets inlined (or not).
   (string-append ; either a vector with name and source info, or just name
    "("
    "#\\(([^ ]+) "
    "(" "#<path:(.+)>" "|" "([^ ]+)" ")"
    " ([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) [^ ]+\\)"
    "|"
    "([^ ]+)" ; just name, we won't be able to do much with it
    ")")
   ;; _Where_ this happens (in which function, can't get more precise info).
   (string-append
    ;; maybe full path info: path, line, col, name
    ;; path allows `:' as the second character (and first, but not a problem)
    ;; to support absolute windows paths (e.g. C:\...)
    "( in: (([^ :]?[^ ]?[^:]+):([^ :]+):([^ :]+): )?([^ ]+))?"
    ;; maybe module info, useless to us (at least for now)
    "( in module: [^ ]+)?")
   " size: ([^ ]+) threshold: ([^ ]+)"
   "$"))

(struct inlining-event (kind ; success, miss, out of fuel, ...
                        name ; _what_ gets inlined
                        loc  ; (U #f (List path line col pos span))
                        where-name ; _where_ it gets inlined (enclosing fun)
                        where-loc  ; (U #f (List path line col))
                        size ; size of the closure being inlined
                        threshold ; how big of a closure can we inline
                        ;; the last two use the same units
                        ))
(define (parse-inlining-event l)
  (match (regexp-match inlining-event-regexp l)
    [`(,all ,kind
            ,what ,name ,path ,file-path ,unsaved-path ,line ,col ,pos ,span
                  ,only-name
            ,where ,where-loc ,where-path ,where-line ,where-col ,where-name
            ,maybe-module-info
            ,size ,threshold)
     (inlining-event kind
                     (string->symbol (or name only-name))
                     (if only-name
                        #f ; no source location
                        (list (or file-path unsaved-path)
                              (string->number line)
                              (string->number col)
                              (string->number pos)
                              (string->number span)))
                     where-name
                     (if where-loc
                         (list where-path
                               (string->number where-line)
                               (string->number where-col))
                         #f) ; no source location
                     (string->number size)
                     (string->number threshold))]
    [_ (error "ill-formed inlining log entry" l)]))


(define (inlining-event->forged-stx evt)
  (match evt
    [(inlining-event kind name loc where-name where-loc size threshold)
     (datum->syntax #'here name loc)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Log processing. Interprets the log entries, and produces new ones.
;;; This is similar in spirit to the post-processing done for missed-opts in
;;; the TR logger.

(define (success?     l) (equal? success-regexp     (log-entry-kind l)))
(define (failure?     l) (equal? failure-regexp     (log-entry-kind l)))
(define (out-of-fuel? l) (equal? out-of-fuel-regexp (log-entry-kind l)))

;; f gets inlined in f (or tried to)
(define (self-inline? l)
  (match (inliner-log-entry-inlining-event l)
    [(inlining-event kind name loc where-name where-loc size threshold)
     (match* (loc where-loc)
       [((list path line col pos span)
         (list where-path where-line where-col))
        (and (equal? path where-path)
             (= col  where-col)
             (= line where-line))]
       [(hunoz hukairz) #f])])) ; we assume it is not, to be conservative

(define (unrolling? l) (and (success? l) (self-inline? l)))

;; self out-of-fuels are not interesting, they're the end of loop unrolling
(define (self-out-of-fuel? l) (and (out-of-fuel? l) (self-inline? l)))

;; We aggregate results for each function.
;; Log messages produced by the inliner are very raw, unlike the TR logs,
;; which have gone through some aggregation. We do the aggregation here.
(define (post-process-inline-log log)
  (define-values (inliner-logs tr-logs)
    (partition inliner-log-entry? log))
  (define grouped-events
    (group-by equal? #:key log-entry-pos ; right file, so that's enough
              inliner-logs))
  (define new-inline-log-entries
    (for*/list ([g     (in-list  grouped-events)]
                [group (in-value (filter (lambda (x)
                                           (not (self-out-of-fuel? x)))
                                         g))]
                #:when (not (null? group)))
      (define head (car group))
      (match head ; events are grouped, first element is representative
        [(log-entry kind msg stx located-stx pos provenance)

         ;; We consider that a function is a loop if it gets inlined in itself
         ;; at least once.
         ;; We treat loops specially, mostly to avoid spurious reports.
         ;; For instance, if `f' is a loop, and gets inlined in `g' multiple
         ;; times, it's likely to be unrolling. Same for out-of-fuels in `g'.
         ;; Therefore, we don't want to report these as inlinings (or failed
         ;; inlinings). If `g' has multiple call sites for `f', we lose
         ;; precision, and may discard actual inlinings.
         ;; However, we care about `f' being unrolled at least once in `g'.
         ;; If we run out of fuel trying to inline `f' in `g' for the first
         ;; time, we report. The reason for this is that it's possible to
         ;; optimize better if `f''s body inside `g' calls `f' than if `g'
         ;; calls `f' directly. For instance, `f' may be a loop involving
         ;; floats, in which case having all calls to `f' originate from `f''s
         ;; body (as opposed to `g') may make unboxing possible.
         ;; Of course, we lose precision if `g' has multiple call sites to `f'.
         (define n-unrollings    (length (filter unrolling? group)))
         (define any-self-o-o-f? (ormap self-out-of-fuel?   group))
         (define is-a-loop?      (or any-self-o-o-f? (> n-unrollings 0)))
         (define inlining-sites
           (group-by equal? #:key (lambda (x)
                                    (inlining-event-where-loc
                                     (inliner-log-entry-inlining-event x)))
                     group))

         (define pruned-group
           (if (not is-a-loop?)
               group
               ;; `f' is a loop. We ignore anything beyond the first inlining
               ;; in `g'.
               (apply
                append
                (for/list ([site (in-list inlining-sites)]
                           #:when
                           ;; If at least one inlining of `f' in `g', prune.
                           (not (for/or ([evt (in-list site)])
                                  (success? evt))))
                  site))))

         (define n-successes (- (length (filter success?     group)) n-unrollings))
         (define n-failures     (length (filter failure?     group)))
         ;; self o-o-f are already gone at this point
         (define n-out-of-fuels (length (filter out-of-fuel? group)))

         (define aggregation-string
           (format-aggregation-string
            n-successes n-unrollings n-failures n-out-of-fuels))

         ;; This is where the interesting decisions are taken.
         (define counts-as-a-missed-opt?
           (or (> n-failures 0) ; any straight failure is a problem
               (> n-out-of-fuels n-successes) ; we fail more often than not
               ))

         (define recommendation
           (cond [is-a-loop?
                  "Consider making this function smaller to encourage inlining."]
                 [else
                  ;; Non-recursive function -> macro
                  "Consider turning this function into a macro to force inlining."]))

         (if counts-as-a-missed-opt?
             (missed-opt-log-entry
              kind
              (format "Missed Inlining ~a\n~a"
                      aggregation-string recommendation)
              stx located-stx pos provenance
              '() '()
              (+ n-failures (- n-out-of-fuels n-successes))) ; badness
             (opt-log-entry
              kind
              (format "Inlining ~a" aggregation-string)
              stx located-stx pos provenance))])))
  (append tr-logs new-inline-log-entries))

(define (format-aggregation-string
         n-successes n-unrollings n-failures n-out-of-fuels)
  ;; Integer String #:suffix String -> (U Null (List String))
  ;; if n = 0, nothing, if n = 1 singular, o/w plural
  (define (pluralize n noun #:suffix [suffix "s"])
    (format "~a ~a~a" n noun (if (> n 1) suffix "")))
  (format "(~a out of ~a~a)"
          (pluralize n-successes "success" #:suffix "es")
          (+ n-successes n-failures n-out-of-fuels)
          (if (> n-unrollings 0)
              (format " and ~a" (pluralize n-unrollings   "unrolling"))
              "")))



(module+ test
  (require rackunit)

  ;; log parsing tests

  (define (parse l) (regexp-match inlining-event-regexp l))

  ;; Windows path
  (check-equal? (parse "optimizer: no inlining, out of fuel: involving: #(.../private/map.rkt:22:14 #<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt> 22 14 620 335 #t) in: C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: prova2 in module: 'anonymous-module size: 55 threshold: 8")
                '("optimizer: no inlining, out of fuel: involving: #(.../private/map.rkt:22:14 #<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt> 22 14 620 335 #t) in: C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: prova2 in module: 'anonymous-module size: 55 threshold: 8"
                  "no inlining, out of fuel: "
                  "#(.../private/map.rkt:22:14 #<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt> 22 14 620 335 #t)"
                  ".../private/map.rkt:22:14"
                  "#<path:C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt>"
                  "C:\\Users\\bernardip\\Documents\\Local\\RacketPortable\\App\\Racket\\collects\\racket\\private\\map.rkt"
                  #f
                  "22"
                  "14"
                  "620"
                  "335"
                  #f
                  " in: C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: prova2"
                  "C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt:23:0: "
                  "C:\\Users\\bernardip\\Documents\\Scheme\\fotografia.rkt"
                  "23"
                  "0"
                  "prova2"
                  " in module: 'anonymous-module"
                  "55"
                  "8"))

  (check-equal? (parse "optimizer: no inlining, out of fuel: involving: #(sqr #<path:/home/stamourv/src/plt/collects/racket/math.rkt> 35 2 838 93 #f) in: /home/stamourv/src/examples/example-shapes.rkt:41:0: inC in module: 'example-shapes size: 21 threshold: 6")
                '("optimizer: no inlining, out of fuel: involving: #(sqr #<path:/home/stamourv/src/plt/collects/racket/math.rkt> 35 2 838 93 #f) in: /home/stamourv/src/examples/example-shapes.rkt:41:0: inC in module: 'example-shapes size: 21 threshold: 6"
                  "no inlining, out of fuel: "
                  "#(sqr #<path:/home/stamourv/src/plt/collects/racket/math.rkt> 35 2 838 93 #f)"
                  "sqr"
                  "#<path:/home/stamourv/src/plt/collects/racket/math.rkt>"
                  "/home/stamourv/src/plt/collects/racket/math.rkt"
                  #f
                  "35"
                  "2"
                  "838"
                  "93"
                  #f
                  " in: /home/stamourv/src/examples/example-shapes.rkt:41:0: inC"
                  "/home/stamourv/src/examples/example-shapes.rkt:41:0: "
                  "/home/stamourv/src/examples/example-shapes.rkt"
                  "41"
                  "0"
                  "inC"
                  " in module: 'example-shapes"
                  "21"
                  "6"))

  (check-equal? (parse "optimizer: inlining: involving: #(inC #<path:/home/stamourv/src/examples/example-shapes.rkt> 41 0 993 165 #f) in: /home/stamourv/src/examples/example-shapes.rkt:27:0: in in module: 'example-shapes size: 41 threshold: 128")
                '("optimizer: inlining: involving: #(inC #<path:/home/stamourv/src/examples/example-shapes.rkt> 41 0 993 165 #f) in: /home/stamourv/src/examples/example-shapes.rkt:27:0: in in module: 'example-shapes size: 41 threshold: 128"
                  "inlining: "
                  "#(inC #<path:/home/stamourv/src/examples/example-shapes.rkt> 41 0 993 165 #f)"
                  "inC"
                  "#<path:/home/stamourv/src/examples/example-shapes.rkt>"
                  "/home/stamourv/src/examples/example-shapes.rkt"
                  #f
                  "41"
                  "0"
                  "993"
                  "165"
                  #f
                  " in: /home/stamourv/src/examples/example-shapes.rkt:27:0: in"
                  "/home/stamourv/src/examples/example-shapes.rkt:27:0: "
                  "/home/stamourv/src/examples/example-shapes.rkt"
                  "27"
                  "0"
                  "in"
                  " in module: 'example-shapes"
                  "41"
                  "128"))
  (check-equal? (parse "optimizer: no inlining, out of fuel: involving: #(sqr #<path:/Applications/Racket v5.3/collects/racket/math.rkt> 35 2 838 93 #f) in: /Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: inC in module: 'anonymous-module size: 21 threshold: 6")
                '("optimizer: no inlining, out of fuel: involving: #(sqr #<path:/Applications/Racket v5.3/collects/racket/math.rkt> 35 2 838 93 #f) in: /Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: inC in module: 'anonymous-module size: 21 threshold: 6"
                  "no inlining, out of fuel: "
                  "#(sqr #<path:/Applications/Racket v5.3/collects/racket/math.rkt> 35 2 838 93 #f)"
                  "sqr"
                  "#<path:/Applications/Racket v5.3/collects/racket/math.rkt>"
                  "/Applications/Racket v5.3/collects/racket/math.rkt"
                  #f
                  "35"
                  "2"
                  "838"
                  "93"
                  #f
                  " in: /Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: inC"
                  "/Users/user/Desktop/Optimization Coach/example-shapes.rkt:41:0: "
                  "/Users/user/Desktop/Optimization Coach/example-shapes.rkt"
                  "41"
                  "0"
                  "inC"
                  " in module: 'anonymous-module"
                  "21"
                  "6")))
