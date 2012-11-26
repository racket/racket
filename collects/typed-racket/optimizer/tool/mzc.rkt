#lang racket/base

;;;; Processing of mzc inliner logs.

(require typed-racket/optimizer/logging
         unstable/syntax racket/match unstable/match racket/list racket/string
         unstable/list
         "profiling.rkt")

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

(define (n-unrollings   group)    (length (filter unrolling?   group)))
(define (n-successes    group) (- (length (filter success?     group))
                                  (n-unrollings group)))
(define (n-failures     group)    (length (filter failure?     group)))
(define (n-out-of-fuels group)    (length (filter out-of-fuel? group)))

;; self out-of-fuels are not interesting, they're the end of loop unrolling
(define (self-out-of-fuel? l) (and (out-of-fuel? l) (self-inline? l)))

(define (any-self-o-o-f? group) (ormap self-out-of-fuel? group))

(define (counts-as-a-missed-opt? group)
  (or (> (n-failures group) 0) ; any straight failure is a problem
      (> (n-out-of-fuels group) (n-successes group)); fails more often than not
      ))


;; We aggregate results for each function.
;; Log messages produced by the inliner are very raw, unlike the TR logs,
;; which have gone through some aggregation. We do the aggregation here.
(define (post-process-inline-log log profile TR-log)
  (define hot-functions (and profile (prune-profile profile)))
  (define grouped-events
    (group-by equal? #:key log-entry-pos log)) ; right file, so that's enough
  (apply
   append
   (append
    (for/list ([group (in-list grouped-events)])
      (process-function group profile hot-functions TR-log))
    (if profile
        (for/list ([node (in-list (profile-nodes profile))])
          (process-profile-node node grouped-events hot-functions TR-log))
        '()))))

;; Process the inlining logs corresponding to a single function.
(define (process-function log profile hot-functions TR-log)
  (define produced-entries '())
  (let/ec escape
    ;; prune this entry from the logs, but return what we produced so far
    (define (prune) (escape produced-entries))
    (match (car log) ; events are grouped, first element is representative
      [(log-entry kind msg stx located-stx pos provenance)

       ;; #f if no profiling info is available for this function
       ;; takes in either a single pos number or a pair of numbers (line col)
       (define (pos->node pos)
         (and profile
              (for/first ([p (in-list (profile-nodes profile))]
                          #:when (if (pair? pos)
                                     (and (equal? (car pos) (node-line p))
                                          (equal? (cdr pos) (node-col  p)))
                                     (equal? pos (node-pos p))))
                p)))
       (define profile-entry (pos->node pos))

       ;; We consider that a function is a loop if it gets inlined in itself
       ;; at least once.
       (define is-a-loop?
         (or (any-self-o-o-f? log) (> (n-unrollings log) 0)))
       ;; From now on, we ignore self-out-of-fuels.
       (set! log (filter (lambda (l) (not (self-out-of-fuel? l))) log))

       (define inlining-sites
         (group-by equal? #:key (lambda (x)
                                  (inlining-event-where-loc
                                   (inliner-log-entry-inlining-event x)))
                   log))

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
       (set! inlining-sites
             (if (not is-a-loop?)
                 inlining-sites
                 ;; `f' is a loop. We ignore anything beyond the first inlining
                 ;; in `g'.
                 (for/list ([site (in-list inlining-sites)])
                   ;; If at least one inlining of `f' in `g', ignore the rest.
                   (or (for/first ([evt (in-list site)] #:when (success? evt))
                         (list evt))
                       site))))

       ;; Some callers are especially interesting if we have profile data.
       ;; If the function under consideration takes a large portion of the
       ;; total time of a given caller, we consider this case interesting.
       ;; This serves as a building block for more interesting patterns, such
       ;; as `key-sites' below.
       ;; returns: caller-profile-node OR #f
       (define interesting-callers
         (and profile-entry
              (filter values
                      (for/list ([edge (node-callers profile-entry)])
                        ;; Does this edge take a "large enough" proportion of
                        ;; the caller's total time?
                        (define caller-node (edge-caller edge))
                        (and (> (edge-caller-time edge)
                                (* (node-total caller-node) 0.3))
                             caller-node)))))

       ;; As above, but consed in front of the inlining info for that caller.
       (define interesting-callers+sites
         (and profile-entry
              ;; Can't map over `inlining-sites', since we also consider
              ;; callers that have no inlining reports at all.
              (for/list ([caller (in-list interesting-callers)])
                (cons caller
                      ;; Find the relevant inlining site information.
                      (or (for/or ([site (in-list inlining-sites)])
                            (match (inlining-event-where-loc
                                    (inliner-log-entry-inlining-event
                                     (car site)))
                              [`(,caller-path ,caller-line ,caller-col)
                               (and (eq? caller
                                         (pos->node (cons caller-line
                                                          caller-col)))
                                    site)]
                              [_ ; can't parse that, give up
                               #f]))
                          '()))))) ; no inlining reports for that caller

       ;; If the function under consideration takes a large portion of the
       ;; total time for a given call site, and is not inlined there, we can
       ;; recommend that the user take a closer look at that specific site.
       ;; returns: `(,caller-profile-node . ,call-site-log-entries) OR #f
       (define key-sites
         (and profile-entry
              (for/list ([site (in-list interesting-callers+sites)]
                         ;; Not inlined enough at that call site.
                         #:when (counts-as-a-missed-opt? (cdr site)))
                site)))

       (define pruned-log (apply append inlining-sites))

       (define recommendation
         (cond [is-a-loop?
                "Consider making this function smaller to encourage inlining."]
               [else
                ;; Non-recursive function -> macro
                "Consider turning this function into a macro to force inlining."]))

       ;; Produce as many log entries as necessary.
       (define (emit e) (set! produced-entries (cons e produced-entries)))
       (define (emit-near-miss msg badness)
         (emit (missed-opt-log-entry
                kind
                (format "Missed Inlining ~a\n~a~a"
                        (format-aggregation-string pruned-log)
                        (if msg (format "~a\n" msg) "")
                        recommendation)
                stx located-stx pos provenance
                '() '()
                badness)))
       (define (emit-success)
         (emit (opt-log-entry
                kind
                (format "Inlining ~a" (format-aggregation-string pruned-log))
                stx located-stx pos provenance)))

       (define inside-hot-function?
         (and profile (memq profile-entry hot-functions)))

       (define (pos-inside-us? pos)
         (define our-pos  (syntax-position located-stx))
         (define our-span (syntax-span     located-stx))
         (and pos our-pos our-span (<= our-pos pos (+ our-pos our-span))))
       (define (inside-us? h)
         (pos-inside-us? (node-pos h)))
       ;; To catch hot curried functions.
       ;; Turns out to be useful for the ray tracer, but increases false
       ;; positives for functions with hot loops inside that otherwise are
       ;; uninteresting wrt inlining.
       (define really-hot-anonymous-function-inside-us?
         (and hot-functions
              ;; list is sorted in increasing order of time
              (ormap (lambda (x) (and (inside-us? x)
                                      (not (node-id x)))) ; anonymous
                     ;; TODO try dropping 3/4
                     (drop hot-functions (quotient (length hot-functions) 2)))))

       ;; If we know which regions are hot, prune reports about cold
       ;; regions. If we don't know, err on the side of showing more.
       ;; We don't want to prune earlier, since traversing cold functions can
       ;; give us advice about hot functions.
       (when (and profile
                  (not inside-hot-function?)
                  (not really-hot-anonymous-function-inside-us?))
         (prune))

       (cond [(and profile-entry (not (null? key-sites)))
              ;; Inlining was not satisfactory for some call sites where we
              ;; accounted for a good portion of the caller's total time.
              (emit-near-miss
               (format "Key call site~a: ~a"
                       (if (> (length key-sites) 1) "s" "")
                       (string-join
                        (for/list ([site (in-list key-sites)])
                          (define node (car site))
                          (format "~a ~a:~a"
                                  (node-id   node)
                                  (node-line node)
                                  (node-col  node)))
                        ", "))
               ;; only compute badness for the interesting sites
               (group-badness (apply append (map cdr key-sites))))]
             [(counts-as-a-missed-opt? pruned-log)
              ;; Overall inlining ratio is not satisfactory.
              (emit-near-miss #f (group-badness pruned-log))]
             [else
              ;; Satisfactory.
              (emit-success)])

       produced-entries]))) ; return the list of new entries

(define (process-profile-node profile-entry grouped-events hot-functions TR-log)

  (define produced-entries '())
  (define (emit e) (set! produced-entries (cons e produced-entries)))

  (define inside-hot-function? (memq profile-entry hot-functions))

  (define (pos-inside-us? pos)
    (define our-pos  (node-pos  profile-entry))
    (define our-span (node-span profile-entry))
    (and pos our-pos our-span (<= our-pos pos (+ our-pos our-span))))

  (when inside-hot-function?
    (for ([TR-entry (in-list TR-log)]
          #:when (info-log-entry? TR-entry)
          #:when (equal? (log-entry-kind TR-entry) "hidden parameter")
          #:when (pos-inside-us? (log-entry-pos TR-entry)))
      (emit (missed-opt-log-entry
             "" ; kind not used at this point
             (string-append "This function may implicitly dereference the "
                            "`current-output-port' parameter. " ;; TODO hard coded
                            "It may be faster to take the value of the "
                            "parameter once, outside hot code, and pass it "
                            "to this function as an argument.")
             (log-entry-located-stx TR-entry) (log-entry-located-stx TR-entry)
             (log-entry-pos TR-entry) 'typed-racket
             '() '()
             20)))) ;; TODO have actual badness

  (when inside-hot-function?
    (for ([TR-entry (in-list TR-log)]
          #:when (info-log-entry? TR-entry)
          #:when (equal? (log-entry-kind TR-entry) "struct constructor")
          #:when (pos-inside-us? (log-entry-pos TR-entry)))
      (emit (missed-opt-log-entry
             "" ; kind not used at this point
             (string-append
              "This struct constructor is used in hot code. "
              "Allocating structs is expensive, consider using vectors instead. "
              "To keep the same interface, consider defining macro wrappers "
              "around the vector operations that have the same name as the "
              "struct constructor and accessors.")
             (log-entry-located-stx TR-entry) (log-entry-located-stx TR-entry)
             (log-entry-pos TR-entry) 'typed-racket
             '() '()
             20)))) ;; TODO have actual badness

  produced-entries)

(define (group-badness group)
  (+ (n-failures group) (- (n-out-of-fuels group) (n-successes group))))

(define (format-aggregation-string group)
  ;; Integer String #:suffix String -> (U Null (List String))
  ;; if n = 0, nothing, if n = 1 singular, o/w plural
  (define (pluralize n noun #:suffix [suffix "s"])
    (format "~a ~a~a" n noun (if (> n 1) suffix "")))
  (define n-u (n-unrollings group))
  (define n-s (n-successes  group))
  (format "(~a out of ~a~a)"
          (pluralize n-s "success" #:suffix "es")
          (+ n-s (n-failures group) (n-out-of-fuels group))
          (if (> n-u 0)
              (format " and ~a" (pluralize n-u "unrolling"))
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
