#lang racket/base

;; Library by: Carl Eastlund <cce@ccs.neu.edu>
;; intended for use in racket/contract, so don't try to add contracts!

(provide

 ;; type predicates
 source-location?
 source-location-list?
 source-location-vector?

 ;; error checks
 check-source-location!

 ;; conversion and combination
 build-source-location
 build-source-location-list
 build-source-location-vector
 build-source-location-syntax

 ;; accessors
 source-location-known?
 source-location-source
 source-location-line
 source-location-column
 source-location-position
 source-location-span
 source-location-end

 ;; update
 update-source-location

 ;; rendering
 source-location->string
 source-location->prefix

 )

(require
  setup/path-to-relative)

(define (source-location? x)
  (process-source-location x good? bad? 'source-location?))

(define (source-location-list? x)
  (process-list x good? bad? 'source-location-list?))

(define (source-location-vector? x)
  (process-vector x good? bad? 'source-location-vector?))

(define (check-source-location! name x)
  (process-source-location x good! bad! name))

(define (source-location-known? x)
  (process-source-location x good-known? bad! 'source-location-known?))

(define (source-location-source x)
  (process-source-location x good-source bad! 'source-location-source))

(define (source-location-line x)
  (process-source-location x good-line bad! 'source-location-line))

(define (source-location-column x)
  (process-source-location x good-column bad! 'source-location-column))

(define (source-location-position x)
  (process-source-location x good-position bad! 'source-location-position))

(define (source-location-span x)
  (process-source-location x good-span bad! 'source-location-span))

(define (source-location-end x)
  (process-source-location x good-end bad! 'source-location-end))

(define dont-update
  (string->uninterned-symbol "Don't update!"))

(define (update-source-location x
                                #:source [src dont-update]
                                #:line [line dont-update]
                                #:column [col dont-update]
                                #:position [pos dont-update]
                                #:span [span dont-update])
  (process-source-location x (good-update src line col pos span) bad!
                           'update-source-location))

(define (source-location->string x [s ""])
  (process-source-location x (good-string s) bad! 'source-location->string))

(define (source-location->prefix x [s ""])
  (process-source-location x (good-prefix s) bad! 'source-location->prefix))

(define (build-source-location . locs)
  (combine-source-locations locs good-srcloc bad!
                            'build-source-location))

(define (build-source-location-list . locs)
  (combine-source-locations locs good-list bad!
                            'build-source-location-list))

(define (build-source-location-vector . locs)
  (combine-source-locations locs good-vector bad!
                            'build-source-location-vector))

(define (build-source-location-syntax . locs)
  (combine-source-locations locs good-syntax bad!
                            'build-source-location-syntax))

(define (good? x src line col pos span) #t)
(define (bad? fmt . args) #f)

(define (good! x src line col pos span) (void))
(define (bad! fmt . args)
  (raise
   (make-exn:fail:contract
    (apply format fmt args)
    (current-continuation-marks))))

(define (good-known? x src line col pos span)
  (and (or src line col pos span) #t))

(define (good-source x src line col pos span) src)
(define (good-line x src line col pos span) line)
(define (good-column x src line col pos span) col)
(define (good-position x src line col pos span) pos)
(define (good-span x src line col pos span) span)
(define (good-end x src line col pos span) (and pos span (+ pos span)))

(define ((good-update src2 line2 col2 pos2 span2) x src1 line1 col1 pos1 span1)
  (let* ([src (if (eq? src2 dont-update) src1 src2)]
         [line (if (eq? line2 dont-update) line1 line2)]
         [col (if (eq? col2 dont-update) col1 col2)]
         [pos (if (eq? pos2 dont-update) pos1 pos2)]
         [span (if (eq? span2 dont-update) span1 span2)])
    (if (and (eq? src src1)
             (eq? line line1)
             (eq? col col1)
             (eq? pos pos1)
             (eq? span span1))
      x
      (rebuild x src line col pos span))))

(define (rebuild x src line col pos span)
  (cond
   [(syntax? x) (datum->syntax x (syntax-e x) (list src line col pos span) x x)]
   [(srcloc? x) (make-srcloc src line col pos span)]
   [(vector? x) (vector src line col pos span)]
   [(or (list? x) src line col pos span) (list src line col pos span)]
   [else #f]))

(define (good-srcloc x src line col pos span)
  (if (srcloc? x) x (make-srcloc src line col pos span)))

(define (good-list x src line col pos span)
  (if (list? x) x (list src line col pos span)))

(define (good-vector x src line col pos span)
  (if (vector? x) x (vector src line col pos span)))

(define (good-syntax x src line col pos span)
  (cond
   [(syntax? x) x]
   [(or (list? x) (vector? x)) (datum->syntax #f null x)]
   [else (datum->syntax #f null (vector src line col pos span))]))

(define ((good-string default) x src line col pos span)
  (format "~a~a"
          (cond
            [(path-string? src) (path->relative-string/library src)]
            [else (or src default)])
          (if line
            (if col
              (format ":~a.~a" line col)
              (format ":~a" line))
            (if pos
              (if span
                (format "::~a-~a" pos (+ pos span))
                (format "::~a" pos))
              ""))))

(define ((good-prefix default) x src line col pos span)
  (let ([str ((good-string default) x src line col pos span)])
    (if (string=? str "") "" (string-append str ": "))))

(define (combine-source-locations locs good bad name)

  (define (loop loc1 src1 line1 col1 pos1 span1 locs)
    (if (null? locs)
      (good loc1 src1 line1 col1 pos1 span1)
      (process-source-location
       (car locs)
       (lambda (loc2 src2 line2 col2 pos2 span2)
         (combine-two
          src1 line1 col1 pos1 span1
          src2 line2 col2 pos2 span2
          (lambda (loc src line col pos span)
            (loop loc src line col pos span (cdr locs)))))
       bad
       name)))

  (if (null? locs)
    (process-source-location #f good bad name)
    (process-source-location
     (car locs)
     (lambda (loc src line col pos span)
       (loop loc src line col pos span (cdr locs)))
     bad
     name)))

(define (combine-two src1 line1 col1 pos1 span1
                     src2 line2 col2 pos2 span2
                     good)
  (if (and src1 src2 (equal? src1 src2))
    (let-values
        ([(src) src1]
         [(line col)
          (cond
           [(and line1 line2)
            (cond
             [(< line1 line2) (values line1 col1)]
             [(> line1 line2) (values line2 col2)]
             [else (values line1
                           (if (and col1 col2)
                             (min col1 col2)
                             (or col1 col2)))])]
           [line1 (values line1 col1)]
           [line2 (values line2 col2)]
           [else (values #f #f)])]
         [(pos span)
          (cond
           [(and pos1 pos2)
            (let ([pos (min pos1 pos2)])
              (cond
               [(and span1 span2)
                (let ([end (max (+ pos1 span1) (+ pos2 span2))])
                  (values pos (- end pos)))]
               [span1 (values pos (- (+ pos1 span1) pos))]
               [span2 (values pos (- (+ pos2 span2) pos))]
               [else (values pos #f)]))])])
      (good #f src line col pos span))
    (good #f #f #f #f #f #f)))

(define (process-source-location x good bad name)
  (cond
   ;; #f
   [(not x) (process-false x good bad name)]
   ;; srcloc
   [(srcloc? x) (process-srcloc x good bad name)]
   ;; list
   [(or (null? x) (pair? x)) (process-list x good bad name)]
   ;; vector
   [(vector? x) (process-vector x good bad name)]
   ;; syntax
   [(syntax? x) (process-syntax x good bad name)]
   ;; other
   [else
    (bad
     "~a: expected a source location (srcloc struct, syntax object, list, vector, or #f); got: ~e"
     name
     x)]))

(define (process-false x good bad name)
  (process-elements #f good bad name #f #f #f #f #f))

(define (process-srcloc x good bad name)
  (process-elements x good bad name
                    (srcloc-source x)
                    (srcloc-line x)
                    (srcloc-column x)
                    (srcloc-position x)
                    (srcloc-span x)))

(define (process-syntax x good bad name)
  (process-elements x good bad name
                    (syntax-get-source x)
                    (syntax-line x)
                    (syntax-column x)
                    (syntax-position x)
                    (syntax-span x)))

(define (syntax-get-source x)
  (or (syntax-source x)
      (syntax-source-module x #t)))

(define (process-list x good bad name)
  (cond
   [(null? x)
    (bad
     "~a: expected a source location (a list of 5 elements); got an empty list: ~e"
     name
     x)]
   [(list? x)
    (let ([n (length x)])
      (if (= n 5)
        (apply process-elements x good bad name x)
        (bad
         "~a: expected a source location (a list of 5 elements); got a list of ~a elements: ~e"
         name
         n
         x)))]
   [(pair? x)
    (bad
     "~a: expected a source location (a list of 5 elements); got an improper list: ~e"
     name
     x)]
   [else
    (bad
     "~a: expected a source location list; got: ~e"
     name
     x)]))

(define (process-vector x good bad name)
  (if (vector? x)
    (let ([n (vector-length x)])
      (if (= n 5)
        (process-elements x good bad name
                          (vector-ref x 0)
                          (vector-ref x 1)
                          (vector-ref x 2)
                          (vector-ref x 3)
                          (vector-ref x 4))
        (bad
         "~a: expected a source location (a vector of 5 elements); got a vector of ~a elements: ~e"
         name
         n
         x)))
    (bad
     "~a: expected a source location vector; got: ~e"
     name
     x)))

(define (process-elements x good bad name src line col pos span)
  (cond
   [(and line (not (exact-positive-integer? line)))
    (bad
     "~a: expected a source location with a positive line number or #f (second element); got line number ~e: ~e"
     name
     line
     x)]
   [(and col (not (exact-nonnegative-integer? col)))
    (bad
     "~a: expected a source location with a non-negative column number or #f (third element); got column number ~e: ~e"
     name
     col
     x)]
   [(or (and col (not line)) (and (not col) line))
    (bad
     "~a: expected a source location with line number and column number both numeric or both #f; got ~a and ~a respectively: ~e"
     name
     line
     col
     x)]
   [(and pos (not (exact-positive-integer? pos)))
    (bad
     "~a: expected a source location with a positive position or #f (fourth element); got line number ~e: ~e"
     name
     pos
     x)]
   [(and span (not (exact-nonnegative-integer? span)))
    (bad
     "~a: expected a source location with a non-negative span or #f (fifth element); got column number ~e: ~e"
     name
     span
     x)]
   [else (good x src line col pos span)]))

