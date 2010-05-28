#!/bin/sh
#|
exec racket -t "$0" -- -s -t 60 -v -R $*
|#

#lang scheme
(require compiler/zo-parse
         compiler/zo-marshal
         compiler/decompile
         setup/dirs)

;; Helpers
(define (bytes->hex-string bs)
  (apply string-append
         (for/list ([b bs])
           (format "~a~x"
                   (if (b . <= . 15) "0" "")
                   b))))

(define (show-bytes-side-by-side orig new)
  (define max-length
    (max (bytes-length orig) (bytes-length new)))
  (define BYTES-PER-LINE 38)
  (define lines
    (ceiling (/ max-length BYTES-PER-LINE)))
  (define (subbytes* b s e)
    (subbytes b (min s (bytes-length b)) (min e (bytes-length b))))
  (for ([line (in-range lines)])
    (define start (* line BYTES-PER-LINE))
    (define end (* (add1 line) BYTES-PER-LINE))
    (printf "+ ~a\n" (bytes->hex-string (subbytes* orig start end)))
    (printf "- ~a\n" (bytes->hex-string (subbytes* new start end)))))

(define (bytes-gulp f)
  (with-input-from-file f 
    (λ () (port->bytes (current-input-port)))))

(define (read-compiled-bytes bs)
  (define ib (open-input-bytes bs))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([read-accept-compiled #t])
                    (read ib)))
                (lambda ()
                  (close-input-port ib))))

(define (zo-parse/bytes bs)
  (define ib (open-input-bytes bs))
  (dynamic-wind void
                (lambda ()
                  (zo-parse ib))
                (lambda ()
                  (close-input-port ib))))

(define (bytes-not-equal?-error b1 b2)
  (unless (bytes=? b1 b2)
    (error 'bytes-not-equal?-error "Not equal")))

(define (replace-file file bytes)
  (with-output-to-file file
    (λ () (write-bytes bytes))
    #:exists 'truncate))

(define ((make-recorder! ht) file phase)
  (hash-update! ht phase (curry list* file) empty))

(define (equal?/why-not v1 v2)
  (define v1->v2 (make-hasheq))
  (define (interned-symbol=? s1 s2)
    (symbol=? (hash-ref! v1->v2 s1 s2) s2))
  (define (yield p m v1 v2)
    (error 'equal?/why-not "~a in ~a: ~S ~S"
           m (reverse p) v1 v2))
  (define (inner p v1 v2)
    (unless (eq? v1 v2)
      (match v1
        [(cons car1 cdr1)
         (match v2
           [(cons car2 cdr2)
            (inner (list* 'car p) car1 car2)
            (inner (list* 'cdr p) cdr1 cdr2)]
           [_
            (yield p "Not a cons on right" v1 v2)])]
        [(? vector?)
         (match v2
           [(? vector?)
            (define v1l (vector-length v1))
            (define v2l (vector-length v2))
            (if (= v1l v2l)
                (for ([i (in-range v1l)])
                  (inner (list* `(vector-ref ,i) p)
                         (vector-ref v1 i)
                         (vector-ref v2 i)))
                (yield p "Vector lengths not equal" v1 v2))]
           [_
            (yield p "Not a vector on right" v1 v2)])]
        [(? struct?)
         (match v2
           [(? struct?)
            (define vv1 (struct->vector v1))
            (define vv2 (struct->vector v2))
            (inner (list* `(struct->vector ,(vector-ref vv1 0)) p)
                   vv1 vv2)]
           [_
            (yield p "Not a struct on right" v1 v2)])]
        [(? hash?)
         (match v2
           [(? hash?)
            (let ([p (list* 'in-hash p)])
              (for ([(k1 hv1) (in-hash v1)])
                (define hv2 
                  (hash-ref v2 k1
                            (lambda ()
                              (yield p (format "~S not in hash on right" k1) v1 v2))))
                (inner (list* `(hash-ref ,k1) p)
                       hv1 hv2)))]
           [_
            (yield p "Not a hash on right" v1 v2)])]
        [(? module-path-index?)
         (match v2
           [(? module-path-index?)
            (define-values (mp1 bmpi1) (module-path-index-split v1))
            (define-values (mp2 bmpi2) (module-path-index-split v2))
            (inner (list* 'module-path-index-split_0 p) mp1 mp2)
            (inner (list* 'module-path-index-split_1 p) bmpi1 bmpi2)]
           [_
            (yield p "Not a module path index on right" v1 v2)])]
        [(? string?)
         (match v2
           [(? string?)
            (unless (string=? v1 v2)
              (yield p "Unequal strings" v1 v2))]
           [_
            (yield p "Not a string on right" v1 v2)])]
        [(? bytes?)
         (match v2
           [(? bytes?)
            (unless (bytes=? v1 v2)
              (yield p "Unequal bytes" v1 v2))]
           [_
            (yield p "Not a bytes on right" v1 v2)])]
        [(? path?)
         (match v2
           [(? path?)
            (unless (equal? v1 v2)
              (yield p "Unequal paths" v1 v2))]
           [_
            (yield p "Not a path on right" v1 v2)])]
        [(? number?)
         (match v2
           [(? number?)
            (unless (equal? v1 v2)
              (yield p "Unequal numbers" v1 v2))]
           [_
            (yield p "Not a number on right" v1 v2)])]
        [(? regexp?)
         (match v2
           [(? regexp?)
            (unless (string=? (object-name v1) (object-name v2))
              (yield p "Unequal regexp" v1 v2))]
           [_
            (yield p "Not a regexp on right" v1 v2)])]
        [(? byte-regexp?)
         (match v2
           [(? byte-regexp?)
            (unless (bytes=? (object-name v1) (object-name v2))
              (yield p "Unequal byte-regexp" v1 v2))]
           [_
            (yield p "Not a byte-regexp on right" v1 v2)])]
        [(? box?)
         (match v2
           [(? box?)
            (inner (list* 'unbox) (unbox v1) (unbox v2))]
           [_
            (yield p "Not a box on right" v1 v2)])]
        [(? symbol?)
         (match v2
           [(? symbol?)
            (unless (symbol=? v1 v2)
              (cond
                [(and (symbol-interned? v1) (not (symbol-interned? v1)))
                 (yield p "Not interned symbol on right" v1 v2)]
                [(and (symbol-unreadable? v1) (not (symbol-unreadable? v1)))
                 (yield p "Not unreadable symbol on right" v1 v2)]
                [(and (symbol-uninterned? v1) (not (symbol-uninterned? v1)))
                 (yield p "Not uninterned symbol on right" v1 v2)]
                [(and (symbol-uninterned? v1) (symbol-uninterned? v2))
                 (unless (interned-symbol=? v1 v2)
                   (yield p "Uninterned symbols don't align" v1 v2))]
                [else
                 (yield p "Other symbol-related problem" v1 v2)]))]
           [_
            (yield p "Not a symbol on right" v1 v2)])]
        [(? empty?)
         (yield p "Not empty on right" v1 v2)]
        [_
         (yield p "Cannot inspect values deeper" v1 v2)])))
  (inner empty v1 v2))  

(define (symbol-uninterned? s)
  (not (or (symbol-interned? s) (symbol-unreadable? s))))

;; Parameters
(define stop-on-first-error (make-parameter #f))
(define verbose-mode (make-parameter #f))
(define care-about-nonserious? (make-parameter #t))
(define invariant-output (make-parameter #f))
(define time-limit (make-parameter +inf.0))
(define randomize (make-parameter #f))

;; Work
(define errors (make-hash))

(define (common-message exn)
  (define given-messages (regexp-match #rx".*given" (exn-message exn)))
  (if (and given-messages (not (empty? given-messages)))
      (first given-messages)
      (exn-message exn)))

(define (exn-printer file phase serious? exn)
  (hash-update! errors (common-message exn) add1 0)
  (unless (and (not (care-about-nonserious?)) (not serious?))
    (when (or (verbose-mode) (stop-on-first-error))
      (fprintf (current-error-port) "~a -- ~a: ~a~n" file phase (exn-message exn)))
    (when (stop-on-first-error)
      exn)))

(define (run-with-time-limit t thnk)
  (define th (thread thnk))
  (sync th
        (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                                  (* 1000 t)))
                    (lambda _
                      (kill-thread th)))))

(define (run-with-limit file k thnk)
  (define file-custodian (make-custodian))
  (define ch (make-channel))
  (custodian-limit-memory file-custodian k)
  (local [(define worker-thread
            (parameterize ([current-custodian file-custodian])
              (thread
               (lambda ()
                 (define r (thnk))
                 (channel-put ch r)
                 (channel-get ch)))))]
    (begin0
      (sync
       (handle-evt ch
                   (lambda (v)
                     (when (exn? v) (raise v))
                     v))
       (handle-evt worker-thread
                   (lambda _
                     (failure! file 'memory))))
      (custodian-shutdown-all file-custodian))))

(define success-ht (make-hasheq))
(define success! (make-recorder! success-ht))
(define failure-ht (make-hasheq))
(define failure! (make-recorder! failure-ht))

(define-syntax run/stages*
  (syntax-rules ()
    [(_ file) (success! file 'everything)]
    [(_ file [step1 serious? e] . rst)
     (let/ec esc
       (let ([step1 (with-handlers ([exn:fail? 
                                     (lambda (x)
                                       (failure! file 'step1)
                                       (esc (exn-printer file 'step1 serious? x)))])
                      e)])
         (success! file 'step1)
         (run/stages* file . rst)))]))

(define-syntax-rule (define-stages (stages run!)
                      file
                      [stage serious? e] ...)
  (define-values (stages run!)
    (values '(stage ...)
            (lambda (file)
              (run/stages* file [stage serious? e] ...)))))

(define debugging? (make-parameter #f))

(define (print-bytes orig new)
  (when (debugging?)
    (show-bytes-side-by-side orig new))
  #t)

(define-stages (stages run!)
  file
  [read-orig 
   #t
   (bytes-gulp file)]
  [parse-orig 
   #t
   (zo-parse/bytes read-orig)]
  [marshal-parsed 
   #t
   (zo-marshal parse-orig)]
  #;[ignored 
     #f
     (printf "orig: ~a, marshalled: ~a~n"
             (bytes-length read-orig)
             (bytes-length marshal-parsed))]
  [parse-marshalled 
   #t
   (zo-parse/bytes marshal-parsed)]
  [compare-parsed-to-parsed-marshalled 
   #f
   (equal?/why-not parse-orig parse-marshalled)]
  [marshal-marshalled 
   #t
   (zo-marshal parse-marshalled)]
  [compare-marshalled-to-marshalled-marshalled 
   #f
   (bytes-not-equal?-error marshal-parsed marshal-marshalled)]
  #;[replace-with-marshalled 
     #t
     (replace-file file marshal-marshalled)]
  [decompile-parsed 
   #t
   (decompile parse-orig)]
  [show-orig-and-marshal-parsed
   #f
   (print-bytes read-orig marshal-parsed)]
  [c-parse-marshalled
   #f
   (read-compiled-bytes marshal-parsed)]
  [compare-orig-to-marshalled 
   #f
   (bytes-not-equal?-error read-orig marshal-parsed)])

(define (run-test file)
  (run-with-limit 
   file
   (* 1024 1024 128)
   (lambda ()
     (run! file))))

(define (randomize-list l)
  (define ll (length l))
  (define seen? (make-hasheq))
  (let loop ([t 0])
    (if (= t ll)
        empty
        (let ([i (random ll)])
          (if (hash-has-key? seen? i)
              (loop t)
              (begin (hash-set! seen? i #t)
                     (list* (list-ref l i)
                            (loop (add1 t)))))))))

(define (maybe-randomize-list l)
  (if (randomize) (randomize-list l) l))

(define (for-zos ! p)
  (define p-str (if (path? p) (path->string p) p))
  (cond
    [(directory-exists? p)
     (for ([sp (in-list (maybe-randomize-list (directory-list p)))])
       (for-zos ! (build-path p sp)))]
    [(regexp-match #rx"\\.zo$" p-str)
     (! p-str)]))

(define (zo-test paths)
  (run-with-time-limit 
   (time-limit)
   (lambda ()
     (for-each (curry for-zos run-test) paths)))
  
  (unless (invariant-output)
    (for ([kind-name (list* 'memory stages)])
      (define fails (length (hash-ref failure-ht kind-name empty)))
      (define succs (length (hash-ref success-ht kind-name empty)))
      (define all (+ fails succs))
      (unless (zero? all)
        (printf "~S~n"
                `(,kind-name
                  (#f ,fails)
                  (#t ,succs)
                  ,all))))
    (printf "~a tests passed~n" (length (hash-ref success-ht 'everything empty)))
    
    (printf "Common Errors:~n")
    
    (for ([p (in-list (sort (filter (λ (p) ((car p) . > . 10))
                                    (hash-map errors (λ (k v) (cons v k))))
                            > #:key car))])
      (printf "~a:~n~a~n~n" (car p) (cdr p)))))

; Run
#;(current-command-line-arguments #("-s" "/home/bjohn3x/development/plt/collects/browser/compiled/browser_scrbl.zo"))
(command-line #:program "zo-test" 
              #:once-each
              [("-D") 
               "Enable debugging output"
               (debugging? #t)]
              [("-s" "--stop-on-first-error") 
               "Stop testing when first error is encountered"
               (stop-on-first-error #t)]
              [("-S")
               "Don't take some errors seriously"
               (care-about-nonserious? #f)]
              [("-v" "--verbose")
               "Display verbose error messages"
               (verbose-mode #t)]
              [("-I")
               "Invariant output"
               (invariant-output #t)]
              [("-R")
               "Randomize"
               (randomize #t)]
              [("-t") 
               number
               "Limit the run to a given amount of time"
               (time-limit (string->number number))]
              #:args p
              (zo-test (if (empty? p)
                           (list (find-collects-dir))
                           p)))