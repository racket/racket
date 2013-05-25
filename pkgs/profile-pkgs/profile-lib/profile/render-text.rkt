#lang at-exp racket/base

(provide render)

(require "analyzer.rkt" "utils.rkt" racket/list racket/string)

(define (f:msec msec)
  (number->string (round (inexact->exact msec))))

(define (f:msec* msec)
  (string-append (f:msec msec) "ms"))

(define (display-table aligns table)
  ;; * thunks are used for cells that are ignored when inspecting widths
  ;; * chars are used for filler cells
  (define (display-line strings)
    (printf "~a\n" (regexp-replace #rx" +$" (string-append* strings) "")))
  (define widths
    (let loop ([table table])
      (define table* (filter pair? table))
      (if (null? table*) '()
          (cons (apply max
                       (filter-map
                        (λ (x) (and (string? (car x)) (string-length (car x))))
                        table*))
                (loop (map cdr table*))))))
  (for ([row (in-list table)])
    (display-line
     (for/list ([cell  (in-list row)]
                [width (in-list widths)]
                [align (in-list aligns)])
       (define cell*
         (cond [(char? cell) (make-string width cell)]
               [(procedure? cell) (cell)]
               [else cell]))
       (define pad
         (make-string (max 0 (- width (string-length cell*))) #\space))
       (case align
         [(l) (string-append cell* pad)]
         [(r) (string-append pad cell*)]
         [else (error 'internal-error "poof")])))))

(define (render profile
                #:truncate-source [truncate-source 50]
                #:hide-self       [hide-self% 1/100]
                #:hide-subs       [hide-subs% 2/100])
  (define (show . xs)
    (let loop ([x xs])
      (cond [(or (not x) (null? x) (void? x)) (void)]
            [(pair? x) (loop (car x)) (loop (cdr x))]
            [else (display x)]))
    (newline))
  (define total-time    (profile-total-time    profile)) ;!! are these two
  (define cpu-time      (profile-cpu-time      profile)) ;!! swapped?
  (define sample-number (profile-sample-number profile))
  (define granularity   (if (zero? sample-number) 0        ;!! this might
                            (/ total-time sample-number))) ;!! be wrong
  (define threads+times (profile-thread-times  profile))
  (define *-node        (profile-*-node profile))
  (define hidden        (get-hidden profile hide-self% hide-subs%))
  (define nodes         (remq* hidden (profile-nodes profile)))
  (define node->
    (let ([t (make-hasheq)])
      (for ([node (in-list nodes)] [idx (in-naturals 1)])
        (define index (format "[~a]" idx))
        (define label (format "~a" (or (node-id node) '???)))
        (hash-set! t node (list index label @string-append{@label @index})))
      (λ (mode node)
        ((case mode [(index) car] [(label) cadr] [(sub-label) caddr])
         (hash-ref t node)))))
  (define (sep ch) (list ch ch ch ch ch ch ch ch ch ch))
  (define =sep (sep #\=))
  (define -sep (sep #\-))
  @show{
    Profiling results
    -----------------
      Total cpu time observed: @f:msec*[total-time] (out of @f:msec*[cpu-time])
      Number of samples taken: @sample-number (once every @f:msec*[granularity])
    }
  (when (> (length threads+times) 1)
    @show{  Threads observed:        @(length threads+times)})
  (when (pair? hidden)
    (define hidden# (length hidden))
    (define nodes#  (length (profile-nodes profile)))
    (define self%   @string-append{self<@(format-percent (or hide-self% 0))})
    (define subs%   @string-append{local<@(format-percent (or hide-subs% 0))})
    (define %s      (cond [(not hide-self%) subs%]
                          [(not hide-subs%) self%]
                          [else @string-append{@self% and @subs%}]))
    @show{  (Hiding functions with @|%s|: @hidden# of @nodes# hidden)})
  (newline)
  (display-table
   '(r l r l l r l l l r l l)
   (append*
    `(,=sep
      ("   " " " "   ""     " " " "  ""     " " " "  Caller")
      ("Idx" " " "To""tal   " " " "Se""lf   " " " "Name+src" "Local%")
      ("   " " " " ms""(pct)" " " "ms""(pct)" " " "  Callee")
      ,=sep)
    (for/list ([node (in-list nodes)])
      (define index  (node-> 'index node))
      (define name   (node-> 'label node))
      (define total  (node-total node))
      (define totalS (f:msec total))
      (define total% @string-append{(@(format-percent total total-time))})
      (define self   (node-self node))
      (define selfS  (f:msec self))
      (define self%  @string-append{(@(format-percent self total-time))})
      (define name+src
        (let* ([src      (format-source (node-src node))]
               [src-len  (string-length src)]
               [name-len (string-length name)])
          (string-append
           name " "
           ;; truncate-source only truncates the source
           (let* ([n (and truncate-source
                          ((+ src-len name-len 1) . - . truncate-source))]
                  [n (and n (positive? n) (- src-len n 3))])
             (cond [(not n) src]
                   [(n . <= . 0) "..."]
                   [else (string-append "..."
                                        (substring src (- src-len n)))])))))
      (define (sub get-edges get-node get-node-time)
        (for*/list ([edge (in-list (get-edges node))]
                    [sub  (in-list (list (get-node edge)))] ; <-- hack...
                    #:unless (or (eq? *-node sub)           ; <-- ...for this
                                 (memq sub hidden)))
          (define name   (node-> 'sub-label sub))
          (define local% (format-percent (get-node-time edge) total))
          `("" "" "" "" "" "" "" ""
            ,(string-append "  " name) ,local%
            "" "")))
      `(,@(reverse (sub node-callers edge-caller edge-caller-time))
        (,(node-> 'index node)
         " " ,totalS ,total%
         " " ,selfS  ,self%
         " " ,(λ () name+src))
        ,@(sub node-callees edge-callee edge-callee-time)
        ,-sep)))))
