#lang racket/base
(require racket/match
         (only-in racket/linklet linklet-body-reserved-symbol?)
         "linklet.rkt"
         "run.rkt"
         "import.rkt"
         "remap.rkt"
         "log.rkt")

(provide select-names
         find-name
         maybe-find-name)

(define (select-names one-mods
                      phase-runss)
  (define names (make-hash)) ; path/submod+phase+sym -> symbol
  (define transformer-names (make-hash)) ; path/submod+phase+sym -> symbol

  (define used-names (make-hasheq))

  ;; Reserve the syntax-literals and transformer-register names:
  (define reserved-names '(.get-syntax-literal!
                           .set-transformer!))

  (define (pick-name name)
    (let loop ([try-name name] [i 0])
      (cond
        [(or (linklet-body-reserved-symbol? try-name)
             (hash-ref used-names try-name #f))
         (let ([i (add1 i)])
           (loop (string->symbol (format "~a_~a" name i)) i))]
        [else
         (hash-set! used-names try-name #t)
         try-name])))

  (define (find-or-add-name! names use name)
    (cond
      [(hash-ref names (cons use name) #f)
       => (lambda (new-name) new-name)]
      [(memq name reserved-names)
       name]
      [else
       (define new-name (pick-name name))
       (hash-set! names (cons use name) new-name)
       new-name]))

  ;; Names that are defined but not exported from the original
  ;; linklets, so they don't need to be exported after merging:
  (define internals (box '()))
    
  (for* ([phase-runs (in-list phase-runss)]
         [runs (in-hash-values phase-runs)]
         [r (in-list (reverse runs))]) ; biases names to starting module in run
    (define linkl (run-linkl r))
    (define meta-linkl (run-meta-linkl r))
    (define portal-stxes (run-portal-stxes r))
    (define path/submod+phase (cons (run-path/submod r) (run-phase r)))

    (define (select-names! name-list category)
      (for ([name (in-list name-list)])
        (define new-name (find-or-add-name! names path/submod+phase name))
        (when category
          (set-box! category (cons new-name (unbox category))))))

    (define (select-transformer-name! name)
      (find-or-add-name! transformer-names path/submod+phase name))

    (when linkl
      (select-names! (linklet*-internal-exports linkl) #f)

      ;; Since we covered exports first, any other defined name is internal
      (select-names! (linklet*-internals linkl) internals))

    (when meta-linkl
      (remap-names (linklet*-body meta-linkl)
                   (lambda (name) name)
                   #:application-hook
                   (lambda (rator rands remap)
                     (cond
                       [(eq? rator '.set-transformer!)
                        (match rands
                          [`((quote ,name) ,_)
                           (select-transformer-name! name)]
                          [_ (error "unrecognized transformer registration")])]))))

    (for-each select-transformer-name! (hash-keys portal-stxes)))

  (when (log-level? (current-logger) 'debug 'demodularizer)
    (log-demodularizer-debug " Rename:")
    (for ([(path/submod+phase+name new-name) (in-hash names)])
      (define path/submod+phase (car path/submod+phase+name))
      (define name (cdr path/submod+phase+name))
      (define path/submod (car path/submod+phase))
      (define phase (cdr path/submod+phase))
      (log-demodularizer-debug "  ~a ~a ~a -> ~a" name path/submod phase new-name)))

  (values names
          transformer-names
          (unbox internals)
          find-or-add-name!))

(define (find-name names path/submod+phase name)
  (hash-ref names (cons path/submod+phase name)))

(define (maybe-find-name names path/submod+phase name)
  (hash-ref names (cons path/submod+phase name) #f))
