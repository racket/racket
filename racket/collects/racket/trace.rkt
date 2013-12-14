#lang racket/base

(module trace-et-al racket/base
  (require racket/pretty
           (for-syntax racket/base))

  (provide trace untrace
           current-trace-print-results
           current-trace-print-args
           trace-call
           current-trace-notify
           current-prefix-out current-prefix-in)

  (define max-dash-space-depth 10)
  (define number-nesting-depth 6)
  (define current-prefix-out (make-parameter "<"))
  (define current-prefix-in (make-parameter ">"))

  (define (as-spaces s)
    (make-string (string-length s) #\space))

  (define-struct prefix-entry (for-first for-rest))

  (define prefixes (make-hash))

  (define (lookup-prefix n label)
    (hash-ref prefixes (cons n label) (lambda () #f)))

  (define (insert-prefix n label first rest)
    (hash-set! prefixes (cons n label) (make-prefix-entry first rest)))

  (define (construct-prefixes level label)
    (let loop ([n level]
              [first (list label)]
              [rest '(" ")])
      (if (>= n max-dash-space-depth)
        (let-values ([(pre-first pre-rest)
                      (build-prefixes number-nesting-depth label)])
          (let ((s (number->string level)))
            (values
              (string-append pre-first "[" s "] ")
              (string-append pre-rest " " (as-spaces s) " "))))
        (cond
          [(= n 0) (values (apply string-append (reverse first))
                          (apply string-append (reverse rest)))]
          [(= n 1) (loop (- n 1)
                        (cons '" " first)
                        (cons '" " rest))]
          [else (loop (- n 2)
                      (cons (format " ~a" label) first)
                      (cons "  " rest))]))))

  (define (build-prefixes level label)
    (let ([p (lookup-prefix level label)])
      (if p
        (values (prefix-entry-for-first p)
                (prefix-entry-for-rest p))
        (let-values (((first rest)
                      (construct-prefixes level label)))
          (insert-prefix level label first rest)
          (values first rest)))))

  (define current-trace-notify
    (make-parameter (lambda (s)
                      (display s)
                      (newline))
                    (lambda (p)
                      (unless (and (procedure? p)
                                  (procedure-arity-includes? p 1))
                        (raise-argument-error 'current-trace-notify
                                              "(any/c . -> . any)"
                                              p))
                      p)))

  (define (as-trace-notify thunk)
    (let ([p (open-output-bytes)])
      (parameterize ([current-output-port p])
        (thunk))
      (let ([b (get-output-bytes p #t 0
                                ;; drop newline:
                                (sub1 (file-position p)))])
        ((current-trace-notify) (bytes->string/utf-8 b)))))

  (define -:trace-print-args
    (lambda (name args kws kw-vals level)
      (as-trace-notify
        (lambda ()
          ((current-trace-print-args) name args kws kw-vals level)))))

  (struct plain (val)
          #:property prop:custom-write (lambda (p port mode)
                                        (write (plain-val p) port)))

  (define current-trace-print-args
    (make-parameter
      (lambda (name args kws kw-vals level)
        (let-values (((first rest)
                      (build-prefixes level (current-prefix-in))))
          (parameterize ((pretty-print-print-line
                          (lambda (n port offset width)
                            (display
                              (if n
                                (if (zero? n) first (format "\n~a" rest))
                                "\n")
                              port)
                            (if n
                              (if (zero? n)
                                (string-length first)
                                (string-length rest))
                              0))))
            ;; Printing the function call in a way that adapts to
            ;; different value printing --- currently a hack
            (cond
              [(print-as-expression)
              ;; In expression mode, represent a function call as a
              ;; transparent structure, so that it prints as a constructor
              ;; application. Also, protect keywords for keyword arguments
              ;; so that they print without quoting.
              (let ([args (append args
                                  (apply append (map (lambda (kw val)
                                                        (list (plain kw) val))
                                                      kws
                                                      kw-vals)))])
                (let-values ([(struct: make- ? -ref -set!)
                              (make-struct-type name #f
                                                (length args) 0 #f
                                                null #f #f null #f
                                                name)])
                  (pretty-print (apply make- args))))]
              [else
                ;; In non-expression mode, just use `write':
                (pretty-write (append (cons name args)
                                      (apply append (map list kws kw-vals))))]))))))

  (define -:trace-print-results
    (lambda (name results level)
      (as-trace-notify
        (lambda ()
          ((current-trace-print-results) name results level)))))

  (define current-trace-print-results
    (make-parameter
      (lambda (name results level)
        (let-values (((first rest)
                      (build-prefixes level (current-prefix-out))))
          (parameterize ((pretty-print-print-line
                          (lambda (n port offset width)
                            (display
                              (if n
                                (if (zero? n) first (format "\n~a" rest))
                                "\n")
                              port)
                            (if n
                              (if (zero? n)
                                (string-length first)
                                (string-length rest))
                              0))))
            (cond
              ((null? results)
              (pretty-display "*** no values ***"))
              ((null? (cdr results))
              (pretty-print (car results)))
              (else
                (pretty-print (car results))
                (parameterize ((pretty-print-print-line
                                (lambda (n port offset width)
                                  (display
                                    (if n
                                      (if (zero? n) rest (format "\n~a" rest))
                                      "\n")
                                    port)
                                  (if n
                                    (string-length rest)
                                    0))))
                  (for-each pretty-print (cdr results))))))))))


  ;; A traced-proc struct instance acts like a procedure,
  ;;  but preserves the original, too.
  (define-values (struct:traced-proc make-traced-proc traced-proc? traced-proc-ref traced-proc-set!)
    (make-struct-type 'traced-proc #f 2 0 #f
                      (list (cons prop:procedure 0))
                      (current-inspector) #f (list 0 1)))

  ;; Install traced versions of a given set of procedures.  The traced
  ;;  versions are also given, so that they can be constructed to have
  ;;  a nice name.
  (define (do-trace ids procs setters traced-procs)
    (for-each (lambda (id proc)
                (unless (procedure? proc)
                  (error 'trace
                        "the value of ~s is not a procedure: ~e" id proc)))
              ids procs)
    (for-each (lambda (proc setter traced-proc)
                (unless (traced-proc? proc)
                  (setter (make-traced-proc
                            (let-values ([(a) (procedure-arity proc)]
                                        [(req allowed) (procedure-keywords proc)])
                              (procedure-reduce-keyword-arity traced-proc
                                                              a
                                                              req
                                                              allowed))
                            proc))))
              procs setters traced-procs))

  ;; Key used for a continuation mark to indicate
  ;;  the nesting depth:
  (define -:trace-level-key (gensym))

  (define trace-call
    (make-keyword-procedure
      (lambda (id f kws vals . args)
        (do-traced id args kws vals f))
      (lambda (id f . args)
        (do-traced id args '() '() f))))

  ;; Apply a traced procedure to arguments, printing arguments
  ;; and results. We set and inspect the -:trace-level-key continuation
  ;; mark a few times to detect tail calls.
  (define (do-traced id args kws kw-vals real-value)
    (let* ([levels (continuation-mark-set->list
                    (current-continuation-marks)
                    -:trace-level-key)]
          [level (if (null? levels) 0 (car levels))])
      ;; Tentatively push the new depth level:
      (with-continuation-mark -:trace-level-key (add1 level)
                              ;; Check for tail-call => car of levels replaced,
                              ;;  which means that the first two new marks are
                              ;;  not consecutive:
                              (let ([new-levels (continuation-mark-set->list
                                                  (current-continuation-marks)
                                                  -:trace-level-key)])
                                (if (and (pair? (cdr new-levels))
                                        (> (car new-levels) (add1 (cadr new-levels))))
                                  ;; Tail call: reset level and just call real-value.
                                  ;;  (This is in tail position to the call to `do-traced'.)
                                  ;;  We don't print the results, because the original
                                  ;;  call will.
                                  (begin
                                    (-:trace-print-args id args kws kw-vals (sub1 level))
                                    (with-continuation-mark -:trace-level-key (car levels)
                                                            (if (null? kws)
                                                              (apply real-value args)
                                                              (keyword-apply real-value kws kw-vals args))))
                                  ;; Not a tail call; push the old level, again, to ensure
                                  ;;  that when we push the new level, we have consecutive
                                  ;;  levels associated with the mark (i.e., set up for
                                  ;;  tail-call detection the next time around):
                                  (begin
                                    (-:trace-print-args id args kws kw-vals level)
                                    (with-continuation-mark -:trace-level-key level
                                                            (call-with-values
                                                              (lambda ()
                                                                (with-continuation-mark -:trace-level-key (add1 level)
                                                                                        (if (null? kws)
                                                                                          (apply real-value args)
                                                                                          (keyword-apply real-value kws kw-vals args))))
                                                              (lambda results
                                                                (flush-output)
                                                                ;; Print the results:
                                                                (-:trace-print-results id results level)
                                                                ;; Return the results:
                                                                (apply values results))))))))))

  (define-for-syntax (check-ids stx ids)
                    (for ([id (in-list (syntax->list ids))])
                      (unless (identifier? id)
                        (raise-syntax-error #f "not an identifier" stx id)))
                    #t)

  (define-syntax (trace stx)
    (syntax-case stx ()
      [(_ id ...) (check-ids stx #'(id ...))
                  (with-syntax ([(tid ...)
                                (for/list ([id (in-list (syntax->list #'(id ...)))])
                                  (let ([tid (format "traced-~a" (syntax-e id))])
                                    (datum->syntax id (string->symbol tid) #f)))])
                    #'(do-trace
                        '(id ...)
                        (list id ...)
                        (list (lambda (v) (set! id v)) ...)
                        (list (let* ([real-value id]
                                    [tid (make-keyword-procedure
                                            (lambda (kws vals . args)
                                              (do-traced 'id args kws vals real-value))
                                            (lambda args
                                              (do-traced 'id args null null real-value)))])
                                tid)
                              ...)))]))

  (define-syntax (untrace stx)
    (syntax-case stx ()
      [(_ id ...) (check-ids stx #'(id ...))
                  #'(begin (when (traced-proc? id)
                            (set! id (traced-proc-ref id 1)))
                          ...)])))

(module chez-like racket/base
  (require
    (only-in (submod ".." trace-et-al) trace)
    (for-syntax
      racket/base
      syntax/define
      syntax/name
      syntax/parse
      (only-in (submod ".." trace-et-al) trace)))

  (provide trace-define trace-lambda trace-let trace-define-syntax)

  (define-syntax (trace-define stx)
    (syntax-case stx ()
      [(_ e ...)
      (let-values ([(name def) (normalize-definition stx #'lambda)])
        #`(begin (define #,name #,def) (trace #,name)))]))

  (define-syntax trace-let
    (syntax-rules ()
      [(_ name ([x* e*] ...) body ...)
      ((letrec ([name (lambda (x* ...) body ...)]) (trace name) name)
        e* ...)]))

  (define-syntax (trace-lambda stx)
    (define (infer-name-or-error)
      (or (syntax-local-infer-name stx)
        (raise-syntax-error
          'trace-lambda
          "Could not infer name; give a name explicitly using #:name"
          stx)))
    (syntax-parse stx
      [(_ (~optional (~seq #:name name:id) #:defaults ([name #`#,(infer-name-or-error)])) args body:expr ...)
      #'(let ([name (lambda args body ...)]) (trace name) name)]))

  (define-syntax (trace-define-syntax stx)
    (syntax-case stx ()
      [(_ e ...)
      (let-values ([(name def) (normalize-definition stx #'lambda)])
        #`(define-syntax #,name
            (let ([#,name #,def]) (trace #,name) #,name)))])))

(require 'trace-et-al 'chez-like)
(provide trace untrace
         current-trace-print-results
         current-trace-print-args
         trace-call
         current-trace-notify
         current-prefix-out current-prefix-in

         trace-define trace-let trace-lambda
         trace-define-syntax)
