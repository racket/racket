#lang racket/base
(require racket/match)

(provide check-global)

(define (check-global linklet global-ok)
  (define es (cdddr linklet))

  ;; Variables that are not under `lambda`:
  (define vars (make-hasheq))

  ;; Get all variables that are declared not under `lambda`. That's
  ;; not necessarily all variables that act like globals, since a
  ;; top-level expression could call a function that allocates a
  ;; mutable variable, but it's close enough to be useful.
  (for ([e (in-list es)])
    (define (loop e)
      (match e
        [`(define-values (,ids ...) ,rhs)
         (for ([id (in-list ids)])
           (hash-set! vars id #t))
         (loop rhs)]
        [`(lambda . ,_) (void)]
        [`(case-lambda . ,_) (void)]
        [`(let-values . ,_)
         (not-under-lambda-let e)]
        [`(letrec-values . ,_)
         (not-under-lambda-let e)]
        [`(,es ...)
         (for ([e (in-list es)])
           (loop e))]
        [_ #f]))
    (define (not-under-lambda-let e)
      (match e
        [`(,_ ([,idss ,rhss] ...) ,bodys ...)
         (for* ([ids (in-list idss)]
                [id (in-list ids)])
           (hash-set! vars id #t))
         (for ([rhs (in-list rhss)])
           (loop rhs))
         (for ([body (in-list bodys)])
           (loop body))]))
    (loop e))

  ;; Variables that are potentially used after the
  ;; linklet body completes
  (define used-later-vars (make-hasheq))

  ;; Exported variable are used later
  (for ([ex (in-list (caddr linklet))])
    (define sym (if (pair? ex) (car ex) ex))
    (hash-set! used-later-vars sym #t))

  ;; Fill `used-later-vars`, because any function not in that set is
  ;; something that we don't need to worry about being called later,
  ;; so we can ignore any side effects it may have. For example,
  ;; `set!`s that just set up recursive bindings are ok, because they
  ;; happen before the linklet body completes, and those are usually
  ;; initialed by functions that are called only on startup.
  (for ([e (in-list es)])
    (define (now e)
      (match e
        [`(define-values (,ids ...) ,rhs)
         (now rhs)]
        [`(lambda ,_  ,bodys ...)
         (for ([body (in-list bodys)])
           (later body))]
        [`(case-lambda [,_ ,bodyss ...] ...)
         (for* ([bodys (in-list bodyss)]
                [body (in-list bodys)])
           (later body))]
        [`(quote ,_) (void)]
        [`(,es ...)
         (unless (null? es)
           (case (car es)
             [(begin begin0 set! if) (void)]
             [else
              ;; The value of any identifier in an argument position
              ;; might be used later
              (for ([e (in-list (cdr es))])
                (when (symbol? e)
                  (later e)))]))
         (for ([e (in-list es)])
           (now e))]
        [_ (void)]))
    (define (later e)
      (match e
        [`(quote ,_) (void)]
        [`(,es ...)
         (for ([e (in-list es)])
           (later e))]
        [_
         (when (symbol? e)
           (hash-set! used-later-vars e #t))]))
    (now e))

  (define complained (make-hasheq))

  (define (found-state! id e)
    (when (hash-ref vars id #f)
      (unless (hash-ref global-ok id #f)
        (unless (hash-ref complained id #f)
          (hash-set! complained id #t)
          (eprintf "Place-spanning global ~s at ~s\n" id e)))))

  ;; A variable acts like a global if it's `set!`ed or if it
  ;; holds a box whose content is mutated, etc. Again, since
  ;; a variable's box could be passed to some other function
  ;; that mutates the box, this check is incomplete, but it's
  ;; likely a useful check.
  (for ([e (in-list es)])
    (let loop ([e e])
      (match e
        [`(define-values (,id) (lambda ,_ ,bodys ...))
         (when (hash-ref used-later-vars id #f)
           (loop bodys))]
        [`(set! ,id ,rhs)
         (found-state! id e)
         (loop rhs)]
        [`(set-box! ,target ,rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop rhs)]
        [`(box-cas! ,target ,rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop rhs)]
        [`(hash-set! ,target ,key-rhs ,val-rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop key-rhs)
         (loop val-rhs)]
        [`(vector-set! ,target ,key-rhs ,val-rhs)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (loop key-rhs)
         (loop val-rhs)]
        [`(will-register ,target ,es ...)
         (if (symbol? target)
             (found-state! target e)
             (loop target))
         (for-each loop es)]
        [`(,es ...)
         (for ([e (in-list es)])
           (loop e))]
        [_ #f])))

  (when (positive? (hash-count complained))
    (exit 1)))
