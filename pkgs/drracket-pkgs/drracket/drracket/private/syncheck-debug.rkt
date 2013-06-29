#lang mzscheme

  (require mzlib/pretty
           mzlib/list
           mzlib/class
           mred)
  
  (provide debug-origin)  ;; : syntax [syntax] -> void
  ;; creates a frame for examining the 
  ;; origin and source fields of an expanded sexp
  ;; also the 'bound-in-source syntax property
  
  (define debug-origin
    (case-lambda 
      [(original-object) (debug-origin original-object (expand original-object))]
      [(original-object expanded-object)
       (define-values (expanded-datum stx-ht) (syntax-object->datum/ht expanded-object))
       
       (define output-text (make-object text%))
       (define output-port (make-text-port output-text))
       (define info-text (make-object text%))
       (define info-port (make-text-port info-text))
       
       ;; assume that there aren't any eq? sub structures, only eq? flat stuff (symbols, etc)
       ;; this is guaranteed by syntax-object->datum/ht
       (define range-start-ht (make-hash-table))
       (define range-ht (make-hash-table))
       (define original-output-port (current-output-port))
       (define (range-pretty-print-pre-hook x v)
         (hash-table-put! range-start-ht x (send output-text last-position)))
       (define (range-pretty-print-post-hook x v)
         (hash-table-put! range-ht x 
                          (cons
                           (cons
                            (hash-table-get range-start-ht x)
                            (send output-text last-position))
                           (hash-table-get range-ht x (λ () null)))))
       
       (define (make-modern text)
         (send text change-style
               (make-object style-delta% 'change-family 'modern)
               0
               (send text last-position)))
       
       (define dummy
         (begin (pretty-print (syntax-object->datum original-object) output-port)
                (newline output-port)
                (parameterize ([current-output-port output-port]
                               [pretty-print-pre-print-hook range-pretty-print-pre-hook]
                               [pretty-print-post-print-hook range-pretty-print-post-hook]
                               [pretty-print-columns 30])
                  (pretty-print expanded-datum))
                (make-modern output-text)))
       
       (define ranges 
         (sort 
          (apply append (hash-table-map range-ht (λ (k vs) (map (λ (v) (cons k v)) vs))))
          (λ (x y)
            (<= (- (car (cdr x)) (cdr (cdr x)))
                (- (car (cdr y)) (cdr (cdr y)))))))
       
       (define (show-info stx)
         (fprintf info-port "datum: ~s\nsource: ~a\nposition: ~s\noffset: ~s\noriginal: ~s\nbound-in-source: ~s\n\n"
                  (syntax-object->datum stx)
                  (syntax-source stx)
                  (syntax-position stx)
                  (syntax-span stx)
                  (syntax-original? stx)
                  (syntax-property stx 'bound-in-source))
         (let loop ([origin (syntax-property stx 'origin)])
           (cond
             [(pair? origin)
              (loop (car origin))
              (loop (cdr origin))]
             [(syntax? origin)
              (display "  " info-port)
              (display origin info-port)
              (newline info-port)
              (fprintf info-port
                       "  original? ~a\n  datum:\n  ~a\n\n"
                       (and (syntax? origin) (syntax-original? origin))
                       (and (syntax? origin) (syntax-object->datum origin)))]
             [else (void)])))
       
       (for-each
        (λ (range)
          (let* ([obj (car range)]
                 [stx (hash-table-get stx-ht obj)]
                 [start (cadr range)]
                 [end (cddr range)])
            (when (syntax? stx)
              (send output-text set-clickback start end 
                    (λ _ 
                      (send info-text begin-edit-sequence)
                      (send info-text erase)
                      (show-info stx)
                      (make-modern info-text)
                      (send info-text end-edit-sequence))))))
        ranges)
       
       (newline output-port)
       (newline output-port)
       (let ([before (send output-text last-position)])
         (display "all" output-port)
         (send output-text set-clickback
               before
               (send output-text last-position)
               (λ _
                 (send info-text begin-edit-sequence)
                 (send info-text erase)
                 (for-each (λ (rng)
                             (let ([stx (hash-table-get stx-ht (car rng))])
                               (when (syntax? stx)
                                 (show-info stx))))
                           ranges)
                 (make-modern info-text)
                 (send info-text end-edit-sequence))))
       
       (let ()
         (define f (make-object frame% "Syntax 'origin Browser" #f 600 300))
         (define p (make-object horizontal-panel% f))
         (make-object editor-canvas% p output-text)
         (make-object editor-canvas% p info-text)
         (send f show #t))]))
  
  ;; build-ht : stx -> hash-table
  ;; the resulting hash-table maps from the each sub-object's to its syntax.
  (define (syntax-object->datum/ht stx)
    (let ([ht (make-hash-table)])
      (values (let loop ([stx stx])
                (let ([obj (syntax-e stx)])
                  (cond
                    [(list? obj) 
                     (let ([res (map loop obj)])
                       (hash-table-put! ht res stx)
                       res)]
                    [(pair? obj) 
                     (let ([res (cons (loop (car obj))
                                      (loop (cdr obj)))])
                       (hash-table-put! ht res stx)
                       res)]
                    [(vector? obj) 
                     (let ([res (list->vector (map loop (vector->list obj)))])
                       (hash-table-put! ht res stx)
                       res)]
                    [else 
                     (let ([res (syntax-object->datum stx)])
                       (hash-table-put! ht res stx)
                       res)])))
              ht)))
  
  ;; make-text-port : text -> port
  ;; builds a port from a text object.  
  (define (make-text-port text)
    (let-values ([(in out) (make-pipe)])
      (thread
       (λ ()
         (let loop ()
           (let ([c (read-char in)])
             (unless (eof-object? c)
               (send text insert (string c)
                     (send text last-position)
                     (send text last-position))
               (loop))))))
      out))
