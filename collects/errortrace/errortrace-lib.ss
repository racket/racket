
;; Poor man's stack-trace-on-exceptions/profiler.
;; See doc.txt for information.

(module errortrace-lib mzscheme
  (require "stacktrace.ss"
           "errortrace-key.ss"
           (lib "list.ss")
           (lib "unit.ss")
           (lib "runtime-path.ss"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Test coverage run-time support
  (define test-coverage-enabled (make-parameter #f))

  (define test-coverage-info (make-hash-table))

  (define (initialize-test-coverage-point key expr)
    (hash-table-put! test-coverage-info key (cons expr 0)))

  (define (test-covered key)
    (let ([v (hash-table-get test-coverage-info key)])
      (set-cdr! v (add1 (cdr v)))))

  (define (get-coverage-counts)
    (hash-table-map test-coverage-info (lambda (k v) v)))

  (define (annotate-covered-file name . more)
    (apply annotate-file name (get-coverage-counts)
           (if (null? more) '(#f) more)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profiling run-time support

  (define profile-thread #f)
  (define profile-key (gensym))

  (define profiling-enabled (make-parameter #f))
  (define profiling-record-enabled (make-parameter #t))
  (define profile-paths-enabled (make-parameter #f))

  (define profile-info (make-hash-table))

  (define (clear-profile-results)
    (hash-table-for-each profile-info
      (lambda (k v)
        (set-box! (car v) #f)
        (set-car! (cdr v) 0)
        (set-car! (cddr v) 0)
        (set-car! (cdr (cdddr v)) null))))

  (define (initialize-profile-point key name expr)
    (hash-table-put! profile-info key
                     (list (box #f) 0 0 (and name (syntax-e name)) expr null)))

  (define (register-profile-start key)
    (and (profiling-record-enabled)
         (let ([v (hash-table-get profile-info key)])
           (let ([b (car v)]
                 [v (cdr v)])
             (set-car! v (add1 (car v)))
             (when (profile-paths-enabled)
               (let ([v (cddddr v)])
		 (let ([cms
			(continuation-mark-set->list
			 (current-continuation-marks)
			 profile-key)])
		   (unless (hash-table? (car v))
		     (set-car! v (make-hash-table 'equal)))
		   (hash-table-put! (car v) cms 
				    (add1 (hash-table-get (car v) cms (lambda () 0)))))))
             (if (unbox b)
                 #f
                 (begin
                   (set-box! b #t)
                   (current-process-milliseconds)))))))

  (define (register-profile-done key start)
    (when start
      (let ([v (hash-table-get profile-info key)])
        (let ([b (car v)]
              [v (cddr v)])
          (set-box! b #f)
          (let ([v (cddr (hash-table-get profile-info key))])
            (set-car! v (+ (- (current-process-milliseconds) start)
                           (car v))))))))

  (define (get-profile-results)
    (hash-table-map profile-info
      (lambda (key val)
        (let ([count (cadr val)]
              [time (caddr val)]
              [name (cadddr val)]
              [expr (cadddr (cdr val))]
              [cmss (cadddr (cddr val))])
          (list count time name expr
		(if (hash-table? cmss)
		    (hash-table-map cmss (lambda (ks v)
					   (cons v 
						 (map (lambda (k)
							(let ([v (cdr (hash-table-get profile-info k))])
							  (list (caddr v) (cadddr v))))
						      ks))))
		    null))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Stacktrace instrumenter

  (define-runtime-path key-syntax
    '(lib "errortrace-key-syntax.ss" "errortrace"))

  (define dynamic-errortrace-key
    (dynamic-require key-syntax 'errortrace-key-syntax))

  ;; with-mark : stx stx -> stx
  (define (with-mark mark expr)
    (with-syntax ([expr expr]
                  [loc (make-st-mark mark)]
                  [et-key dynamic-errortrace-key])
      (execute-point
       mark
       (syntax
        (with-continuation-mark
            et-key
            loc
          expr)))))

  (define-values/invoke-unit/infer stacktrace@)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Execute counts

  (define execute-info (make-hash-table))

  (define execute-counts-enabled (make-parameter #f))

  (define (register-executed-once key)
    (let ([i (hash-table-get execute-info key)])
      (set-cdr! i (add1 (cdr i)))))

  (define (execute-point mark expr)
    (if (execute-counts-enabled)
        (let ([key (gensym)])
          (hash-table-put! execute-info key (cons mark 0))
          (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
                        [expr expr]
                        [register-executed-once register-executed-once]);<- 3D!
            (syntax
             (begin
               (register-executed-once 'key)
               expr))))
        expr))

  (define (get-execute-counts)
    (hash-table-map execute-info (lambda (k v) v)))

  (define (annotate-executed-file name . more)
    (apply annotate-file name (get-execute-counts)
           (if (null? more) '("^.,") more)))

  ;; shared functionality for annotate-executed-file and annotate-covered-file
  (define (annotate-file name counts display-string)
    (let ([name (path->complete-path name (current-directory))])
      (let* (;; Filter relevant syntaxes
             [here (filter (lambda (s)
                             (and (equal? name (syntax-source (car s)))
                                  (syntax-position (car s))))
                           counts)]
             ;; Sort them: earlier first, wider if in same position
             [sorted (sort here
                           (lambda (a b)
                             (let ([ap (syntax-position (car a))]
                                   [bp (syntax-position (car b))])
                               (or (< ap bp)
                                   (and (= ap bp)
                                        (> (syntax-span (car a))
                                           (syntax-span (car b))))))))]
             ;; Merge entries with the same position+span
             [sorted (if (null? sorted)
                       sorted ; guarantee one element for the next case
                       (let loop ([xs (reverse! sorted)] [r '()])
                         (cond [(null? (cdr xs)) (append xs r)]
                               [(and (= (syntax-position (caar xs))
                                        (syntax-position (caadr xs)))
                                     (= (syntax-span (caar xs))
                                        (syntax-span (caadr xs))))
                                ;; doesn't matter which syntax object is kept,
                                ;; we only care about its position+span
                                (loop (cons (cons (caar xs)
                                                  (max (cdar xs) (cdadr xs)))
                                            (cddr xs))
                                      r)]
                               [else (loop (cdr xs) (cons (car xs) r))])))]
             [pic (make-string (file-size name) #\space)]
             [display-string
              (case display-string
                [(#t) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
                [(#f) "#-"]
                [else display-string])]
             [many-char (string-ref display-string
                                    (sub1 (string-length display-string)))])
        ;; Fill out picture
        (for-each (lambda (s)
                    (let ([pos (sub1 (syntax-position (car s)))]
                          [span (syntax-span (car s))]
                          [key (let ([k (cdr s)])
                                 (if (< k (string-length display-string))
                                   (string-ref display-string k)
                                   many-char))])
                      (let loop ([p pos])
                        (unless (= p (+ pos span))
                          (string-set! pic p key)
                          (loop (add1 p))))))
                  sorted)
        ;; Write annotated file
        (with-input-from-file name
          (lambda ()
            (let loop ()
              (let ([pos (file-position (current-input-port))]
                    [line (read-line (current-input-port) 'any)])
                (unless (eof-object? line)
                  (printf "~a\n" line)
                  (let ([w (string-length line)])
                    ;; Blank leading spaces in pic (copy them: works for tabs)
                    (let loop ([i 0])
                      (when (and (< i w)
                                 (char-whitespace? (string-ref line i)))
                        (string-set! pic (+ pos i) (string-ref line i))
                        (loop (add1 i))))
                    (printf "~a\n" (substring pic pos (+ pos w))))
                  (loop)))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eval handler, exception handler

  (define instrumenting-enabled
    (make-parameter #t))
  (define error-context-display-depth
    (make-parameter 10000 (lambda (x) (and (integer? x) x))))

  ;; port exn -> void
  ;; effect: prints out the context surrounding the exception
  (define (print-error-trace p x)
    (let loop ([n (error-context-display-depth)]
               [l (map st-mark-source
                       (continuation-mark-set->list (exn-continuation-marks x)
                                                    errortrace-key))])
      (cond
        [(or (zero? n) (null? l)) (void)]
        [(pair? l)
         (let* ([stx (car l)]
                [source (syntax-source stx)]
                [file (cond
                       [(string? source) source]
                       [(path? source)
                        (path->string source)]
                       [(not source)
                        #f]
                       [else
                        (format "~a" source)])]
                [line (syntax-line stx)]
                [col (syntax-column stx)]
                [pos (syntax-position stx)])
           (fprintf p "~a~a: ~e~n"
                    (or file "[unknown source]")
                    (cond
                     [line (format ":~a:~a" line col)]
                     [pos (format "::~a" pos)]
                     [else ""])
                    (syntax-object->datum stx))
           (loop (- n 1) (cdr l)))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profile printer

  (define (output-profile-results paths? sort-time?)
    (profiling-enabled #f)
    (error-print-width 50)
    (printf "Sorting profile data...~n")
    (let* ([sel (if sort-time? cadr car)]
           [counts (sort (filter (lambda (c) (positive? (car c)))
                                 (get-profile-results))
                         (lambda (a b) (< (sel a) (sel b))))]
           [total 0])
      (for-each
       (lambda (c)
         (set! total (+ total (sel c)))
         (printf "=========================================================~n")
         (printf "time = ~a : no. = ~a : ~e in ~s~n"
                 (cadr c) (car c) (caddr c) (cadddr c))
         ;; print call paths
         (when paths?
           (for-each
            (lambda (cms)
              (unless (null? (cdr cms))
                (printf "  ~e VIA ~e" (car cms) (caadr cms))
                (for-each
                 (lambda (cm)
                   (printf " <- ~e" (car cm)))
                 (cddr cms))
                (printf "~n")))
	    (sort (cadddr (cdr c)) (lambda (a b) (> (car a) (car b)))))))
       counts)
      (printf "Total samples: ~a~n" total)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define orig-inspector (current-code-inspector))

  (define errortrace-annotate
    (lambda (top-e)
      (define (normal e)
        (let ([ex (expand-syntax e)])
          (annotate-top ex #f)))
      (syntax-case top-e (begin module)
        [(module name . reste)
         (if (eq? (syntax-e #'name) 'errortrace-key)
             top-e
             (let ([top-e (expand-syntax top-e)])
               (syntax-case top-e (module #%plain-module-begin)
                 [(module name init-import (#%plain-module-begin body ...))
                  (normal
                   #`(module name init-import
                       #,(syntax-recertify
			  #`(#%plain-module-begin
			     #,((make-syntax-introducer)
				#'(require (lib "errortrace-key.ss" "errortrace")))
			     #,((make-syntax-introducer)
				#'(require-for-syntax
				   (lib "errortrace-key.ss" "errortrace")))
			     body ...)
			  (list-ref (syntax->list top-e) 3)
			  orig-inspector
			  #f)))])))]
        [_else
         (normal top-e)])))

  (define errortrace-compile-handler
    (let ([orig (current-compile)]
          [ns (current-namespace)])
      (lambda (e immediate-eval?)
        (orig
         (if (and (instrumenting-enabled)
                  (eq? ns (current-namespace))
                  (not (compiled-expression? (if (syntax? e)
                                                 (syntax-e e)
                                                 e))))
             (let ([e2 (errortrace-annotate
                        (if (syntax? e)
                            e
                            (namespace-syntax-introduce
                             (datum->syntax-object #f e))))])
	       e2)
             e)
         immediate-eval?))))

  (define errortrace-error-display-handler
    (let ([orig (error-display-handler)])
      (lambda (msg exn)
        (if (exn? exn)
            (let ([p (open-output-string)])
              (display (exn-message exn) p)
              (newline p)
              (print-error-trace p exn)
              (orig (get-output-string p) exn))
            (orig msg exn)))))

  (provide errortrace-compile-handler
           errortrace-error-display-handler
           errortrace-annotate

           print-error-trace
           error-context-display-depth

           instrumenting-enabled

           profiling-enabled
           profiling-record-enabled
           profile-paths-enabled
           get-profile-results
           output-profile-results
           clear-profile-results

           execute-counts-enabled
           get-execute-counts
           annotate-executed-file

           ;; use names that are consistent with the above
           (rename test-coverage-enabled coverage-counts-enabled)
           get-coverage-counts
           annotate-covered-file

           annotate-top))
