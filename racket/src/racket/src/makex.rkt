#lang racket/base
(require racket/cmdline)

(define list-mode (make-parameter #f))
(define assign-mode (make-parameter #f))
(define exports-mode (make-parameter #f))
(define winex-mode (make-parameter #f))
(define gwinex-mode (make-parameter #f))
(define gcalert-mode (make-parameter #f))
(define precisegc-mode (make-parameter #f))

(command-line
 #:once-each
 [("--list") "list mode" (list-mode #t)]
 [("--assign") "assign mode" (assign-mode #t)]
 [("--exports") "export mode" (exports-mode #t)]
 [("--winex") "windows export mode" (winex-mode #t)]
 [("--gwinex") "windows GC exports mode " (gwinex-mode #t)]
 [("--precisegc") "precise GC mode" (precisegc-mode #t)])

(when (list-mode)
  (gcalert-mode #t))

(when (or (assign-mode)
          (exports-mode)
          (winex-mode)
          (gwinex-mode))
  (list-mode #t))

(define (balance s)
  (for/fold ([parens 0]) ([c (in-string s)])
    (case c
      [(#\() (+ parens 1)]
      [(#\)) (- parens 1)]
      [else parens])))

(define exported
  (let loop ()
    (let ([l (read-line)])
      (cond
       [(equal? l "/* SKIP */")
        (let loop ()
          (let ([l (read-line)])
            (unless (equal? l "/* START */")
              (loop))))
        (unless (list-mode)
          (printf "typedef struct {\n"))
        (let loop ([exported null])
          (let ([l (read-line)])
            (cond
             [(eof-object? l) (reverse exported)]
             [(regexp-match #rx"^/[*]" l)
              (unless (list-mode) (displayln l))
              (loop exported)]
             [(regexp-match #rx"^#" l)
              (unless (list-mode) (displayln l))
              (loop (cons l exported))]
             [(regexp-match? #rx"^ *$" l)
              (loop exported)]
             [else
              (let* ([l (regexp-replace #rx"^extern " l "")]
                     [l (regexp-replace #rx"^XFORM_NONGCING " l "")]
                     [l (regexp-replace #rx"^XFORM_NONGCING_NONALIASING " l "")]
                     [l (regexp-replace #rx"^MZ_EXTERN " l "")]
                     [l (regexp-replace #rx"^THREAD_LOCAL " l "")]
                     [l2 (regexp-replace #rx"^volatile " l "")]
                     [volatile (if (equal? l l2) "" "volatile ")]
                     [l l2]
                     [l2 (regexp-replace #rx"^const " l "")]
                     [const (if (equal? l l2) "" "const ")]
                     [l l2]
                     [l2 (regexp-replace #rx"^struct " l "")]
                     [const (if (equal? l l2) const "struct ")]
                     [l l2]
                     [l2 (regexp-replace #rx"^unsigned " l "")]
                     [unsigned (if (equal? l l2) "" "unsigned ")]
                     [l l2]
                     [star (if (regexp-match? #rx"\\[1?\\];$" l)
                               "*"
                               "")]
                     [m (regexp-match "([a-zA-Z0-9_]*) ([*]*)([a-zA-Z0-9_]*)(.*)" l)])
                (unless (list-mode)
                  (if (equal? "(" (substring (list-ref m 4) 0 1))
                      (apply printf "~a~a~a ~a(*~a)~a\n"
                             const unsigned (cdr m))
                      (printf "~a~a~a~a ~a~a~a;\n"
                              const volatile unsigned (cadr m) star (caddr m) (cadddr m))))
                (when (positive? (balance (list-ref m 4)))
                  (let loop ()
                    (let ([l (read-line)])
                      (unless (list-mode)
                        (displayln l))
                      (unless (regexp-match #rx"[)];" l)
                        (loop)))))
                (loop (cons (list-ref m 3) exported)))])))]
       [else
        (unless (list-mode)
          (displayln l))
        (loop)]))))

(unless (list-mode)
  (printf "#ifndef SCHEME_EX_INLINE\n} Scheme_Extension_Table;\n#endif\n"))

(when (exports-mode)
  (printf "#!..\n"))
(when (or (winex-mode)
          (gwinex-mode))
  (printf "EXPORTS\n"))

(when (list-mode)
  (for/fold ([suspend? #f] [else-suspend? #f]) ([l (in-list exported)])
    (cond
     [(equal? "#" (substring l 0 1))
      (if (or (exports-mode)
              (winex-mode)
              (gwinex-mode))
          (cond
           [(regexp-match? #rx"#ifdef MZ_REAL_THREADS" l)
            (values #t #f)]
           [(regexp-match? #rx"#ifndef MZ_REAL_THREADS" l)
            (values #f #t)]
           [(regexp-match? #rx"#ifdef MACINTOSH_EVENTS" l)
            (values #t #f)]
           [(regexp-match? #rx"#ifdef USE_MAC_FILE_TOOLBOX" l)
            (values #t #f)]
           [(regexp-match? #rx"#ifdef USE_MAC_CARBON_FILE_TOOLBOX" l)
            (values #t #f)]
           [(regexp-match? #rx"#ifdef MZ_USE_SINGLE_FLOATS" l)
            (values #t #f)]
           [(regexp-match? #rx"#ifdef MZ_LONG_DOUBLE" l)
            (values #t #f)]
           [(and (exports-mode)
                 (regexp-match? #rx"#ifdef USE_MZ_SETJMP" l))
            (values #t #f)]
           [(regexp-match? #px"#\\s*ifdef MZ_PRECISE_GC" l)
            (if (precisegc-mode)
                (values #f #t)
                (values #t #f))]
           [(regexp-match? #rx"#ifndef LINK_EXTENSIONS_BY_TABLE" l)
            (values #f #t)]
           [(regexp-match? #px"#\\s*else" l)
            (values else-suspend? #f)]
           [(regexp-match? #px"#\\s*endif" l)
            (values #f #f)]
           [else
            (values suspend? else-suspend?)])
          (begin
            (displayln l)
            (values suspend? else-suspend?)))]
     [else
      (cond
       [(assign-mode)
        (printf "  scheme_extension_table->~a = ~a;\n" l l)]
       [(exports-mode)
        (unless suspend? (printf "~a\n" l))]
       [(or (winex-mode) (gwinex-mode))
        (unless suspend?
          (when (or
                 (and (winex-mode) (regexp-match #rx"^scheme_" l))
                 (and (gwinex-mode) (regexp-match #rx"^GC_" l)))
            (printf " ~a" l)
            (when (ormap (lambda (rx)
                           (regexp-match? rx l))
                         '(#rx"scheme_current_thread"
                              #rx"scheme_fuel_counter"
                              #rx"scheme_eof"
                              #rx"scheme_null"
                              #rx"scheme_true"
                              #rx"scheme_false"
                              #rx"scheme_void"
                              #rx"scheme_undefined"
                              #rx"scheme_null"))
              (printf " DATA"))
            (printf "\n")))]
       [else
        (printf "#define ~a (scheme_extension_table->~a)\n" l l)])
      (values suspend? else-suspend?)]))
  (void))

(when (gcalert-mode)
  (printf "#ifdef MZ_PRECISE_GC\n");
  (printf "#pragma GC_VARIABLE_STACK_THOUGH_TABLE\n")
  (printf "#endif\n"))
