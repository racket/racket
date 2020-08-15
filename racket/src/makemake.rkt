#lang racket/base
(require racket/cmdline)

(define src
  (command-line
   #:args (src) src))

(printf "# Generated from ~s\n" src)

(define statics '(MAKE))

(define vars (make-hasheq))
(define target-uses (make-hasheq))

;; each call is `(cons <target> <vars>)` for
;; variables explicitly propagated
(define target-calls (make-hasheq))

(define (hash-sorted-keys ht)
  (hash-map ht (lambda (k v) k) #t))

(define (read-make-line in)
  (define l (read-line in))
  (cond
    [(eof-object? l) l]
    [(regexp-match? #rx"\\\\$" l)
     (string-append (substring l 0 (sub1 (string-length l)))
                    (read-make-line in))]
    [else l]))

(define (match->symbol m)
  (define s (cadr m))
  (string->symbol (if (string? s)
                      s
                      (bytes->string/utf-8 s))))


(define target-macros '())

(define (match->called-targets m)
  (define s (let ([s (cadr m)])
              (if (string? s)
                  s
                  (bytes->string/utf-8 s))))
  (let loop ([s s])
    (let t-loop ([tms target-macros])
      (cond
        [(null? tms) (list (string->symbol s))]
        [(regexp-match? (caar tms) s)
         (apply append
                (for/list ([alt (cdar tms)])
                  (loop (regexp-replace (caar tms) s alt))))]
        [else (t-loop (cdr tms))]))))

(define macros '())

(define (expand-macros s)
  (if (eof-object? s)
      s
      (for/fold ([s s]) ([rx+val (in-list macros)])
        (regexp-replace* (car rx+val) s (cdr rx+val)))))

(define rx:macro #rx"^([A-Za-z_0-9]+) *== *(.*)$")
(define rx:static #rx"^([A-Za-z_0-9]+) *:= *(.*)$")
(define rx:var #rx"^([A-Za-z_0-9]+) *=")
(define rx:target #rx"^([-A-Za-z_0-9/\\.]+) *:")

(define rx:cd-make #rx"^[ \t]*cd +[-a-zA-Z0-9_/\\.]+ +[&][&] +[$][(]MAKE[)]")
(define rx:make #rx"[$][(]MAKE[)] +(?:-j +[^ ]+ +)?([-a-zA-Z0-9/_$().]+)")

(define rx:assign #rx"^ +([-_A-Za-z0-9]+)=(\"[^\"]*\"|'[^\']*'|[^ ]*)")

(define (parse in)
  (define (declare-variable! m)
    (define var (match->symbol m))
    (when (hash-ref vars var #f)
      (printf "variable redefined: ~a\n" var))
    (hash-set! vars var #t)
    var)

  (let loop ([target #f])
    (define l (expand-macros (read-make-line in)))
    (cond
      [(eof-object? l) (void)]
      [(regexp-match #rx"^# Target selector: (.*)$" l)
       => (lambda (sel-m)
            (define next-l (read-make-line in))
            (cond
              [(regexp-match rx:var next-l)
               => (lambda (m)
                    (define var (declare-variable! m))
                    (define alts
                      (let ([i (open-input-string (cadr sel-m))])
                        (let loop ()
                          (cond
                            [(regexp-match #rx"`([^`]*)`" i)
                             => (lambda (m)
                                  (cons (bytes->string/utf-8 (cadr m)) (loop)))]
                            [else '()]))))
                    (printf "#  Target values for ~a: ~s\n" var alts)
                    (set! target-macros (cons (cons (regexp (format "[$][(]~a[)]" var))
                                                    alts)
                                              target-macros))
                    (loop #f))]
              [else (error "did not find variable for target selector")]))]
      [(or (regexp-match? #rx"^#.*" l)
           (regexp-match? #rx"^ *$" l))
       (loop target)]
      [(regexp-match rx:macro l)
       => (lambda (m)
            (define macro (match->symbol m))
            (when (assq macro macros)
              (printf "macro redefined: ~a\n" macro))
            (set! macros (cons (cons (regexp (format "[$][(]~a[)]" macro)) (regexp-replace-quote (caddr m)))
                               macros))
            (loop #f))]
      [(regexp-match rx:static l)
       => (lambda (m)
            (set! statics (cons (match->symbol m) statics))
            (loop #f))]
      [(regexp-match rx:var l)
       => (lambda (m)
            (declare-variable! m)
            (loop #f))]
      [(regexp-match rx:target l)
       => (lambda (m)
            (define target (match->symbol m))
            (when (hash-ref target-uses target #f)
              (printf "target redefined: ~a\n" target))
            (hash-set! target-uses target '())
            (hash-set! target-calls target '())
            (loop target))]
      [(regexp-match? #rx"^\t" l)
       (let ([i (open-input-string l)])
         (let loop ()
           (cond
             [(regexp-try-match rx:cd-make i)
              ;; ignore `make` target in a different directory
              (loop)]
             [else
              (define m (regexp-match rx:make i))
              (when m
                (for ([called-target (match->called-targets m)])
                  (define vars (let loop ()
                                 (cond
                                   [(regexp-try-match rx:assign i)
                                    => (lambda (m)
                                         (cons (match->symbol m) (loop)))]
                                   [else '()])))
                  (hash-set! target-calls target
                             (cons (cons called-target vars)
                                   (hash-ref target-calls target '()))))
                (loop))])))
       (let ([i (open-input-string l)])
         (let loop ()
           (define m (regexp-match #rx"[$][(]([-_A-Za-z0-9]+)[)]" i))
           (when m
             (define var (match->symbol m))
             (define vars (hash-ref target-uses target '()))
             (unless (or (memq var vars)
                         (memq var statics))
               (hash-set! target-uses target (cons var vars)))
             (loop))))
       (loop target)]
      [else
       (printf "## unparsed: ~s\n" l)
       (loop target)])))

(define (less-space s)
  (regexp-replace* #rx"\t *|  +" s " "))

(define (variables in)
  (let loop ()
    (define l (expand-macros (read-make-line in)))
    (cond
      [(eof-object? l) (void)]
      [(regexp-match rx:macro l)
       => (lambda (m)
            (printf "~a = ~a\n" (cadr m) (less-space (caddr m)))
            (loop))]
      [(regexp-match? rx:static l)
       (displayln (less-space (regexp-replace #rx":=" l "=")))
       (loop)]
      [(regexp-match? rx:var l)
       (displayln (less-space l))
       (loop)]
      [(regexp-match? rx:target l)
       (loop)]
      [(regexp-match? #rx"^\t" l)
       (loop)]
      [else
       (loop)])))

(define (convert in)
  (let loop ([target #f])
    (define l (expand-macros (read-make-line in)))
    (cond
      [(eof-object? l) (void)]
      [(or (regexp-match? #rx"^#.*" l)
           (regexp-match? #rx"^ *$" l))
       (loop target)]
      [(regexp-match? rx:macro l)
       (loop #f)]
      [(regexp-match? rx:static l)
       (loop #f)]
      [(regexp-match? rx:var l)
       (loop #f)]
      [(regexp-match rx:target l)
       => (lambda (m)
            (define target (match->symbol m))
            (displayln l)
            (loop target))]
      [(regexp-match? #rx"^\t" l)
       (let ([i (open-input-string l)])
         (let loop ()
           (cond
             [(regexp-try-match rx:cd-make i 0 #f (current-output-port))
              => (lambda (m)
                   ;; ignore `make` target in a different directory
                   (display (car m))
                   (loop))]
             [else
              (define m (regexp-match rx:make i 0 #f (current-output-port)))
              (when m
                (display (car m))
                (define called-targets (match->called-targets m))
                (define vars (let loop ()
                               (cond
                                 [(regexp-try-match rx:assign i 0 #f (current-output-port))
                                  => (lambda (m)
                                       (printf " ~a=~a" (cadr m) (caddr m))
                                       (cons (match->symbol m) (loop)))]
                                 [else '()])))
                (define want-vars (remove* vars
                                           (hash-sorted-keys
                                            (for*/hasheq ([called-target (in-list called-targets)]
                                                          [var (in-list (hash-ref target-uses called-target '()))])
                                              (values var #t)))))
                (for ([want-var (in-list want-vars)])
                  (printf " ~a=\"$(~a)\"" want-var want-var))
                (loop))])))
       (newline)
       (loop target)]
      [else
       (loop target)])))

(call-with-input-file* src parse)

(let loop ([complain? #t])
  (define orig (hash-copy target-uses))
  (for ([(target calls) (in-hash target-calls)])
    (for ([call (in-list calls)])
      (define called-target (car call))
      (define vars (cdr call))
      (when complain?
        (unless (hash-ref target-uses called-target #f)
          (printf "## ~s -> ~s missing\n" target called-target)))
      (define want-vars (hash-ref target-uses called-target '()))
      (for ([want-var (in-list want-vars)])
        (unless (memq want-var statics)
          (unless (memq want-var vars)
            (define need-vars (hash-ref target-uses target '()))
            (unless (memq want-var need-vars)
              (hash-set! target-uses target (cons want-var need-vars))))))))
  (unless (equal? orig target-uses)
    (loop #f)))

(call-with-input-file* src variables)
(call-with-input-file* src convert)
