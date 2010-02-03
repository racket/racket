#lang scheme/base

(require scheme/promise)

(provide output)

;; Outputs some value, for the preprocessor language.
;;
;; Uses global state because `output' is wrapped around each expression in a
;; scribble/text file so this is much more convenient than wrapping the whole
;; module's body in a `list' (which will be difficult with definitions etc).
;; The state is a pair of prefixes -- one that is the prefix for the current
;; value (which gets accumulated to with nested lists), and the other is the
;; prefix for the current "line" (which is reset after a newline).  The
;; line-prefix is needed because a line can hold a list, which means that the
;; line-prefix will apply for the contents of the list including newlines in
;; it.  This state is associated to a port via a hash table.  Another state
;; that is used is the port's column position, which is maintained by the
;; system (when line counts are enabled) -- this is used to tell what part of a
;; prefix is already displayed.
;;
;; Each prefix is either an integer (for a number of spaces) or a string.  The
;; prefix mechanism can be disabled by using #f for the global prefix, and in
;; this case the line prefix can have (cons pfx lpfx) so it can be restored --
;; used by `disable-prefix' and `restore-prefix' resp.  (This is different from
;; a 0 prefix -- #f means that no prefix will be accumulated).
;;
(define (output x [p (current-output-port)])
  ;; these are the global prefix and the one that is local to the current line
  (define pfxs (port->state p))
  ;; the low-level string output function (can change with `with-writer')
  (define write write-string)
  ;; to get the output column
  (define (getcol) (let-values ([(line col pos) (port-next-location p)]) col))
  ;; total size of the two prefixes
  (define (2pfx-length pfx1 pfx2)
    (if (and pfx1 pfx2)
      (+ (if (number? pfx1) pfx1 (string-length pfx1))
         (if (number? pfx2) pfx2 (string-length pfx2)))
      0))
  ;; combines a prefix with a target column to get to
  (define (pfx+col pfx)
    (and pfx (let ([col (getcol)])
               (cond [(number? pfx) (max pfx col)]
                     [(>= (string-length pfx) col) pfx]
                     [else (string-append
                            pfx (make-spaces (- col (string-length pfx))))]))))
  ;; adds two prefixes
  (define (pfx+ pfx1 pfx2)
    (and pfx1 pfx2
         (if (and (number? pfx1) (number? pfx2)) (+ pfx1 pfx2)
             (string-append (if (number? pfx1) (make-spaces pfx1) pfx1)
                            (if (number? pfx2) (make-spaces pfx2) pfx2)))))
  ;; prints two prefixes
  (define (output-pfx col pfx1 pfx2)
    (define-syntax-rule (->str pfx) (if (number? pfx) (make-spaces pfx) pfx))
    (define-syntax-rule (show pfx) ; optimize when not needed
      (unless (eq? pfx 0) (write (->str pfx) p)))
    (when (and pfx1 pfx2)
      (if (eq? 0 col)
        (begin (show pfx1) (show pfx2))
        (let ([len1 (if (number? pfx1) pfx1 (string-length pfx1))])
          (cond [(< col len1) (write (->str pfx1) p col) (show pfx2)]
                [(= col len1) (show pfx2)]
                [(eq? 0 pfx2)]
                [else
                 (let ([col (- col len1)]
                       [len2 (if (number? pfx2) pfx2 (string-length pfx2))])
                   (when (< col len2) (write (->str pfx2) p col)))])))))
  ;; the basic printing unit: strings
  (define (output-string x)
    (define pfx (mcar pfxs))
    (if (not pfx) ; prefix disabled?
      (write x p)
      (let ([len (string-length x)]
            [nls (regexp-match-positions* #rx"\n" x)])
        (let loop ([start 0] [nls nls] [lpfx (mcdr pfxs)] [col (getcol)])
          (cond [(pair? nls)
                 (let ([nl (car nls)])
                   (if (regexp-match? #rx"^ *$" x start (car nl))
                     (newline p) ; only spaces before the end of the line
                     (begin (output-pfx col pfx lpfx)
                            (write x p start (cdr nl))))
                   (loop (cdr nl) (cdr nls) 0 0))]
                ;; last substring from here (always set lpfx state when done)
                [(start . = . len)
                 (set-mcdr! pfxs lpfx)]
                [(col . > . (2pfx-length pfx lpfx))
                 (set-mcdr! pfxs lpfx)
                 ;; the prefix was already shown, no accumulation needed
                 (write x p start)]
                [else
                 (let ([m (regexp-match-positions #rx"^ +" x start)])
                   ;; accumulate spaces to lpfx, display if it's not all spaces
                   (let ([lpfx (if m (pfx+ lpfx (- (cdar m) (caar m))) lpfx)])
                     (set-mcdr! pfxs lpfx)
                     (unless (and m (= len (cdar m)))
                       (output-pfx col pfx lpfx)
                       ;; the spaces were already added to lpfx
                       (write x p (if m (cdar m) start)))))])))))
  ;; main loop
  (define (loop x)
    (cond
      ;; no output for these
      [(or (void? x) (not x) (null? x)) (void)]
      ;; for lists and pairs the current line prefix is added to the global
      ;; one, then output the contents recursively (no need to change the
      ;; state, since we pass the values in the loop, and we'd need to restore
      ;; it afterwards anyway)
      [(pair? x) (if (list? x)
                   (let* ([pfx (mcar pfxs)] [lpfx (mcdr pfxs)]
                          [npfx (pfx+col (pfx+ pfx lpfx))])
                     (set-mcar! pfxs npfx) (set-mcdr! pfxs 0)
                     (for ([x (in-list x)]) (loop x))
                     (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx))
                   (begin (loop (car x)) (loop (cdr x))))]
      ;; delayed values
      [(and (procedure? x) (procedure-arity-includes? x 0)) (loop (x))]
      [(promise? x) (loop (force x))]
      ;; special output wrappers
      [(special? x)
       (let ([c (special-contents x)])
         (case (special-flag x)
           [(splice) (for-each loop c)]
           [(flush) ; useful before `disable-prefix'
            (output-pfx (getcol) (mcar pfxs) (mcdr pfxs))]
           [(disable-prefix) ; save the previous pfxs
            (let ([pfx (mcar pfxs)] [lpfx (mcdr pfxs)])
              (set-mcar! pfxs #f)  (set-mcdr! pfxs (cons pfx lpfx))
              (for-each loop c)
              (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx))]
           [(restore-prefix) ; restore the previous pfxs
            (let* ([pfx (mcar pfxs)] [lpfx (mcdr pfxs)]
                   [npfx (pfx+col (if (and (not pfx) (pair? lpfx))
                                    (pfx+ (car lpfx) (cdr lpfx))
                                    (pfx+ pfx lpfx)))])
              (set-mcar! pfxs npfx)  (set-mcdr! pfxs 0)
              (for-each loop c)
              (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx))]
           [(add-prefix) ; add to the current prefix (unless it's #f)
            (let* ([pfx (mcar pfxs)] [lpfx (mcdr pfxs)]
                   [npfx (pfx+ (pfx+col (pfx+ pfx lpfx)) (car c))])
              (set-mcar! pfxs npfx) (set-mcdr! pfxs 0)
              (for-each loop (cdr c))
              (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx))]
           [(set-prefix)
            (let ([pfx (mcar pfxs)] [lpfx (mcdr pfxs)])
              (set-mcar! pfxs (car c)) (set-mcdr! pfxs 0)
              (for-each loop (cdr c))
              (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx))]
           [(with-writer)
            (let ([old write])
              (set! write (or (car c) write-string))
              (for-each loop (cdr c))
              (set! write old))]
           #; ; no need for this hack yet
           [(with-writer-change)
            ;; the function gets the old writer and return a new one
            ;; (useful to sabe the current writer then restore it inside)
            (let ([old write])
              (set! write ((car c) write))
              (for-each loop (cdr c))
              (set! write old))]
           [else (error 'output "unknown special value flag: ~e"
                        (special-flag x))]))]
      [else
       (output-string
        (cond [(string? x)  x]
              [(bytes? x)   (bytes->string/utf-8 x)]
              [(symbol? x)  (symbol->string      x)]
              [(path? x)    (path->string        x)]
              [(keyword? x) (keyword->string     x)]
              [(number? x)  (number->string      x)]
              [(char? x)    (string              x)]
              ;; generic fallback: throw an error
              [else (error 'output "don't know how to render value: ~v" x)]))]))
  ;;
  (port-count-lines! p)
  (loop x)
  (void))

(define port->state
  (let ([t (make-weak-hasheq)]
        [last '(#f #f)]) ; cache for the last port, to avoid a hash lookup
    (lambda (p)
      (if (eq? p (car last)) (cdr last)
          (let ([s (or (hash-ref t p #f)
                       (let ([s (mcons 0 0)]) (hash-set! t p s) s))])
            (set! last (cons p s))
            s)))))

;; special constructs

(define-struct special (flag contents))

(define-syntax define/provide-special
  (syntax-rules ()
    [(_ (name x ...))
     (begin (provide name)
            (define (name x ... . contents)
              (make-special 'name (list* x ... contents))))]
    [(_ name)
     (begin (provide name)
            (define name (make-special 'name #f)))]))

(define/provide-special (splice))
(define/provide-special flush)
(define/provide-special (disable-prefix))
(define/provide-special (restore-prefix))
(define/provide-special (add-prefix pfx))
(define/provide-special (set-prefix pfx))
(define/provide-special (with-writer writer))
#; ; no need for this hack yet
(define/provide-special (with-writer-change writer))

(define make-spaces ; (efficiently)
  (let ([t (make-hasheq)] [v (make-vector 80 #f)])
    (lambda (n)
      (or (if (< n 80) (vector-ref v n) (hash-ref t n #f))
          (let ([spaces (make-string n #\space)])
            (if (< n 80) (vector-set! v n spaces) (hash-set! t n spaces))
            spaces)))))

;; Convenient utilities

(provide add-newlines)
(define (add-newlines list #:sep [sep "\n"])
  (define r
    (let loop ([list list])
      (if (null? list)
        null
        (let ([1st (car list)])
          (if (or (not 1st) (void? 1st))
            (loop (cdr list))
            (list* sep 1st (loop (cdr list))))))))
  (if (null? r) r (cdr r)))

(provide split-lines)
(define (split-lines list)
  (let loop ([list list] [cur '()] [r '()])
    (cond
      [(null? list) (reverse (cons (reverse cur) r))]
      [(equal? "\n" (car list)) (loop (cdr list) '() (cons (reverse cur) r))]
      [else (loop (cdr list) (cons (car list) cur) r)])))
