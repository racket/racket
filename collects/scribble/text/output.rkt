#lang racket/base

(require racket/promise)

(provide output)

;; Outputs values for the `scribble/text' language:
;; - several atomic values are printed as in `display',
;; - promises, thunks, and boxes are indirections for the value they contain
;;   (useful in various cases),
;; - some "special" values are used for controlling output (eg, flushing,
;;   prefix changes, etc),
;; - specifically, `block's delimit indentation levels, `splice's do not,
;; - lists (more generally, pairs) are like either one depending on the context
;;   (same as blocks/splices when inside a `block'/`splice'), at the toplevel
;;   they default to blocks.
;;
;; Uses global state because `output' is wrapped around each expression in a
;; scribble/text file so this is much more convenient than wrapping the whole
;; module's body in a `list' (which will be difficult with definitions etc).
;; The state is a pair of prefixes -- one that is the prefix for the current
;; value (which gets extended with nested blocks), and the other is the prefix
;; for the current "line" (which is reset after a newline).  The line-prefix is
;; needed because a line can hold a block, which means that the line-prefix
;; will apply for the contents of the block including newlines in it.  This
;; state is associated with a port via a hash table.  Another state that is
;; used is the port's column position, which is maintained by the system (when
;; line counts are enabled) -- this is used to tell what part of a prefix is
;; already displayed.
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
  ;; the current mode for lists
  (define list=block? #t)
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
                 (define nl (car nls))
                 (if (regexp-match? #rx"^ *$" x start (car nl))
                   (newline p) ; only spaces before the end of the line
                   (begin (output-pfx col pfx lpfx)
                          (write x p start (cdr nl))))
                 (loop (cdr nl) (cdr nls) 0 0)]
                ;; last substring from here (always set lpfx state when done)
                [(start . = . len)
                 (set-mcdr! pfxs lpfx)]
                [(col . > . (2pfx-length pfx lpfx))
                 (set-mcdr! pfxs lpfx)
                 ;; the prefix was already shown, no accumulation needed
                 (write x p start)]
                [else
                 (define m (regexp-match-positions #rx"^ +" x start))
                 ;; accumulate spaces to lpfx, display if it's not all spaces
                 (define lpfx* (if m (pfx+ lpfx (- (cdar m) (caar m))) lpfx))
                 (set-mcdr! pfxs lpfx*)
                 (unless (and m (= len (cdar m)))
                   (output-pfx col pfx lpfx*)
                   ;; the spaces were already added to lpfx
                   (write x p (if m (cdar m) start)))])))))
  ;; blocks and splices
  (define (output-block c)
    (define pfx  (mcar pfxs))
    (define lpfx (mcdr pfxs))
    (define npfx (pfx+col (pfx+ pfx lpfx)))
    (set-mcar! pfxs npfx) (set-mcdr! pfxs 0)
    (if (list? c)
      (for ([c (in-list c)]) (loop c))
      (begin (loop (car c)) (loop (cdr c))))
    (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx))
  (define (output-splice c)
    (for-each loop c))
  ;; main loop
  (define (loop x)
    (cond
      ;; no output for these
      [(or (void? x) (not x) (null? x)) (void)]
      ;; for lists and pairs the current line prefix is added to the global
      ;; one, then output the contents recursively (no need to change the
      ;; state, since we pass the values in the loop, and we'd need to restore
      ;; it afterwards anyway)
      [(pair? x) (if list=block? (output-block x) (output-splice x))]
      ;; delayed values
      [(and (procedure? x) (procedure-arity-includes? x 0)) (loop (x))]
      [(promise? x) (loop (force x))]
      [(box?     x) (loop (unbox x))]
      ;; special output wrappers
      [(special? x)
       (define c (special-contents x))
       (case (special-flag x)
         ;; preserve tailness & avoid `set!' for blocks/splices if possible
         [(block) (if list=block?
                    (output-block c)
                    (begin (set! list=block? #t)
                           (output-block c)
                           (set! list=block? #f)))]
         [(splice) (if list=block?
                     (begin (set! list=block? #f)
                            (output-splice c)
                            (set! list=block? #t))
                     (output-splice c))]
         [(flush) ; useful before `disable-prefix'
          (output-pfx (getcol) (mcar pfxs) (mcdr pfxs))]
         [(disable-prefix) ; save the previous pfxs
          (define pfx  (mcar pfxs))
          (define lpfx (mcdr pfxs))
          (set-mcar! pfxs #f)  (set-mcdr! pfxs (cons pfx lpfx))
          (for-each loop c)
          (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx)]
         [(restore-prefix) ; restore the previous pfxs
          (define pfx  (mcar pfxs))
          (define lpfx (mcdr pfxs))
          (define npfx (pfx+col (if (and (not pfx) (pair? lpfx))
                                  (pfx+ (car lpfx) (cdr lpfx))
                                  (pfx+ pfx lpfx))))
          (set-mcar! pfxs npfx)  (set-mcdr! pfxs 0)
          (for-each loop c)
          (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx)]
         [(add-prefix) ; add to the current prefix (unless it's #f)
          (define pfx  (mcar pfxs))
          (define lpfx (mcdr pfxs))
          (define npfx (pfx+ (pfx+col (pfx+ pfx lpfx)) (car c)))
          (set-mcar! pfxs npfx) (set-mcdr! pfxs 0)
          (for-each loop (cdr c))
          (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx)]
         [(set-prefix)
          (define pfx  (mcar pfxs))
          (define lpfx (mcdr pfxs))
          (set-mcar! pfxs (car c)) (set-mcdr! pfxs 0)
          (for-each loop (cdr c))
          (set-mcar! pfxs pfx) (set-mcdr! pfxs lpfx)]
         [(with-writer)
          (define old write)
          (set! write (or (car c) write-string))
          (for-each loop (cdr c))
          (set! write old)]
         #; ; no need for this hack yet
         [(with-writer-change)
          ;; The function gets the old writer and return a new one (useful to
          ;; save the current writer and restore it inside).  Could also be
          ;; used to extend a writer, but that shows why a customizable writer
          ;; is a bad choice: instead, it should be a list of substitutions
          ;; that can be extended more conveniently.  A simple implementation
          ;; would be to chain functions that do substitutions.  But that runs
          ;; into problems when functions want to substitute the same thing,
          ;; and worse: when the output of one function would get substituted
          ;; again by another.  Another approach would be to join matcher
          ;; regexps with "|" after wrapping each one with parens, then find
          ;; out which one matched by looking at the result and applying its
          ;; substitution, but the problem with that is that is that it forbids
          ;; having parens in the regexps -- this could be fixed by not
          ;; parenthesizing each expression, and instead running the found
          ;; match against each of the input regexps to find the matching one,
          ;; but that can be very inefficient.  Yet another issue is that in
          ;; some cases we might *want* the "worse" feature mentioned earlier:
          ;; for example, when we want to do some massaging of the input texts
          ;; yet still have the result encoded for HTML output -- so perhaps
          ;; the simple approach is still better.  The only difference from the
          ;; current `with-writer' is using a substituting function, so it can
          ;; be composed with the current one instead of replacing it
          ;; completely.
          (define old write)
          (set! write ((car c) write))
          (for-each loop (cdr c))
          (set! write old)]
         [else (error 'output "unknown special value flag: ~e"
                      (special-flag x))])]
      [else
       (output-string
        (cond [(string?  x) x]
              [(bytes?   x) (bytes->string/utf-8 x)]
              [(symbol?  x) (symbol->string      x)]
              [(path?    x) (path->string        x)]
              [(keyword? x) (keyword->string     x)]
              [(number?  x) (number->string      x)]
              [(char?    x) (string              x)]
              ;; generic fallback: throw an error (could use `display' so new
              ;; values can define how they're shown, but the same
              ;; functionality can be achieved with thunks and prop:procedure)
              [else (error 'output "don't know how to render value: ~v" x)]))]))
  ;;
  (port-count-lines! p)
  (loop x)
  (void))

(define port->state
  (let ([t (make-weak-hasheq)]
        [last '(#f #f)]) ; cache for the last port, to avoid a hash lookup
    (λ (p)
      (if (eq? p (car last)) (cdr last)
          (let ([s (or (hash-ref t p #f)
                       (let ([s (mcons 0 0)]) (hash-set! t p s) s))])
            (set! last (cons p s))
            s)))))

;; special constructs

(define-struct special (flag contents))

(define-syntax define/provide-special
  (syntax-rules ()
    [(_ (name))
     (begin (provide name)
            (define (name . contents)
              (make-special 'name contents)))]
    [(_ (name x ...))
     (begin (provide name)
            (define (name x ... . contents)
              (make-special 'name (list* x ... contents))))]
    [(_ name)
     (begin (provide name)
            (define name (make-special 'name #f)))]))

(define/provide-special (block))
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
  (let ([t (make-hasheq)] [v (make-vector 200 #f)])
    (λ (n)
      (or (if (< n 200) (vector-ref v n) (hash-ref t n #f))
          (let ([spaces (make-string n #\space)])
            (if (< n 200) (vector-set! v n spaces) (hash-set! t n spaces))
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
