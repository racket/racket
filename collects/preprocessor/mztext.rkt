#lang scheme/base

(require preprocessor/pp-utils scheme/port scheme/promise
         (only-in mzlib/string read-from-string-all))
(provide (all-from-out preprocessor/pp-utils))

;;=============================================================================
;; Composite port
;; A composite port is an input port and a procedure that can be used to
;; prepend stuff to this port.  The implementation uses a custom input port
;; that implements its own peeking, because peeking is not always consistent
;; (in case stuff was prepended, previous peeks are invalidated), so we cannot
;; rely on the internal default peek.  The port holds a list of input ports and
;; strings, which are being used as necessary when input is required.  This
;; list can also hold thunk values -- these thunks will be executed when
;; reading input goes past them (when peeking goes past them, nothing happens).

(provide make-composite-input)
(define (make-composite-input . ports)
  ;; don't care about concurrency, since multiple uses should use different
  ;; input ports.
  (define (pop-port!)
    (begin0 (car ports) (set! ports (cdr ports))))
  (define (add! x)
    (cond [(pair? x) (add! (cdr x)) (add! (car x))]
          [(null? x) #t]
          [(void? x) #t]
          [(not x) #t]
          [(or (input-port? x)
               (and (procedure? x) (procedure-arity-includes? x 0)))
           (set! ports (cons x ports))]
          [(bytes?  x) (add! (open-input-bytes x))]
          [(string? x) (add! (open-input-string x))]
          [(path?   x) (add! (path->bytes x))]
          [(symbol? x) (add! (symbol->string x))]
          [(number? x) (add! (number->string x))]
          [(char?   x) (add! (string x))]
          [else (error 'composite-input "bad object: ~e" x)]))
  ;; Large parts taken from `input-port-append'.
  (define (read bstr)
    (let loop ()
      (cond [(null? ports) eof]
            [(procedure? (car ports)) ; reading past a thunk: execute it
             (add! ((pop-port!)))
             (loop)]
            [else
             (let ([n (read-bytes-avail!* bstr (car ports))])
               (cond [(eq? n 0) (wrap-evt (car ports) (lambda (x) 0))]
                     [(eof-object? n) (close-input-port (pop-port!)) (loop)]
                     [else n]))])))
  (define (peek bstr skip evt)
    ;; Peeking is more difficult, due to skips.
    (let loop ([ports ports] [skip skip])
      (cond
       [(null? ports) eof]
       [(procedure? (car ports)) (loop (cdr ports) skip)]
       [else (let ([n (peek-bytes-avail!* bstr skip evt (car ports))])
               (cond [(eq? n 0)
                      ;; Not ready, yet.
                      (peek-bytes-avail!-evt bstr skip evt (car ports))]
                     [(eof-object? n)
                      ;; Port is exhausted, or we skipped past its input.
                      ;; If skip is not zero, we need to figure out
                      ;;  how many chars were skipped.
                      (loop (cdr ports)
                            (- skip (compute-avail-to-skip skip (car ports))))]
                     [else n]))])))
  (define (close)
    (for-each (lambda (p) (when (input-port? p) (close-input-port p))) ports))
  (let ([ps ports]) (set! ports '()) (add! ps))
  (let ([p (make-input-port 'composite-input read peek close)])
    (port->adder-op p 'set! add!)
    p))
;; Helper for input-port-append; given a skip count
;;  and an input port, determine how many characters
;;  (up to upto) are left in the port. We figure this
;;  out using binary search.
(define (compute-avail-to-skip upto p)
  (let ([str (make-bytes 1)])
    (let loop ([upto upto][skip 0])
      (if (zero? upto)
        skip
        (let* ([half (quotient upto 2)]
               [n (peek-bytes-avail!* str (+ skip half) #f p)])
          (if (eq? n 1)
            (loop (- upto half 1) (+ skip half 1))
            (loop half skip)))))))

(provide add-to-input)
(define (add-to-input . args)
  (port->adder-op (stdin) 'add args))

(define port->adder-op
  (let ([table (make-weak-hasheq)])
    (lambda (port msg . args)
      (case msg
        [(add) (apply (hash-ref table port
                        (lambda ()
                          (error 'add-to-input
                                 "current input is not a composite port")))
                      args)]
        [(set!) (hash-set! table port (car args))]
        [(get?) (hash-ref table port (lambda () #f))]
        [else (error 'port->adder-op "unknown message: ~e" msg)]))))

;;=============================================================================
;; Dispatching
;; A dispatcher is a pair of a regexp and a list of dispatch functions.  The
;; regexp should have some parenthesized subexpressions, and the one that
;; actually matched is used to select the dispatching functions, which is
;; invoked on the match.  This functionality is used for the main loop (with
;; the default single dispatcher for "@") and for `get-arg'.

(define (dispatch dispatcher continue failure . copy?)
  (let ([m (if (and (pair? copy?) (car copy?))
             (regexp-match (car dispatcher) (stdin) 0 #f (stdout))
             (regexp-try-match (car dispatcher) (stdin)))])
    (if m
      (ormap (lambda (x y) (and x (y x continue))) (cdr m) (cdr dispatcher))
      (failure))))

;; dispatchers is a list of (string dispatcher) lists
(define (make-dispatcher prefix dispatchers . regexps?)
  (define re
    (if (and (pair? regexps?) (car regexps?)) (lambda (x) x) regexp-quote))
  (cons (regexp (string-append
                 prefix "(?:"
                 (apply string-append
                        (cdr (apply append
                                    (map (lambda (d)
                                           (list "|" (format "(~a)"
                                                             (re (car d)))))
                                         dispatchers))))
                 ")"))
        (map cadr dispatchers)))

;;=============================================================================
;; Dispatchers

(provide dispatchers)
(define dispatchers
  (let ([dispatchers (make-thread-cell '() #t)])
    (case-lambda
     [() (thread-cell-ref dispatchers)]
     [(new) (thread-cell-set! dispatchers new) (rebuild-dispatcher-table)])))
(define dispatcher-table (make-parameter #f))

(provide command-marker)
(define command-marker
  (let ([marker (make-thread-cell #f #t)])
    (case-lambda
     [() (thread-cell-ref marker)]
     [(new)
      (thread-cell-set! marker new)
      (command-marker-here-re
       (and marker (regexp (string-append "^" (regexp-quote new)))))
      (rebuild-dispatcher-table)])))
(define command-marker-here-re (make-parameter #f))

(define (rebuild-dispatcher-table)
  (dispatcher-table
   (make-dispatcher
    "" (if (command-marker)
         `(,@(dispatchers)
           (,(regexp-quote (command-marker)) ,command-dispatcher))
         (dispatchers))
    #t)))

(define (command-dispatcher match cont)
  (define (do-thunk thunk)
    (call-with-values thunk
      (lambda vs
        (define (value->cont v cont)
          (cond [(or (void? v) (not v) (null? v)) cont]
                [(pair? v) (value->cont (car v) (value->cont (cdr v) cont))]
                [(promise? v) (value->cont (force v) cont)]
                [(not (procedure? v))
                 (when (or (bytes? v) (string? v) (path? v) (symbol? v)
                           (number? v) (char? v) (input-port? v))
                   (add-to-input v))
                 cont]
                [(procedure-arity-includes? v 0) (do-thunk v) cont]
                [(procedure-arity-includes? v 1) (lambda () (v cont))]
                [else (error 'mztext "got a bad procedure value: ~e" v)]))
        ((if (andmap (lambda (x) (or (not x) (void? x))) vs)
           (begin (swallow-newline) cont)
           (value->cont vs cont))))))
  (cond [(regexp-try-match (command-marker-here-re) (stdin))
         => (lambda (here) (display (car here)) (cont))]
        [else (let ((r (read-syntax))) (do-thunk (lambda () (eval r))))]))

(provide paren-pairs)
(define paren-pairs
  (make-parameter '(("(" ")") ("[" "]") ("{" "}") ("<" ">"))))

(provide get-arg-reads-word?)
(define get-arg-reads-word? (make-parameter #f))

;; A list of an open regexp for any openning, and then a list of thunks, each
;; one for retreiving a piece of text by some paren pair.
(define arg-dispatcher
  (let ([dispatcher #f] [pairs #f])
    (lambda ()
      (unless (eq? pairs (paren-pairs))
        (set! pairs (paren-pairs))
        (set! dispatcher
              (make-dispatcher
               "^[ \t\r\n\f]*"
               (map (lambda (p) (list (car p) (apply make-arg-getter p)))
                    pairs))))
      dispatcher)))

(define (make-arg-getter open close)
  (let ([re (regexp (if (equal? open close)
                      (begin (set! open close) (regexp-quote close))
                      (format "(~a)|(~a)"
                              (regexp-quote close) (regexp-quote open))))])
    (lambda (match cont)
      (let loop ([level 0] [pos 0])
        (let ([m (regexp-match-peek-positions re (stdin) pos)])
          (unless m (error 'get-arg "missing ~s" close))
          ;; (cadr m) => close, (caddr m) => open
          (cond [(or (eq? open close) (and (zero? level) (cadr m)))
                 (begin0 (read-string (caar m))
                   (regexp-match-positions re (stdin)))]
                [(caddr m) (loop (add1 level) (cdar m))]
                [(cadr m)  (loop (sub1 level) (cdar m))]
                [else (error 'get-arg "internal error")]))))))

(provide get-arg)
(define (get-arg)
  (dispatch
   (arg-dispatcher)
   #f
   (lambda ()
     (let ([m (regexp-try-match
               (if (get-arg-reads-word?) #rx"[^ \t\r\n]+" #rx"[^ \t\r\n]")
               (stdin))])
       (if m (car m) eof)))))

(provide get-arg*)
(define (get-arg*)
  (let ([arg (get-arg)])
    (if (eof-object? arg)
      arg
      (let ([buf (open-output-string)])
        (parameterize ([stdout buf] [stdin (make-composite-input arg)])
          (run) (flush-output buf))
        (get-output-string buf)))))

;;=============================================================================
;; User functionality

(provide swallow-newline)
(define (swallow-newline)
  ;; careful: if there's no match, we don't want to consume the input
  (regexp-try-match #rx"^[ \t]*\r?\n" (stdin))
  (void))

(define (string->substlist args str)
  (if (null? args)
    str
    (let* ([re (map (lambda (x) (regexp-quote (symbol->string x))) args)]
           [re (regexp (string-append
                        "(" (car re)
                        (apply string-append
                               (map (lambda (x) (string-append "|" x))
                                    (cdr re)))
                        ")"))]
           [posns (regexp-match-positions* re str)])
      (define (sub n . m) (apply substring str n m))
      (let loop ([pos 0] [posns posns] [r '()])
        (cond [(null? posns)
               (cons 'list (reverse (if (= pos (string-length str))
                                        r (cons (sub pos) r))))]
              [(= pos (caar posns))
               (loop (cdar posns) (cdr posns)
                     (cons (string->symbol (sub (caar posns) (cdar posns)))
                           r))]
              [else (loop (caar posns) posns
                          (cons (sub pos (caar posns)) r))])))))

(provide defcommand)
(define (defcommand)
  (let ([name (get-arg)] [args (get-arg)] [body (get-arg)])
    (cond
     [(eof-object? name) (error 'defcommand "no name")]
     [(eof-object? args) (error 'defcommand "no args for `~a'" name)]
     [(eof-object? body) (error 'defcommand "no body for `~a'" name)]
     [else
      (let ([name (string->symbol name)]
            [args (read-from-string-all args)]
            [body body])
        (define (get-arg! a)
          (let ([x (get-arg)])
            (if (eof-object? x)
              (error name "expecting an argument for `~s'" a)
              x)))
        (unless (and (list? args) (andmap symbol? args))
          (error 'defcommand "bad arguments for ~s: ~e" name args))
        (eval `(define (,name)
                 (let ,(map (lambda (a) `[,a (,get-arg! ',a)]) args)
                   ,(string->substlist args body)))))])))

;;=============================================================================
;; Invocation

(define (initialize)
  (read-case-sensitive #t)
  (unless (command-marker) (command-marker "@"))
  (namespace-require 'scheme/base)
  (namespace-require 'preprocessor/mztext)
  (do-evals))

(define (run)
  (dispatch (dispatcher-table) run void #t))

(provide include)
(define (include . files)
  (define inputs (if (null? files)
                   (let ([arg (get-arg)])
                     (if (eof-object? arg)
                       (error 'include "expecting a file argument")
                       (list arg)))
                   files))
  (define curdir (cd))
  (define (cd+file f)
    (let*-values ([(f) (if (bytes? f) (bytes->path f) f)]
                  [(dir name dir?)
                   (if (input-port? f) (values #f #f #f) (split-path f))]
                  [(dir) (if (path? dir) dir (cd))])
      ;; could `add-to-input' and then `run' if we wrap this by a (cd dir), but
      ;; instead, plant cd-thunks in the input stream.
      (add-to-input
       (lambda () (cd dir) (current-file (and (string? name) name)))
       (if (input-port? f) f (open-input-file f))
       (lambda () (cd curdir) (current-file #f)))))
  (swallow-newline) ; swallow *before* more stuff is added
  (for-each cd+file (reverse inputs))
  (run))

(provide preprocess)
(define (preprocess . files)
  (initialize)
  (unless (null? files)
    (parameterize ([stdin (make-composite-input)])
      (apply include files))))
