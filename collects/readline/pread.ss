#lang scheme/base

(require "mzrl.ss" scheme/list scheme/file)

;; --------------------------------------------------------------------------
;; Configuration

(define current-prompt   (make-parameter #"> "))
(define max-history      (make-parameter 100))
(define keep-duplicates  (make-parameter #f))
(define keep-blanks      (make-parameter #f))
(provide current-prompt max-history keep-duplicates keep-blanks)

;; --------------------------------------------------------------------------
;; Simple namespace-based completion

;; efficiently convert symbols to byte strings
(define symbol->bstring
  (let ([t (make-weak-hash)])
    (lambda (sym)
      (or (hash-ref t sym #f)
          (let ([bstr (string->bytes/utf-8 (symbol->string sym))])
            (hash-set! t sym bstr)
            bstr)))))

;; get a list of byte strings for current bindings, cache last result
(define get-namespace-bstrings
  (let ([last-syms #f] [last-bstrs #f])
    (lambda ()
      (let ([syms (namespace-mapped-symbols)])
        (unless (equal? syms last-syms)
          (set! last-syms syms)
          (set! last-bstrs (sort (map symbol->bstring syms) bytes<?)))
        last-bstrs))))

(define (namespace-completion pat)
  (let* ([pat (if (string? pat) (string->bytes/utf-8 pat) pat)]
         [pat (regexp-quote pat)]
         [pat (regexp-replace* #px#"(\\w)\\b" pat #"\\1\\\\w*")]
         [pat (byte-pregexp (bytes-append #"^" pat))])
    (filter (lambda (bstr) (regexp-match pat bstr))
            (get-namespace-bstrings))))

(set-completion-function! namespace-completion)

;; --------------------------------------------------------------------------
;; History management
;; (Note: local-history, and the preference are in reverse order, from
;; the newest to the oldest.)

(define local-history '())

(define (trim-local-history)
  (when ((length local-history) . > . (max-history))
    (set! local-history (take local-history (max-history)))))

(define (load-history)
  (set! local-history (get-preference 'readline-input-history (lambda () null)))
  (trim-local-history)
  (for-each add-history (reverse local-history)))
;; add it now to the actual history
(load-history)

(define (save-history)
  (put-preferences '(readline-input-history) (list local-history)))

(define (add-to-history s force-keep?)
  (define keep (or force-keep? (keep-duplicates)))
  (when (and (bytes? s) (or (keep-blanks) (not (zero? (bytes-length s)))))
    ;; remove duplicate (keep-blanks determines how we search)
    (unless (or (null? local-history) (eq? #t keep))
      (let ([dup (let loop ([n -1] [h local-history] [r '()])
                   (cond [(null? h) #f]
                         [(equal? (car h) s) `(,n ,@(reverse r) ,@(cdr h))]
                         [(eq? keep 'unconsecutive) #f] ; no loop
                         [else (loop (sub1 n) (cdr h) (cons (car h) r))]))])
        (when dup
          (set! local-history (cdr dup))
          (history-delete (car dup)))))
    (add-history-bytes s)
    (let loop ()
      (when ((history-length) . > . (max-history)) (history-delete 0) (loop)))
    (set! local-history (cons s local-history))
    (trim-local-history)))

;; remove `l' items from `local-history', ignoring ones that are not
;; in the front of the history (in the eq? sense)
(define (drop-from-history l)
  (let loop ([l l] [h local-history])
    (if (and (pair? l) (pair? h))
      (if (eq? (car l) (car h))
        (begin (history-delete -1) (loop (cdr l) (cdr h)))
        (loop (cdr l) h))
      (set! local-history h))))

;; captured now so we don't flush some other output port
(define readline-output-port (current-output-port))
(port-count-lines! readline-output-port)

(define (readline-bytes/hist p force-keep?)
  (when (eq? readline-output-port (current-output-port))
    (let-values ([(line col pos) (port-next-location readline-output-port)])
      (when (< 0 col) (newline readline-output-port))))
  (let ([s (readline-bytes p)]) (add-to-history s force-keep?) s))

(exit-handler
 (let ([old (exit-handler)])
   (lambda (v) (save-history) (old v))))

;; --------------------------------------------------------------------------
;; An input port that goes through readline

;; readline-prompt can be
;;   #f: no prompt (normal state),
;;   <bytes>: a prompt to use
;;   'space: a prompt has been used, now use spaces instead
;;           (from readline-prompt-spaces)
;; this also controls saving multi-line histories: when the prompt is #f we
;; collect history as usual; otherwise, we accumulate the lines in a chunk (and
;; add them to the history without removing duplicates) and at the beginning of
;; each new chunk (when we read a line with a prompt that is not 'space) we
;; throw away the intermediate history lines that were added and add the whole
;; chunk as one big multiline string.
(provide readline-prompt)
(define readline-prompt (make-parameter #f))
(define readline-prompt-spaces (make-parameter #"  "))
(define multiline-chunk (make-parameter '()))

(define (do-multiline-chunk)
  (let ([chunk (multiline-chunk)])
    (when (pair? chunk)
      (drop-from-history chunk)
      (add-to-history (apply bytes-append (reverse chunk)) #f)
      (multiline-chunk '()))))

(define (readline-bytes/multiline-chunk prompt)
  (let ([line (readline-bytes/hist prompt #t)])
    (when (and (bytes? line) (not (zero? (bytes-length line))))
      (let ([c (multiline-chunk)])
        (multiline-chunk (if (pair? c)
                           (list* line (readline-prompt-spaces) #"\n" c)
                           (cons line c)))))
    line))

(define (do-one-line)
  (let ([p (readline-prompt)])
    (case p
      [(#f) (do-multiline-chunk) (readline-bytes/hist #"" #f)]
      [(space) (readline-bytes/multiline-chunk (readline-prompt-spaces))]
      [else (do-multiline-chunk)
            (unless (= (bytes-length (readline-prompt-spaces))
                       (bytes-length p))
              (readline-prompt-spaces (make-bytes (bytes-length p) 32)))
            (readline-prompt 'space) ; use spaces next time
            (readline-bytes/multiline-chunk p)])))

(provide readline-input)
(define readline-input
  (let ([buffer  #f]
        [skip    #f]
        [blen    #f]
        [closed? #f]
        [LF      (bytes-ref #"\n" 0)])
    (define (close!) (set! closed? #t) (save-history))
    (define (reader tgt)
      (let loop ()
        (cond [closed? eof]
              [(eof-object? buffer) (set! buffer #f) eof]
              [(not buffer)
               (set! buffer (do-one-line))
               (if (eof-object? buffer)
                 (begin (save-history) (set! buffer #f) eof)
                 (begin (set! skip 0)
                        (set! blen (bytes-length buffer))
                        (reader tgt)))]
              [else
               ;; copy bytes
               (let ([tgtlen (bytes-length tgt)]
                     [left (- blen skip)])
                 (cond [(< tgtlen left) ; not enough target space
                        (let ([end (+ skip tgtlen)])
                          (bytes-copy! tgt 0 buffer skip end)
                          (set! skip end)
                          tgtlen)]
                       [(= tgtlen left) ; enough room for text but no newline
                        (bytes-copy! tgt 0 buffer skip blen)
                        (set! skip blen)
                        left]
                       [else ; enough room for text with newline
                        (bytes-copy! tgt 0 buffer skip blen)
                        (bytes-set! tgt left LF)
                        (set! buffer #f)
                        (add1 left)]))])))
    (make-input-port 'readline-input reader #f close!)))

;; --------------------------------------------------------------------------
;; Reading functions

;; a function that can be used for current-prompt-read
(provide read-cmdline-syntax)
(define (read-cmdline-syntax)
  (define prompt (current-prompt))
  (flush-output)
  ;; needs to set `readline-prompt' to get a prompt when reading
  (parameterize ([read-accept-reader #t] [readline-prompt prompt])
    (unless (eq? readline-input (current-input-port))
      ;; not the readline port -- print the prompt (changing the
      ;; readline-prompt and using read-complete-syntax below should still
      ;; work fine)
      (display prompt) (flush-output))
    (begin0 (let ([in (current-input-port)])
              ((current-read-interaction) (object-name in) in))
            (do-multiline-chunk))))
