#lang racket/base
(require '#%terminal ffi/unsafe/port)
(module+ test
  (require (for-syntax racket/base))
  (define-syntax (test stx)
    (syntax-case stx ()
      [(_ e1 e2)
       (with-syntax ([line (syntax-line stx)])
         #'(test/proc line e1 e2))]))
  (define (test/proc line e1 e2)
    (unless (equal? e1 e2)
      (error 'test "expected equality on line ~s\n  ~e\n  ~e"
             line e1 e2))))
(provide mk-setup-fprintf)

(define (terminal-get-screen-height) (car (terminal-get-screen-size)))
(define (terminal-get-screen-width) (cdr (terminal-get-screen-size)))

(define (mk-setup-fprintf name-str timestamp-output?)
  (define op (current-output-port))
  (define ip (current-input-port))
  (define terminal-available?
    (let ()
      (define in-fd (unsafe-port->file-descriptor ip))
      (define out-fd (unsafe-port->file-descriptor op))
      (and in-fd
           out-fd
           (terminal-init in-fd out-fd))))

  (define last-position #f)

  ;; (or/c #f (hash natural? -o> (cons natural? string)))
  ;; captures the current state of the drawing
  ;; the domain are the `#:n` arguments to setup-printf
  ;; the range are a pair of the `i` argument passed to
  ;; `write-something-at-line-i-from-bottom` and the string
  ;; that appears in that line
  (define lines-status #f)

  ;; n : (or/c natural? #f)
  ;;  if `n` isn't #f, then we should take over the terminal
  ;;  and show only the latest line for each `n`
  (define (setup-fprintf #:n [n #f] #:only-if-terminal? [only-if-terminal? #f] p task s . args)
    (let ([task (if task (string-append task ": ") "")])
      (define st
        (string-append name-str ": " task s
                       (if timestamp-output?
                           (format " @ ~a" (current-process-milliseconds))
                           "")))
      (define formatted-st
        (if (null? args)
            st
            (apply format st args)))
      (cond
        [(and terminal-available?
              (eq? p op)
              (or n lines-status))
         (define-values (_1 _2 pos-before) (port-next-location op))
         (write-nth-line ip n formatted-st only-if-terminal? (if last-position (= pos-before last-position) #t))
         (define-values (_3 _4 pos-after) (port-next-location op))
         (set! last-position pos-after)]
        [(not only-if-terminal?)
         (write-string formatted-st p)
         (newline p)
         (flush-output p)
         (set! last-position #f)])))

  (define (write-nth-line ip n str only-if-terminal? output-didnt-move?)
    (when (and n (not lines-status))
      ;; entering
      (set! lines-status (make-hash)))
    (when (and (not n) lines-status)
      ;; exiting
      (set! lines-status #f))
    (when (if only-if-terminal? lines-status #t)
      (cond
        [lines-status
         ;; we are in the mode; check to see if something seems to have been
         ;; printed for us and, if so, make a new, blank set of lines for us
         ;; to control
         (unless output-didnt-move?
           (for ([i (in-list (sort (hash-keys lines-status) <))])
             (printf "~a\n" (cdr (hash-ref lines-status i)))))
         ;; now we have the space set up; figure out if have a place to
         ;; write the new line that we've been given
         (define have-a-spot?
           (cond
             [(hash-has-key? lines-status n) #t]
             [((hash-count lines-status) . < . (- (terminal-get-screen-height) 1))
              ;; here we need to make a space for the line; first insert a blank line at the end
              (printf "\n")
              ;; then update the `lines-status` table and
              ;; make a blank space for the new line
              (add-an-entry lines-status n #t)]
             ;; if there are too many lines to fit on the screen, just drop
             ;; new lines; (this doesn't seem like it is going to happen much)
             [else #f]))
         (when have-a-spot?
           ;; now we've got space for this line, let's write it
           (define pr (hash-ref lines-status n))
           (define i (car pr))
           (unless (equal? str (cdr pr))
             ;; don't write a line if it already has the right text;
             ;; avoids flicker that can happen with repeated "idle" lines
             (hash-set! lines-status n (cons i str))
             (write-something-at-line-i-from-bottom i str)))]
        [else
         ;; we're out of the mode; just print normally
         (write-string str op)
         (newline op)
         (flush-output op)])))
  setup-fprintf)

;; adjust the `lines-status` table to track a new line.
;; This involves rewriting lines that are below the
;; newly entered line to move them onto new lines (and avoiding
;; flicker by not doing anything for lines that are above).
(define (add-an-entry lines-status n update-lines?)
  (define before (sort (hash-keys lines-status) <))
  (define lines-taken-up (hash-count lines-status))
  (let loop ([before before]
             [j lines-taken-up]
             [found-line? #f])
    (cond
      [(null? before)
       (unless found-line?
         (hash-set! lines-status n (cons (+ j 1) "")))]
      [else
       (define fst (car before))
       (define pr (hash-ref lines-status fst))
       (define i (car pr))
       (define str (cdr pr))
       (cond
         [(< fst n)
          (hash-set! lines-status fst (cons (+ i 1) str))
          (loop (cdr before) (- j 1) #f)]
         [(not found-line?)
          (hash-set! lines-status n (cons (+ j 1) ""))
          (loop before (- j 1) #t)]
         [else
          (when update-lines? (write-something-at-line-i-from-bottom i str))
          (loop (cdr before) (- j 1) #t)])])))

(module+ test
  (let ()
    (define lines-status (make-hash))
    (add-an-entry lines-status 3 #f)
    (test lines-status (make-hash (list (cons 3 (cons 1 "")))))
    (hash-set! lines-status 3 (cons 1 "abc"))
    (add-an-entry lines-status 4 #f)
    (test lines-status (make-hash (list (cons 3 (cons 2 "abc"))
                                        (cons 4 (cons 1 "")))))
    (hash-set! lines-status 4 (cons 1 "def"))
    (add-an-entry lines-status 1 #f)
    (test lines-status (make-hash (list (cons 1 (cons 3 ""))
                                        (cons 3 (cons 2 "abc"))
                                        (cons 4 (cons 1 "def")))))))

(define (write-something-at-line-i-from-bottom i str)
  (terminal-move-cursor 'up i)
  (define terminal-w (terminal-get-screen-width))
  (define written-width
    (let loop ([w 0]
               [i 0])
      (cond
        ;; this won't work right if we're right at the
        ;; edge and we write a character that takes
        ;; more than one space. What to do then?
        [(>= w terminal-w) w]
        [(< i (string-length str))
         (define c (string-ref str i))
         (define char-w (terminal-write-char c))
         (loop (+ (if (= char-w -128) 1 char-w) w)
               (+ i 1))]
        [else w])))
  (terminal-clear 'eol)
  (terminal-move-cursor 'left written-width)
  (terminal-move-cursor 'down i))

(module+ main
  (define setup-fprintf (mk-setup-fprintf "raco setup" #f))
  (for ([i (in-range 1000)])
    (sleep 0.01)
    (let ([n (random 100)])
      (setup-fprintf (current-output-port) "task" #:n n
                     (format "line ~ax~a ~a ~a"
                             (terminal-get-screen-width)
                             (terminal-get-screen-height)
                             n (for/list ([i (in-range (+ 1 (random 2)))]) (random))))))
  (setup-fprintf (current-output-port) "task" "done"))
