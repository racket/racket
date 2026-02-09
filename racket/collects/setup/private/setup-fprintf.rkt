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

  ;; last-%age : (or/c #f (real-in 0 1))
  (define last-%age #f)

  ;; n : (or/c natural? #f)
  ;;  if `n` isn't #f, then we should take over the terminal
  ;;  and show only the latest line for each `n`
  ;; %age : (or/c #f (real-in 0 1))
  ;;  if a real, then show a progress bar along the bottom of the
  ;;    screen with a matching width
  ;;  if #f, then don't change the progress from whatever its
  ;;    value was the last time `setup-fprintf` was called
  (define (setup-fprintf #:n [n #f] #:%age [%age #f]
                         #:only-if-terminal? [only-if-terminal? #f]
                         p task s . args)
    (unless (or (not %age)
                (and (real? %age) (<= 0 %age 1)))
      (raise-arguments-error 'setup-fprintf
                             "%age must be (or/c #f (real-in 0 1))"
                             '#:%age %age))

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
         (define pos-before (terminal-file-position))
         (define output-moved? (and last-position (not (= pos-before last-position))))
         (when output-moved?
           (set! last-%age #f))
         (write-nth-line ip n %age formatted-st only-if-terminal? output-moved?)
         (define pos-after (terminal-file-position))
         (set! last-position pos-after)
         (when %age
           (set! last-%age %age))]
        [else
         (unless only-if-terminal?
           (write-string formatted-st p)
           (newline p)
           (flush-output p))
         (set! last-position #f)])))

  (define (print-%age %age)
    ;; we just skip the progress bar when the screen
    ;; is too skinny; there will be a blank line allocated
    ;; for it, however.
    (when (> (terminal-get-screen-width) 30)
      (terminal-move-cursor 'up 1)
      (define chars " ▏▎▍▌▋▊▉█")
      (define w (- (terminal-get-screen-width) 6))
      (define chars-width (* %age w))
      (call-with-exception-handler
       (λ (x) (terminal-set-color -1 #f) x)
       (λ ()
         (define today (seconds->date (current-seconds)))
         (define-values (fg-color1 fg-color2)
           (cond
             [(and (= (date-day today) 25)
                   (= (date-month today) 12))
              (values 1 2)]
             [else (values 7 7)]))
         (define color-band-width
           (cond
             [(<= w 50) 1]
             [(<= w 100) 2]
             [else 3]))
         (for ([i (in-range w)])
           (define fg-color
             (if (< (modulo i (* color-band-width 2)) color-band-width)
                 fg-color1
                 fg-color2))
           (terminal-set-color fg-color #f)
           (cond
             [(and (<= i chars-width) (< chars-width (+ i 1)))
              (define remainder-width
                (floor (* (string-length chars)
                          (- chars-width (floor chars-width)))))
              (terminal-write-char (string-ref chars remainder-width))]
             [(for/or ([j (in-inclusive-range 1 9)])
                (define tenth (* (/ j 10) w))
                ;; the dark background light line bars are one
                ;; side of the character and the light background
                ;; dark line bars are on the other side of the character
                ;; so we print them off by one from each other to make
                ;; them look as close together as possible.
                (if (> i chars-width)
                    (and (<= i (+ tenth 1)) (< (+ tenth 1) (+ i 1)))
                    (and (<= i tenth) (< tenth (+ i 1)))))
              (cond
                [(> i chars-width)
                 (terminal-set-color -1 #f)
                 (terminal-write-char (string-ref chars 1))
                 (terminal-set-color fg-color #f)]
                [else
                 (terminal-write-char (string-ref chars (- (string-length chars) 3)))])]
             [(< i chars-width) (terminal-write-char (string-ref chars (- (string-length chars) 1)))]
             [else (terminal-write-char (string-ref chars 0))]))
         (terminal-set-color -1 #f)))

      (cond
        [(= %age 1)
         (terminal-write-char #\space)
         (terminal-write-char #\1)
         (terminal-write-char #\0)
         (terminal-write-char #\0)
         (terminal-write-char #\%)]
        [else
         (define %age-str (number->string (floor (* %age 100))))
         (define one-digit? (= 1 (string-length %age-str)))
         (terminal-write-char #\space)
         (terminal-write-char #\space)
         (terminal-write-char (if one-digit? #\space (string-ref %age-str 0)))
         (terminal-write-char (if one-digit? (string-ref %age-str 0) (string-ref %age-str 1)))
         (terminal-write-char #\%)])

      (terminal-move-cursor 'left (+ w 5))
      (terminal-move-cursor 'down 1)))

  (define (write-nth-line ip n %age str only-if-terminal? output-moved?)
    (when (and n (not lines-status))
      ;; entering
      (set! lines-status (make-hash)))
    (when (and (not n) lines-status)
      ;; exiting
      ;; we cheat here on the 100% line -- if we're exiting
      ;; and there is a percentage line at all, then go up
      ;; and print a line that says it finished with 100%,
      ;; no matter what the progress actually was at before.
      (when last-%age (print-%age 1))
      (set! lines-status #f)
      (set! last-%age #f))
    (when (if only-if-terminal? lines-status #t)
      (cond
        [lines-status
         (define need-%age-space? (or %age last-%age))
         ;; we are in the mode; check to see if something seems to have been
         ;; printed for us and, if so, make a new set of lines for us
         ;; to control
         (when output-moved?
           (for ([i (in-list (sort (hash-keys lines-status) <))])
             (printf "~a\n" (cdr (hash-ref lines-status i))))
           (when need-%age-space? (printf "\n")))
         ;; now we have the space set up; figure out if have a place to
         ;; write the new line that we've been given
         (define have-a-spot?
           (cond
             [(hash-has-key? lines-status n) #t]
             [((hash-count lines-status) . < . (- (terminal-get-screen-height) 2))
              ;; here we need to make a space for the line; first insert a blank line at the end
              (printf "\n")
              ;; then update the `lines-status` table and
              ;; make a blank space for the new line
              (add-an-entry lines-status n #t need-%age-space?)]
             ;; if there are too many lines to fit on the screen, just drop
             ;; new lines; (this doesn't seem like it is going to happen much)
             ;; we always save a line for the status bar, even if we're not using it
             [else #f]))
         (when (and %age (not last-%age))
           ;; we have a percentage but we haven't had one before,
           ;; allocate the blank line for it.
           (unless output-moved?
             ;; if the output moved, we already made a blank space
             (printf "\n")))
         (when have-a-spot?
           ;; now we've got space for this line, let's write it
           (define pr (hash-ref lines-status n))
           (define i (car pr))
           (unless (equal? str (cdr pr))
             ;; don't write a line if it already has the right text;
             ;; avoids flicker that can happen with repeated "idle" lines
             (hash-set! lines-status n (cons i str))
             (write-something-at-line-i-from-bottom (if need-%age-space? (+ i 1) i) str)))
         (when %age (print-%age %age))]
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
(define (add-an-entry lines-status n update-lines? need-%age-space?)
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
          (when update-lines? (write-something-at-line-i-from-bottom (if need-%age-space? (+ i 1) i) str))
          (loop (cdr before) (- j 1) #t)])])))

(module+ test
  (let ()
    (define lines-status (make-hash))
    (add-an-entry lines-status 3 #f #f)
    (test lines-status (make-hash (list (cons 3 (cons 1 "")))))
    (hash-set! lines-status 3 (cons 1 "abc"))
    (add-an-entry lines-status 4 #f #f)
    (test lines-status (make-hash (list (cons 3 (cons 2 "abc"))
                                        (cons 4 (cons 1 "")))))
    (hash-set! lines-status 4 (cons 1 "def"))
    (add-an-entry lines-status 1 #f #f)
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
  (for ([i (in-range 200)])
    (sleep 0.01)
    (let ([n (random 10)])
      (setup-fprintf (current-output-port) "task" #:n n
                     #:%age (/ i 200)
                     (format "line ~ax~a ~a ~a"
                             (terminal-get-screen-width)
                             (terminal-get-screen-height)
                             n
                             (for/list ([i (in-range (+ 1 (random 2)))]) (random))))))
  (setup-fprintf (current-output-port) "task" "phase1 done")
  (for ([i (in-range 200)])
    (sleep 0.01)
    (let ([n (random 10)])
      (setup-fprintf (current-output-port) "task" #:n n
                     (format "line ~ax~a ~a ~a"
                             (terminal-get-screen-width)
                             (terminal-get-screen-height)
                             n
                             (for/list ([i (in-range (+ 1 (random 2)))]) (random))))))
  (setup-fprintf (current-output-port) "task" "all done"))
