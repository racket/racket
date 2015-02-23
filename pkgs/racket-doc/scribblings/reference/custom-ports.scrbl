#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "customport"]{Custom Ports}

The @racket[make-input-port] and @racket[make-output-port] procedures
create @deftech{custom ports} with arbitrary control procedures (much like
implementing a device driver). Custom ports are mainly useful to
obtain fine control over the action of committing bytes as read or
written.

@defproc[(make-input-port [name any/c]
                          [read-in (or/c
                                    (bytes? 
                                     . -> . (or/c exact-nonnegative-integer?
                                                  eof-object?
                                                  procedure?
                                                  evt?))
                                    input-port?)]
                          [peek (or/c
                                 (bytes? exact-nonnegative-integer? (or/c evt? #f)
                                         . -> . (or/c exact-nonnegative-integer?
                                                      eof-object?
                                                      procedure?
                                                      evt?
                                                      #f))
                                 input-port?)]
                          [close (-> any)]
                          [get-progress-evt (or/c (-> evt?) #f) #f]
                          [commit (or/c (exact-positive-integer? evt? evt? . -> . any)
                                        #f) 
                                  #f]
                          [get-location (or/c 
                                         (->
                                          (values (or/c exact-positive-integer? #f)
                                                  (or/c exact-nonnegative-integer? #f)
                                                  (or/c exact-positive-integer? #f)))
                                         #f)
                                        #f]
                          [count-lines! (-> any) void]
                          [init-position (or/c exact-positive-integer?
                                               port?
                                               #f
                                               (-> (or/c exact-positive-integer? #f)))
                                         1]
                          [buffer-mode (or/c (case-> ((or/c 'block 'none) . -> . any)
                                                     (-> (or/c 'block 'none #f)))
                                             #f)
                                       #f])
         input-port?]{

Creates an input port, which is immediately open for reading. If
@racket[close] procedure has no side effects, then the port need not
be explicitly closed. See also @racket[make-input-port/peek-to-read].

The arguments implement the port as follows:

@itemize[

  @item{@racket[name] --- the name for the input port.}

  @item{@racket[read-in] --- either an input port, in which case reads
    are redirected to the given port, or a procedure that takes a single
    argument:
    a mutable byte string to receive read bytes. The procedure's
    result is one of the following:
    @itemize[

      @item{the number of bytes read, as an exact, non-negative integer;}

      @item{@racket[eof];}

      @item{a procedure of arity four (representing a ``special''
      result, as discussed @elemref["special"]{further below}) and
      optionally of arity zero, but a procedure result is allowed only
      when @racket[peek] is not @racket[#f];}

      @item{a @techlink{pipe} input port that supplies bytes to be
      used as long as the pipe has content (see
      @racket[pipe-content-length]) or until @racket[read-in] or
      @racket[peek] is called again; or}

      @item{a @tech{synchronizable event} (see @secref["sync"]) other
      than a pipe input port or procedure of arity four; the event
      becomes ready when the read is complete (roughly): the event's
      value can be one of the above three results or another event like
      itself; in the last case, a reading process loops with
      @racket[sync] until it gets a non-event result.}

    ]

    The @racket[read-in] procedure must not block indefinitely. If no
    bytes are immediately available for reading, the @racket[read-in]
    must return @racket[0] or an event, and preferably an event (to
    avoid busy waits). The @racket[read-in] should not return
    @racket[0] (or an event whose value is @racket[0]) when data is
    available in the port, otherwise polling the port will behave
    incorrectly. An event result from an event can also break polling.

    If the result of a @racket[read-in] call is not one of the above
    values, the @exnraise[exn:fail:contract]. If a returned integer is
    larger than the supplied byte string's length, the
    @exnraise[exn:fail:contract]. If @racket[peek] is @racket[#f] and
    a procedure for a @elemref["special"]{special} result is returned,
    the @exnraise[exn:fail:contract].

    The @racket[read-in] procedure can report an error by raising an
    exception, but only if no bytes are read. Similarly, no bytes
    should be read if @racket[eof], an event, or a procedure is
    returned. In other words, no bytes should be lost due to spurious
    exceptions or non-byte data.

    A port's reading procedure may be called in multiple threads
    simultaneously (if the port is accessible in multiple threads),
    and the port is responsible for its own internal
    synchronization. Note that improper implementation of such
    synchronization mechanisms might cause a non-blocking read
    procedure to block indefinitely.

    If the result is a pipe input port, then previous
    @racket[get-progress-evt] calls whose event is not yet ready must
    have been the pipe input port itself. Furthermore,
    @racket[get-progress-evt] must continue to return the pipe as long
    as it contains data, or until the @racket[read-in] or
    @racket[peek-in] procedure is called again (instead of using the
    pipe, for whatever reason). If @racket[read-in] or
    @racket[peek-in] is called, any previously associated pipe (as
    returned by a previous call) is disassociated from the
    port and is not in use by any other thread as a result of the
    previous association.

    If @racket[peek], @racket[get-progress-evt], and
    @racket[commit] are all provided and
    non-@racket[#f], then the following is an acceptable implementation
    of @racket[read-in]:

@racketblock[
(code:line
   (lambda (bstr)
     (let* ([progress-evt (get-progress-evt)]
            [v (peek bstr 0 progress-evt)])
       (cond
        [(sync/timeout 0 progress-evt) 0] (code:comment #,(t "try again"))
        [(evt? v) (wrap-evt v (lambda (x) 0))] (code:comment #,(t "sync, try again"))
        [(and (number? v) (zero? v)) 0] (code:comment #,(t "try again"))
        [else
         (if (commit (if (number? v) v 1)
                         progress-evt
                         always-evt)
             v      (code:comment #,(t "got a result"))
             0)]))) (code:comment #,(t "try again"))
)]

    An implementor may choose not to implement the @racket[peek],
    @racket[get-progress-evt], and @racket[commit]
    procedures, however, and even an implementor who does supply
    them may provide a different @racket[read-in]
    that uses a fast path for non-blocking reads.

    In an input port is provided for @racket[read-in], then an input port
    must also be provided for @racket[peek].}


  @item{@racket[peek] --- either @racket[#f], an input port (in which
   case peeks are redirected to the given port), or a procedure
   that takes three arguments:

     @itemize[

     @item{a mutable byte string to receive peeked bytes;}

     @item{a non-negative number of bytes (or
     @elemref["special"]{specials}) to skip before peeking; and}

     @item{either @racket[#f] or a progress event produced by
     @racket[get-progress-evt].}

     ]

    The results and conventions for @racket[peek] are mostly the same
    as for @racket[read-in]. The main difference is in the handling of
    the progress event, if it is not @racket[#f].  If the given
    progress event becomes ready, the @racket[peek] must abort any
    skip attempts and not peek any values. In particular,
    @racket[peek] must not peek any values if the progress event is
    initially ready. If the port has been closed, the progress event
    should be ready, in which case @racket[peek] should complete
    (instead of failing because the port is closed).
    
    Unlike @racket[read-in], @racket[peek] should produce
    @racket[#f] (or an event whose value is @racket[#f]) if no bytes
    were peeked because the progress event became ready. Like
    @racket[read-in], a @racket[0] result indicates that another
    attempt is likely to succeed, so @racket[0] is inappropriate when
    the progress event is ready. Also like @racket[read-in],
    @racket[peek] must not block indefinitely.

    The skip count provided to @racket[peek] is a number of bytes (or
    @elemref["special"]{specials}) that must remain present in the
    port---in addition to the peek results---when the peek results are
    reported. If a progress event is supplied, then the peek is
    effectively canceled when another process reads data before the
    given number can be skipped. If a progress event is not supplied
    and data is read, then the peek must effectively restart with the
    original skip count.

    The system does not check that multiple peeks return consistent
    results, or that peeking and reading produce consistent results.

    If @racket[peek] is @racket[#f], then peeking for the port is
    implemented automatically in terms of reads, but with several
    limitations. First, the automatic implementation is not
    thread-safe. Second, the automatic implementation cannot handle
    @elemref["special"]{special} results (non-byte and non-eof), so
    @racket[read-in] cannot return a procedure for a
    @elemref["special"]{special} when @racket[peek] is
    @racket[#f]. Finally, the automatic peek implementation is
    incompatible with progress events, so if @racket[peek] is
    @racket[#f], then @racket[progress-evt] and @racket[commit] must
    be @racket[#f]. See also @racket[make-input-port/peek-to-read],
    which implements peeking in terms of @racket[read-in] without
    these constraints.

    In an input port is provided for @racket[peek], then an input port
    must also be provided for @racket[read-in].}

  @item{@racket[close] --- a procedure of zero arguments that is
    called to close the port. The port is not considered closed until
    the closing procedure returns. The port's procedures will never be
    used again via the port after it is closed. However, the closing
    procedure can be called simultaneously in multiple threads (if the
    port is accessible in multiple threads), and it may be called
    during a call to the other procedures in another thread; in the
    latter case, any outstanding reads and peeks should be terminated
    with an error.}

  @item{@racket[get-progress-evt] --- either @racket[#f] (the
    default), or a procedure that takes no arguments and returns an
    event. The event must become ready only after data is next read
    from the port or the port is closed. If the port is already closed,
    the event must be ready. After the event becomes
    ready, it must remain so. See the description of @racket[read-in]
    for information about the allowed results of this function when
    @racket[read-in] returns a pipe input port. See also
    @racket[semaphore-peek-evt], which is sometimes useful for
    implementing @racket[get-progress-evt].

    If @racket[get-progress-evt] is @racket[#f], then
    @racket[port-provides-progress-evts?] applied to the port will
    produce @racket[#f], and the port will not be a valid argument to
    @racket[port-progress-evt].

    The result event will not be exposed directly by
    @racket[port-progress-evt]. Instead, it will be wrapped in an
    event for which @racket[progress-evt?] returns true.}

  @item{@racket[commit] --- either @racket[#f] (the
    default), or a procedure that takes three arguments: 

     @itemize[

     @item{an exact, positive integer @math{k_r};}

     @item{a progress event produced by @racket[get-progress-evt];}

     @item{an event, @racket[_done], that is either a channel-put
           event, channel, semaphore, semaphore-peek event, always
           event, or never event.}

     ]

     A @defterm{commit} corresponds to removing data from the stream
     that was previously peeked, but only if no other process removed
     data first. (The removed data does not need to be reported,
     because it has been peeked already.) More precisely, assuming
     that @math{k_p} bytes, @elemref["special"]{specials}, and
     mid-stream @racket[eof]s have been previously peeked or skipped
     at the start of the port's stream, @racket[commit] must satisfy
     the following constraints:

     @itemize[

     @item{It must return only when the commit is complete or when the
     given progress event becomes ready.}

     @item{It must commit only if @math{k_p} is positive.}

     @item{If it commits, then it must do so with either @math{k_r} items
     or @math{k_p} items, whichever is smaller, and only if @math{k_p} is
     positive.}

     @item{It must never choose @racket[_done] in a synchronization
     after the given progress event is ready, or after @racket[_done]
     has been synchronized once.}

     @item{It must not treat any data as read from the port unless
     @racket[_done] is chosen in a synchronization.}

     @item{It must not block indefinitely if @racket[_done] is ready;
     it must return soon after the read completes or soon after the
     given progress event is ready, whichever is first.}

     @item{It can report an error by raising an exception, but only if
      no data has been committed. In other words, no data should be lost due to
      an exception, including a break exception.}

     @item{It must return a true value if data has been committed,
     @racket[#f] otherwise. When it returns a value, the given
     progress event must be ready (perhaps because data has just been
     committed).}

     @item{It should return a byte string as a true result when line
     counting is enabled and @racket[get-location] is @racket[#f] (so
     that line counting is implemented the default way); the result
     byte string represents the data that was committed for the
     purposes of character and line counting. If any other true result
     is returned when a byte string is expected, it is treated like a
     byte string where each byte corresponds to a non-newline
     character.}

     @item{It must raise an exception if no data (including
     @racket[eof]) has been peeked from the beginning of the port's
     stream, or if it would have to block indefinitely to wait for the
     given progress event to become ready.}

     ]

    A call to @racket[commit] is @racket[parameterize-break]ed to
    disable breaks.}

  @item{@racket[get-location] --- either @racket[#f] (the
    default), or a procedure that takes no arguments and returns three
    values: the line number for the next item in the port's stream (a
    positive number or @racket[#f]), the column number for the next
    item in the port's stream (a non-negative number or @racket[#f]),
    and the position for the next item in the port's stream (a
    positive number or @racket[#f]). See also @secref["linecol"].  

    This procedure is called to implement @racket[port-next-location],
    but only if line counting is enabled for the port via
    @racket[port-count-lines!] (in which case @racket[count-lines!] is
    called). The @racket[read] and @racket[read-syntax] procedures
    assume that reading a non-whitespace character increments the
    column and position by one.}

  @item{@racket[count-lines!] --- a procedure of no arguments
    that is called if and when line counting is enabled for the port.
    The default procedure is @racket[void].}

  @item{@racket[init-position] --- normally an exact, positive integer
    that determines the position of the port's first item, which is
    used by @racket[file-position] or when line counting is
    @italic{not} enabled for the port. The default is @racket[1].  If
    @racket[init-position] is @racket[#f], the port is treated as
    having an unknown position. If @racket[init-position] is a port,
    then the given port's position is always used for the new port's
    position. If @racket[init-position] is a procedure, it is called
    as needed to obtain the port's position.}

  @item{@racket[buffer-mode] --- either @racket[#f] (the default) or a
    procedure that accepts zero or one arguments. If
    @racket[buffer-mode] is @racket[#f], then the resulting port does
    not support a buffer-mode setting. Otherwise, the procedure is
    called with one symbol argument (@racket['block] or
    @racket['none]) to set the buffer mode, and it is called with zero
    arguments to get the current buffer mode. In the latter case, the
    result must be @racket['block], @racket['none], or @racket[#f]
    (unknown). See @secref["port-buffers"] for more information on
    buffer modes.}

 ]

 @elemtag["special"]{@bold{``Special'' results:}} When
 @racket[read-in] or @racket[peek] (or an event produced by one of
 these) returns a procedure, the procedure is used to obtain a
 non-byte result. (This non-byte result is @italic{not} intended to
 return a character or @racket[eof]; in particular, @racket[read-char]
 raises an exception if it encounters a special-result procedure, even
 if the procedure produces a byte.) A special-result procedure must
 accept four arguments, and it can optionally accept zero arguments:

 @itemize[

  @item{When the special read is triggered by @racket[read-syntax]
  or @racket[read-syntax/recursive], the
  procedure is passed four arguments that represent a source
  location.}

  @item{When the special read is triggered by @racket[read],
  @racket[read-byte-or-special],
  @racket[read-char-or-special], @racket[peek-byte-or-special], or
  @racket[peek-char-or-special], the procedure is passed no arguments
  if it accepts zero arguments, otherwise it is passed four arguments
  that are all @racket[#f].}

 ]

 The special-value procedure can return an arbitrary value, and it
 will be called zero or one times (not necessarily before further
 reads or peeks from the port). See @secref["reader-procs"] for
 more details on the procedure's result.

 If @racket[read-in] or @racket[peek] returns a special
 procedure when called by any reading procedure other than
 @racket[read], @racket[read-syntax], @racket[read-char-or-special],
 @racket[peek-char-or-special], @racket[read-byte-or-special], or
 @racket[peek-byte-or-special], then the @exnraise[exn:fail:contract].}

@(begin
#reader scribble/comment-reader
[examples
;; A port with no input...
;; Easy: @racket[(open-input-bytes #"")]
;; Hard:
(define /dev/null-in 
  (make-input-port 'null
                   (lambda (s) eof)
                   (lambda (skip s progress-evt) eof)
                   void
                   (lambda () never-evt)
                   (lambda (k progress-evt done-evt)
                     (error "no successful peeks!"))))
(read-char /dev/null-in)
(peek-char /dev/null-in)
(read-byte-or-special /dev/null-in)
(peek-byte-or-special /dev/null-in 100)

;; A port that produces a stream of 1s:
(define infinite-ones 
  (make-input-port
   'ones
   (lambda (s) 
     (bytes-set! s 0 (char->integer #\1)) 1)
   #f
   void))
(read-string 5 infinite-ones)

;; But we can't peek ahead arbitrarily far, because the
;; automatic peek must record the skipped bytes, so
;; we'd run out of memory.

;; An infinite stream of 1s with a specific peek procedure:
(define infinite-ones 
  (let ([one! (lambda (s) 
                (bytes-set! s 0 (char->integer #\1)) 1)])
    (make-input-port
     'ones
     one!
     (lambda (s skip progress-evt) (one! s))
     void)))
(read-string 5 infinite-ones)

;; Now we can peek ahead arbitrarily far:
(peek-string 5 (expt 2 5000) infinite-ones)

;; The port doesn't supply procedures to implement progress events:
(port-provides-progress-evts? infinite-ones)
(port-progress-evt infinite-ones)

;; Non-byte port results:
(define infinite-voids
  (make-input-port
   'voids
   (lambda (s) (lambda args 'void))
   (lambda (skip s evt) (lambda args 'void))
   void))
(read-char infinite-voids)
(read-char-or-special infinite-voids)

;; This port produces 0, 1, 2, 0, 1, 2, etc., but it is not
;; thread-safe, because multiple threads might read and change @racket[n].
(define mod3-cycle/one-thread
  (let* ([n 2]
         [mod! (lambda (s delta)
                 (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
                 1)])
    (make-input-port
     'mod3-cycle/not-thread-safe
     (lambda (s) 
       (set! n (modulo (add1 n) 3))
       (mod! s 0))
     (lambda (s skip evt) 
       (mod! s skip))
     void)))
(read-string 5 mod3-cycle/one-thread)
(peek-string 5 (expt 2 5000) mod3-cycle/one-thread)

;; Same thing, but thread-safe and kill-safe, and with progress
;; events. Only the server thread touches the stateful part
;; directly. (See the output port examples for a simpler thread-safe
;; example, but this one is more general.)
(define (make-mod3-cycle)
  (define read-req-ch (make-channel))
  (define peek-req-ch (make-channel))
  (define progress-req-ch (make-channel))
  (define commit-req-ch (make-channel))
  (define close-req-ch (make-channel))
  (define closed? #f)
  (define n 0)
  (define progress-sema #f)
  (define (mod! s delta)
    (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
    1)
  ;; ----------------------------------------
  ;; The server has a list of outstanding commit requests,
  ;;  and it also must service each port operation (read, 
  ;;  progress-evt, etc.)
  (define (serve commit-reqs response-evts)
    (apply
     sync
     (handle-evt read-req-ch
                 (handle-read commit-reqs response-evts))
     (handle-evt progress-req-ch
                 (handle-progress commit-reqs response-evts))
     (handle-evt commit-req-ch
                 (add-commit commit-reqs response-evts))
     (handle-evt close-req-ch
                 (handle-close commit-reqs response-evts))
     (append
      (map (make-handle-response commit-reqs response-evts)
           response-evts)
      (map (make-handle-commit commit-reqs response-evts)
           commit-reqs))))
  ;; Read/peek request: fill in the string and commit
  (define ((handle-read commit-reqs response-evts) r)
    (let ([s (car r)]
          [skip (cadr r)]
          [ch (caddr r)]
          [nack (cadddr r)]
          [evt (car (cddddr r))]
          [peek? (cdr (cddddr r))])
      (let ([fail? (and evt
                        (sync/timeout 0 evt))])
        (unless (or closed? fail?)
          (mod! s skip)
          (unless peek?
            (commit! 1)))
        ;; Add an event to respond:
        (serve commit-reqs
               (cons (choice-evt 
                      nack
                      (channel-put-evt ch (if closed? 
                                              0 
                                              (if fail? #f 1))))
                     response-evts)))))
  ;; Progress request: send a peek evt for the current 
  ;;  progress-sema
  (define ((handle-progress commit-reqs response-evts) r)
    (let ([ch (car r)]
          [nack (cdr r)])
      (unless progress-sema
        (set! progress-sema (make-semaphore (if closed? 1 0))))
      ;; Add an event to respond:
      (serve commit-reqs
             (cons (choice-evt 
                    nack
                    (channel-put-evt
                     ch
                     (semaphore-peek-evt progress-sema)))
                   response-evts))))
  ;; Commit request: add the request to the list
  (define ((add-commit commit-reqs response-evts) r)
    (serve (cons r commit-reqs) response-evts))
  ;; Commit handling: watch out for progress, in which case
  ;;  the response is a commit failure; otherwise, try
  ;;  to sync for a commit. In either event, remove the
  ;;  request from the list
  (define ((make-handle-commit commit-reqs response-evts) r)
    (let ([k (car r)]
          [progress-evt (cadr r)]
          [done-evt (caddr r)]
          [ch (cadddr r)]
          [nack (cddddr r)])
      ;; Note: we don't check that k is @racket[<=] the sum of
      ;;  previous peeks, because the entire stream is actually
      ;;  known, but we could send an exception in that case.
      (choice-evt
       (handle-evt progress-evt
                   (lambda (x) 
                     (sync nack (channel-put-evt ch #f))
                     (serve (remq r commit-reqs) response-evts)))
       ;; Only create an event to satisfy done-evt if progress-evt
       ;;  isn't already ready.
       ;; Afterward, if progress-evt becomes ready, then this
       ;;  event-making function will be called again, because
       ;;  the server controls all posts to progress-evt.
       (if (sync/timeout 0 progress-evt)
           never-evt
           (handle-evt done-evt
                       (lambda (v)
                         (commit! k)
                         (sync nack (channel-put-evt ch #t))
                         (serve (remq r commit-reqs)
                                response-evts)))))))
  ;; Response handling: as soon as the respondee listens,
  ;;  remove the response
  (define ((make-handle-response commit-reqs response-evts) evt)
    (handle-evt evt
                (lambda (x)
                  (serve commit-reqs
                         (remq evt response-evts)))))
  ;; Close handling: post the progress sema, if any, and set
  ;;   the @racket[closed?] flag
  (define ((handle-close commit-reqs response-evts) r)
    (let ([ch (car r)]
          [nack (cdr r)])
      (set! closed? #t)
      (when progress-sema
        (semaphore-post progress-sema))
      (serve commit-reqs
             (cons (choice-evt nack
                               (channel-put-evt ch (void)))
                   response-evts))))
  ;; Helper for reads and post-peek commits:
  (define (commit! k)
    (when progress-sema
      (semaphore-post progress-sema)
      (set! progress-sema #f))
    (set! n (+ n k)))
  ;; Start the server thread:
  (define server-thread (thread (lambda () (serve null null))))
  ;; ----------------------------------------
  ;; Client-side helpers:
  (define (req-evt f)
    (nack-guard-evt
     (lambda (nack)
       ;; Be sure that the server thread is running:
       (thread-resume server-thread (current-thread))
       ;; Create a channel to hold the reply:
       (let ([ch (make-channel)])
         (f ch nack)
         ch))))
  (define (read-or-peek-evt s skip evt peek?)
    (req-evt (lambda (ch nack)
               (channel-put read-req-ch
                            (list* s skip ch nack evt peek?)))))
  ;; Make the port:
  (make-input-port 'mod3-cycle
                   ;; Each handler for the port just sends
                   ;;  a request to the server
                   (lambda (s) (read-or-peek-evt s 0 #f #f))
                   (lambda (s skip evt) 
                     (read-or-peek-evt s skip evt #t))
                   (lambda () ; close
                     (sync (req-evt
                            (lambda (ch nack)
                              (channel-put progress-req-ch
                                           (list* ch nack))))))
                   (lambda () ; progress-evt
                     (sync (req-evt
                            (lambda (ch nack)
                              (channel-put progress-req-ch
                                           (list* ch nack))))))
                   (lambda (k progress-evt done-evt)  ; commit
                     (sync (req-evt
                            (lambda (ch nack)
                              (channel-put 
                               commit-req-ch
                               (list* k progress-evt done-evt ch 
                                      nack))))))))

(define mod3-cycle (make-mod3-cycle))
(let ([result1 #f]
      [result2 #f])
  (let ([t1 (thread 
             (lambda ()
               (set! result1 (read-string 5 mod3-cycle))))]
        [t2 (thread
             (lambda ()
               (set! result2 (read-string 5 mod3-cycle))))])
    (thread-wait t1)
    (thread-wait t2)
    (string-append result1 "," result2)))

(define s (make-bytes 1))
(define progress-evt (port-progress-evt mod3-cycle))
(peek-bytes-avail! s 0 progress-evt mod3-cycle)
s
(port-commit-peeked 1 progress-evt (make-semaphore 1)
                    mod3-cycle)
(sync/timeout 0 progress-evt)
(peek-bytes-avail! s 0 progress-evt mod3-cycle)
(port-commit-peeked 1 progress-evt (make-semaphore 1) 
                    mod3-cycle)
(close-input-port mod3-cycle)
])

@;------------------------------------------------------------------------
@;------------------------------------------------------------------------

@defproc[(make-output-port [name any/c]
                           [evt evt?]
                           [write-out (or/c
                                       (bytes? exact-nonnegative-integer?
                                               exact-nonnegative-integer?
                                               boolean?
                                               boolean?
                                               . -> .
                                               (or/c exact-nonnegative-integer?
                                                     #f
                                                     evt?))
                                       output-port?)]
                           [close (-> any)]
                           [write-out-special (or/c (any/c boolean? boolean?
                                                           . -> .
                                                           (or/c any/c
                                                                 #f
                                                                 evt?))
                                                    output-port?
                                                    #f)
                                              #f]
                           [get-write-evt (or/c
                                           (bytes? exact-nonnegative-integer?
                                                   exact-nonnegative-integer?
                                                   . -> .
                                                   evt?)
                                           #f)
                                          #f]
                           [get-write-special-evt (or/c 
                                                   (any/c . -> . evt?)
                                                   #f)
                                                  #f]
                           [get-location (or/c 
                                          (->
                                           (values (or/c exact-positive-integer? #f)
                                                   (or/c exact-nonnegative-integer? #f)
                                                   (or/c exact-positive-integer? #f)))
                                          #f)
                                         #f]
                           [count-lines! (-> any) void]
                           [init-position (or/c exact-positive-integer?
                                                port?
                                                #f
                                                (-> (or/c exact-positive-integer? #f)))
                                          1]
                           [buffer-mode (or/c (case-> 
                                               ((or/c 'block 'line 'none) . -> . any)
                                               (-> (or/c 'block 'line 'none #f)))
                                              #f)
                                        #f])
          output-port?]{

Creates an output port, which is immediately open for
writing. If @racket[close] procedure has no side effects, then
the port need not be explicitly closed. The port can buffer data
within its @racket[write-out] and @racket[write-out-special]
procedures.

 @itemize[

   @item{@racket[name] --- the name for the output port.}

   @item{@racket[evt] --- a synchronization event (see @secref["sync"];
    e.g., a semaphore or another port). The event is used in place of
    the output port when the port is supplied to synchronization
    procedures like @racket[sync].  Thus, the event should be
    unblocked when the port is ready for writing at least one byte
    without blocking, or ready to make progress in flushing an
    internal buffer without blocking. The event must not unblock
    unless the port is ready for writing; otherwise, the guarantees of
    @racket[sync] will be broken for the output port. Use
    @racket[always-evt] if writes to the port always succeed without
    blocking.}

   @item{@racket[write-out] --- either an output port, which indicates that
         writes should be redirected to the given port, or a procedure
         of five arguments:

     @itemize[

     @item{an immutable byte string containing bytes to write;}

     @item{a non-negative exact integer for a starting offset
     (inclusive) into the byte string;}

     @item{a non-negative exact integer for an ending offset
     (exclusive) into the byte string;}

     @item{a boolean; @racket[#f] indicates that the port is allowed
     to keep the written bytes in a buffer, and that it is
     allowed to block indefinitely; @racket[#t] indicates that the
     write should not block, and that the port should attempt to flush
     its buffer and completely write new bytes instead of
     buffering them;}

     @item{a boolean; @racket[#t] indicates that if the port blocks
     for a write, then it should enable breaks while blocking (e.g.,
     using @racket[sync/enable-break]); this argument is always
     @racket[#f] if the fourth argument is @racket[#t].}

     ]

    The procedure returns one of the following:

     @itemize[

     @item{a non-negative exact integer representing the number of
     bytes written or buffered;}

     @item{@racket[#f] if no bytes could be written, perhaps because
     the internal buffer could not be completely flushed;}

     @item{a @techlink{pipe} output port (when buffering is allowed
     and not when flushing) for buffering bytes as long as the pipe is
     not full and until @racket[write-out] or
     @racket[write-out-special] is called; or}

     @item{a synchronizable event (see @secref["sync"]) other than a
     pipe output port that acts like the result of
     @racket[write-bytes-avail-evt] to complete the write.}

     ]

    Since @racket[write-out] can produce an event, an acceptable
    implementation of @racket[write-out] is to pass its first three
    arguments to the port's @racket[get-write-evt]. Some port
    implementors, however, may choose not to provide
    @racket[get-write-evt] (perhaps because writes cannot be
    made atomic), or may implement @racket[write-proc] to
    enable a fast path for non-blocking writes or to
    enable buffering.

    From a user's perspective, the difference between buffered and
    completely written data is (1) buffered data can be lost in the
    future due to a failed write, and (2) @racket[flush-output] forces
    all buffered data to be completely written. Under no circumstances
    is buffering required.
    
    If the start and end indices are the same, then the fourth
    argument to @racket[write-out] will be @racket[#f], and the write
    request is actually a flush request for the port's buffer (if
    any), and the result should be @racket[0] for a successful flush
    (or if there is no buffer).

    The result should never be @racket[0] if the start and end indices
    are different, otherwise the @exnraise[exn:fail:contract].
    Similarly, the @exnraise[exn:fail:contract] if @racket[write-out]
    returns a pipe output port when buffering is disallowed or when it
    is called for flushing.  If a returned integer is larger than the
    supplied byte-string range, the @exnraise[exn:fail:contract].

    The @racket[#f] result should be avoided, unless the next write
    attempt is likely to work. Otherwise, if data cannot be written,
    return an event instead.

    An event returned by @racket[write-out] can return @racket[#f] or
    another event like itself, in contrast to events produced by
    @racket[write-bytes-avail-evt] or @racket[get-write-evt].
    A writing process loops with @racket[sync] until it obtains a
    non-event result.

    The @racket[write-out] procedure is always called with breaks
    disabled, independent of whether breaks were enabled when the write
    was requested by a client of the port. If breaks were enabled for
    a blocking operation, then the fifth argument to @racket[write-out]
    will be @racket[#t], which indicates that @racket[write-out] should
    re-enable breaks while blocking.

    If the writing procedure raises an exception, due to write
    or commit operations, it must not have committed any bytes
    (though it may have committed previously buffered bytes).

    A port's writing procedure may be called in multiple threads
    simultaneously (if the port is accessible in multiple
    threads). The port is responsible for its own internal
    synchronization. Note that improper implementation of such
    synchronization mechanisms might cause a non-blocking write
    procedure to block.}

  @item{@racket[close] --- a procedure of zero arguments that is
    called to close the port. The port is not considered closed until
    the closing procedure returns. The port's procedures will never be
    used again via the port after it is closed. However, the closing
    procedure can be called simultaneously in multiple threads (if the
    port is accessible in multiple threads), and it may be called
    during a call to the other procedures in another thread; in the
    latter case, any outstanding writes or flushes should be
    terminated immediately with an error.}

  @item{@racket[write-out-special] --- either @racket[#f] (the
    default), an output port (which indicates that
    special writes should be redirected to the given port),
    or a procedure to handle @racket[write-special] calls
    for the port. If @racket[#f], then the port does not support
    special output, and @racket[port-writes-special?] will return
    @racket[#f] when applied to the port.

    If a procedure is supplied, it takes three arguments: the special
    value to write, a boolean that is @racket[#f] if the procedure can
    buffer the special value and block indefinitely, and a boolean
    that is @racket[#t] if the procedure should enable breaks while
    blocking. The result is one of the following:

     @itemize[

     @item{a non-event true value, which indicates that the special is
      written;}

     @item{@racket[#f] if the special could not be written, perhaps
     because an internal buffer could not be completely flushed;}

     @item{a synchronizable event (see @secref["sync"]) that acts like
     the result of @racket[get-write-special-evt] to complete the write.}

     ]

    Since @racket[write-out-special] can return an event,
    passing the first argument to an implementation of
    @racket[get-write-special-evt] is acceptable as a
    @racket[write-out-special].

    As for @racket[write-out], the @racket[#f] result is discouraged,
    since it can lead to busy waiting. Also as for @racket[write-out],
    an event produced by @racket[write-out-special] is allowed
    to produce @racket[#f] or another event like itself.  The
    @racket[write-out-special] procedure is always called with
    breaks disabled, independent of whether breaks were enabled when
    the write was requested by a client of the port.}

   @item{@racket[get-write-evt] --- either @racket[#f] (the
   default) or a procedure of three arguments:

     @itemize[

     @item{an immutable byte string containing bytes to write;}

     @item{a non-negative exact integer for a starting offset
     (inclusive) into the byte string; and}

     @item{a non-negative exact integer for an ending offset
     (exclusive) into the byte string.}

     ]

    The result is a synchronizable event (see @secref["sync"]) to act as
    the result of @racket[write-bytes-avail-evt] for the port (i.e.,
    to complete a write or flush), which becomes available only as
    data is committed to the port's underlying device, and whose
    result is the number of bytes written.

    If @racket[get-write-evt] is @racket[#f], then
    @racket[port-writes-atomic?] will produce @racket[#f] when applied
    to the port, and the port will not be a valid argument to
    procedures such as @racket[write-bytes-avail-evt].
    Otherwise, an event returned by @racket[get-write-evt] must
    not cause data to be written to the port unless the event is
    chosen in a synchronization, and it must write to the port if the
    event is chosen (i.e., the write must appear atomic with respect
    to the synchronization).

    If the event's result integer is larger than the supplied
    byte-string range, the @exnraise[exn:fail:contract] by a wrapper
    on the event. If the start and end indices are the same (i.e., no
    bytes are to be written), then the event should produce @racket[0]
    when the buffer is completely flushed. (If the port has no buffer,
    then it is effectively always flushed.)

    If the event raises an exception, due to write or commit
    operations, it must not have committed any new bytes (though it
    may have committed previously buffered bytes).

    Naturally, a port's events may be used in multiple threads
    simultaneously (if the port is accessible in multiple
    threads). The port is responsible for its own internal
    synchronization.}

  @item{@racket[get-write-special-evt] --- either @racket[#f]
    (the default), or a procedure to handle @racket[write-special-evt]
    calls for the port. This argument must be @racket[#f] if either
    @racket[write-out-special] or @racket[get-write-evt]
    is @racket[#f], and it must be a procedure if both of those
    arguments are procedures.

    If it is a procedure, it takes one argument: the special value to
    write. The resulting event (with its constraints) is analogous to
    the result of @racket[get-write-evt].

    If the event raises an exception, due to write or commit
    operations, it must not have committed the special value (though
    it may have committed previously buffered bytes and values).}



  @item{@racket[get-location] --- either @racket[#f] (the
    default), or a procedure that takes no arguments and returns three
    values: the line number for the next item written to the port's
    stream (a positive number or @racket[#f]), the column number for
    the next item written to port's stream (a non-negative number or
    @racket[#f]), and the position for the next item written to port's
    stream (a positive number or @racket[#f]). See also
    @secref["linecol"].

    This procedure is called to implement @racket[port-next-location]
    for the port, but only if line counting is enabled for the port
    via @racket[port-count-lines!] (in which case
    @racket[count-lines!] is called).}

  @item{@racket[count-lines!] --- a procedure of no arguments
    that is called if and when line counting is enabled for the port.
    The default procedure is @racket[void].}

  @item{@racket[init-position] --- normally an exact, positive integer
    that determines the position of the port's first item, which is
    used by @racket[file-position] or when line counting is
    @italic{not} enabled for the port. The default is @racket[1].  If
    @racket[init-position] is @racket[#f], the port is treated as
    having an unknown position. If @racket[init-position] is a port,
    then the given port's position is always used for the new port's
    position. If @racket[init-position] is a procedure, it is called
    as needed to obtain the port's position.}

  @item{@racket[buffer-mode] --- either @racket[#f] (the
    default) or a procedure that accepts zero or one arguments. If
    @racket[buffer-mode] is @racket[#f], then the resulting
    port does not support a buffer-mode setting. Otherwise, the
    procedure is called with one symbol argument (@racket['block],
    @racket['line], or @racket['none]) to set the buffer mode, and it is
    called with zero arguments to get the current buffer mode. In the
    latter case, the result must be @racket['block], @racket['line],
    @racket['none], or @racket[#f] (unknown). See @secref["port-buffers"]
    for more information on buffer modes.}

 ]
}

@(begin
#reader scribble/comment-reader
[examples
;; A port that writes anything to nowhere:
(define /dev/null-out
  (make-output-port 
   'null
   always-evt
   (lambda (s start end non-block? breakable?) (- end start))
   void
   (lambda (special non-block? breakable?) #t)
   (lambda (s start end) (wrap-evt
                          always-evt
                          (lambda (x)
                            (- end start))))
   (lambda (special) always-evt)))
(display "hello" /dev/null-out)
(write-bytes-avail #"hello" /dev/null-out)
(write-special 'hello /dev/null-out)
(sync (write-bytes-avail-evt #"hello" /dev/null-out))

;; A port that accumulates bytes as characters in a list,
;;  but not in a thread-safe way:
(define accum-list null)
(define accumulator/not-thread-safe
  (make-output-port 
   'accum/not-thread-safe
   always-evt
   (lambda (s start end non-block? breakable?)
     (set! accum-list
           (append accum-list
                   (map integer->char
                        (bytes->list (subbytes s start end)))))
     (- end start))
   void))
(display "hello" accumulator/not-thread-safe)
accum-list

;; Same as before, but with simple thread-safety:
(define accum-list null)
(define accumulator 
  (let* ([lock (make-semaphore 1)]
         [lock-peek-evt (semaphore-peek-evt lock)])
    (make-output-port
     'accum
     lock-peek-evt
     (lambda (s start end non-block? breakable?)
       (if (semaphore-try-wait? lock)
           (begin
             (set! accum-list
                   (append accum-list
                           (map integer->char
                                (bytes->list
                                 (subbytes s start end)))))
             (semaphore-post lock)
             (- end start))
           ;; Cheap strategy: block until the list is unlocked,
           ;;   then return 0, so we get called again
           (wrap-evt
            lock-peek
            (lambda (x) 0))))
     void)))
(display "hello" accumulator)
accum-list

;; A port that transforms data before sending it on
;;  to another port. Atomic writes exploit the
;;  underlying port's ability for atomic writes.
(define (make-latin-1-capitalize port)
  (define (byte-upcase s start end)
    (list->bytes
     (map (lambda (b) (char->integer
                       (char-upcase
                        (integer->char b))))
          (bytes->list (subbytes s start end)))))
  (make-output-port
   'byte-upcase
   ;; This port is ready when the original is ready:
   port
   ;; Writing procedure:
   (lambda (s start end non-block? breakable?)
     (let ([s (byte-upcase s start end)])
       (if non-block?
           (write-bytes-avail* s port)
           (begin
             (display s port)
             (bytes-length s)))))
   ;; Close procedure --- close original port:
   (lambda () (close-output-port port))
   #f
   ;; Write event:
   (and (port-writes-atomic? port)
        (lambda (s start end)
          (write-bytes-avail-evt
           (byte-upcase s start end) 
           port)))))
(define orig-port (open-output-string))
(define cap-port (make-latin-1-capitalize orig-port))
(display "Hello" cap-port)
(get-output-string orig-port)
(sync (write-bytes-avail-evt #"Bye" cap-port))
(get-output-string orig-port)
])
