#lang scribble/doc
@(require scribble/manual
          scribble/eval 
          "guide-utils.rkt"
          (for-label racket))

@(define concurrency-eval (make-base-eval))

@(define reference-doc '(lib "scribblings/reference/reference.scrbl"))

@title[#:tag "concurrency"]{Concurrency and Synchronization}

Racket provides @deftech{concurrency} in the form of
@deftech{threads}, and it provides a general @racket[sync] function
that can be used to synchronize both threads and other implicit forms of
concurrency, such as @tech{ports}.

Threads run concurrently in the sense that one thread can preempt
another without its cooperation, but threads do not run in parallel in
the sense of using multiple hardware processors.  See
@secref["parallelism"] for information on parallelism in Racket.

@section{Threads}

To execute a procedure concurrently, use @racket[thread].  The
following example creates two new threads from the main thread:

@racketblock[
(displayln "This is the original thread")
(thread (lambda () (displayln "This is a new thread.")))
(thread (lambda () (displayln "This is another new thread.")))
]

The next example creates a new thread that would otherwise loop forever, but
the main thread uses @racket[sleep] to pause itself for 2.5 seconds, then
uses @racket[kill-thread] to terminate the worker thread:

@racketblock[
(define worker (thread (lambda ()
                         (let loop ()
                           (displayln "Working...")
                           (sleep 0.2)
                           (loop)))))
(sleep 2.5)
(kill-thread worker)
]

@margin-note{In DrRacket, the main thread keeps going until the Stop button is
clicked, so in DrRacket the @racket[thread-wait] is not necessary.}

If the main thread finishes or is killed, the application exits, even if
other threads are still running.  A thread can use @racket[thread-wait] to
wait for another thread to finish.  Here, the main thread uses
@racket[thread-wait] to make sure the worker thread finishes before the main
thread exits:

@racketblock[
(define worker (thread
                 (lambda ()
                   (for ([i 100])
                     (printf "Working hard... ~a~n" i)))))
(thread-wait worker)
(displayln "Worker finished")
]

@section{Thread Mailboxes}

Each thread has a mailbox for receiving messages.  The @racket[thread-send] function
asynchronously sends a message to another thread's mailbox, while
@racket[thread-receive] returns the oldest message from the current
thread's mailbox, blocking to wait for a message if necessary.  In the
following example, the main thread sends data to the worker thread to be
processed, then sends a @racket['done] message when there is no more data and
waits for the worker thread to finish.

@racketblock[
(define worker-thread (thread
                       (lambda ()
                         (let loop ()
                           (match (thread-receive)
                             [(? number? num)
                              (printf "Processing ~a~n" num)
                              (loop)]
                             ['done
                              (printf "Done~n")])))))
(for ([i 20])
  (thread-send worker-thread i))
(thread-send worker-thread 'done)
(thread-wait worker-thread)
]

In the next example, the main thread delegates work to multiple arithmetic
threads, then waits to receive the results.  The arithmetic threads process work
items then send the results to the main thread.

@racketblock[
(define (make-arithmetic-thread operation)
  (thread (lambda ()
            (let loop ()
              (match (thread-receive)
                [(list oper1 oper2 result-thread)
                 (thread-send result-thread
                              (format "~a + ~a = ~a"
                                      oper1
                                      oper2
                                      (operation oper1 oper2)))
                 (loop)])))))

(define addition-thread (make-arithmetic-thread +))
(define subtraction-thread (make-arithmetic-thread -))

(define worklist '((+ 1 1) (+ 2 2) (- 3 2) (- 4 1)))
(for ([item worklist])
  (match item
    [(list '+ o1 o2)
     (thread-send addition-thread
                  (list o1 o2 (current-thread)))]
    [(list '- o1 o2)
     (thread-send subtraction-thread
                  (list o1 o2 (current-thread)))]))

(for ([i (length worklist)])
  (displayln (thread-receive)))
]

@section{Semaphores}

Semaphores facilitate synchronized access to an arbitrary shared resource.
Use semaphores when multiple threads must perform non-atomic operations on a
single resource.

In the following example, multiple threads print to standard output
concurrently.  Without synchronization, a line printed by one thread might
appear in the middle of a line printed by another thread.  By using a semaphore
initialized with a count of @racket[1], only one thread will print at a time.
The @racket[semaphore-wait] function blocks until the semaphore's internal counter is
non-zero, then decrements the counter and returns. The @racket[semaphore-post] function
increments the counter so that another thread can unblock and then print.

@racketblock[
(define output-semaphore (make-semaphore 1))
(define (make-thread name)
  (thread (lambda ()
            (for [(i 10)]
              (semaphore-wait output-semaphore)
              (printf "thread ~a: ~a~n" name i)
              (semaphore-post output-semaphore)))))
(define threads
  (map make-thread '(A B C)))
(for-each thread-wait threads)
]

The pattern of waiting on a semaphore, working, and posting to the
semaphore can also be expressed using
@racket[call-with-semaphore],which has the advantage of posting to the
semaphore if control escapes (e.g., due to an exception):

@racketblock[
(define output-semaphore (make-semaphore 1))
(define (make-thread name)
  (thread (lambda ()
            (for [(i 10)]
              (call-with-semaphore
               output-semaphore
               (lambda ()
                (printf "thread ~a: ~a~n" name i)))))))
(define threads
  (map make-thread '(A B C)))
(for-each thread-wait threads)
]

Semaphores are a low-level technique.  Often, a better solution is to restrict
resource access to a single thread.  For example, synchronizing access to
standard output might be better accomplished by having a dedicated thread for
printing output.

@section{Channels}

Channels synchronize two threads while a value is passed from one thread to the
other.  Unlike a thread mailbox, multiple threads can get items from a single
channel, so channels should be used when multiple threads need to consume items
from a single work queue.

In the following example, the main thread adds items to a channel using
@racket[channel-put], while multiple worker threads consume those items using
@racket[channel-get].  Each call to either procedure blocks until another
thread calls the other procedure with the same channel.  The workers process
the items and then pass their results to the result thread via the @racket[result-channel].

@racketblock[
(define result-channel (make-channel))
(define result-thread
        (thread (lambda ()
                  (let loop ()
                    (displayln (channel-get result-channel))
                    (loop)))))

(define work-channel (make-channel))
(define (make-worker thread-id)
  (thread
   (lambda ()
     (let loop ()
       (define item (channel-get work-channel))
       (case item
         [(DONE)
          (channel-put result-channel
                       (format "Thread ~a done" thread-id))]
         [else
          (channel-put result-channel
                       (format "Thread ~a processed ~a"
                               thread-id
                               item))
          (loop)])))))
(define work-threads (map make-worker '(1 2)))
(for ([item '(A B C D E F G H DONE DONE)])
  (channel-put work-channel item))
(for-each thread-wait work-threads)
]

@section{Buffered Asynchronous Channels}

Buffered asynchronous channels are similar to the channels described above, but
the ``put'' operation of asynchronous channels does not block---unless the given
channel was created with a buffer limit and the limit has been reached.  The
asynchronous-put operation is therefore somewhat similar to
@racket[thread-send], but unlike thread mailboxes, asynchronous channels allow
multiple threads to consume items from a single channel.

In the following
example, the main thread adds items to the work channel, which holds a maximum
of three items at a time.  The worker threads process items from this channel and
then send results to the print thread.

@racketblock[
(require racket/async-channel)

(define print-thread
  (thread (lambda ()
            (let loop ()
              (displayln (thread-receive))
              (loop)))))
(define (safer-printf . items)
  (thread-send print-thread
               (apply format items)))

(define work-channel (make-async-channel 3))
(define (make-worker-thread thread-id)
  (thread
   (lambda ()
     (let loop ()
       (define item (async-channel-get work-channel))
       (safer-printf "Thread ~a processing item: ~a" thread-id item)
       (loop)))))

(for-each make-worker-thread '(1 2 3))
(for ([item '(a b c d e f g h i j k l m)])
  (async-channel-put work-channel item))
]

Note the above example lacks any synchronization to verify that all items were
processed.  If the main thread were to exit without such synchronization, it is
possible that the worker threads will not finish processing some items or the
print thread will not print all items.

@section{Synchronizable Events and @racket[sync]}

There are other ways to synchronize threads.  The @racket[sync] function allows
threads to coordinate via @tech[#:doc reference-doc]{synchronizable events}.
Many values double as events, allowing a uniform way to synchronize threads
using different types.  Examples of events include channels, ports, threads,
and alarms.

In the next example, a channel and an alarm are used as synchronizable events.
The workers @racket[sync] on both so that they can process channel items until the
alarm is activated.  The channel items are processed, and then results are sent back
to the main thread.

@racketblock[
(define main-thread (current-thread))
(define alarm (alarm-evt (+ 3000 (current-inexact-milliseconds))))
(define channel (make-channel))
(define (make-worker-thread thread-id)
  (thread
   (lambda ()
     (define evt (sync channel alarm))
     (cond
       [(equal? evt alarm)
        (thread-send main-thread 'alarm)]
       [else
        (thread-send main-thread
                     (format "Thread ~a received ~a"
                             thread-id
                             evt))]))))
(make-worker-thread 1)
(make-worker-thread 2)
(make-worker-thread 3)
(channel-put channel 'A)
(channel-put channel 'B)
(let loop ()
  (match (thread-receive)
    ['alarm
     (displayln "Done")]
    [result
     (displayln result)
     (loop)]))
]

The next example shows a function for use in a simple TCP echo server.  The
function uses @racket[sync/timeout] to synchronize on input from the given port
or a message in the thread's mailbox.  The first argument to @racket[sync/timeout]
specifies the maximum number of seconds it should wait on the given events. The
@racket[read-line-evt] function returns an event that is ready when a line of input is
available in the given input port.  The result of @racket[thread-receive-evt] is ready when
@racket[thread-receive] would not block.  In a real application, the messages
received in the thread mailbox could be used for control messages, etc.

@racketblock[
(define (serve in-port out-port)
  (let loop []
    (define evt (sync/timeout 2
                              (read-line-evt in-port 'any)
                              (thread-receive-evt)))
    (cond
      [(not evt)
       (displayln "Timed out, exiting")
       (tcp-abandon-port in-port)
       (tcp-abandon-port out-port)]
      [(string? evt)
       (fprintf out-port "~a~n" evt)
       (flush-output out-port)
       (loop)]
      [else
       (printf "Received a message in mailbox: ~a~n"
               (thread-receive))
       (loop)])))
]

The @racket[serve] function is used in the following example, which
starts a server thread and a client thread that communicate over TCP.  The
client prints three lines to the server, which echoes them back.  The client's
@racket[copy-port] call blocks until EOF is received.  The server times out after
two seconds, closing the ports, which allows @racket[copy-port] to finish and the
client to exit.  The main thread uses @racket[thread-wait] to wait for the
client thread to exit (since, without @racket[thread-wait], the main thread might
exit before the other threads are finished).

@racketblock[
(define port-num 4321)
(define (start-server)
  (define listener (tcp-listen port-num))
  (thread
    (lambda ()
      (define-values [in-port out-port] (tcp-accept listener))
      (serve in-port out-port))))

(start-server)

(define client-thread
  (thread
   (lambda ()
     (define-values [in-port out-port] (tcp-connect "localhost" port-num))
     (display "first\nsecond\nthird\n" out-port)
     (flush-output out-port)
     (code:comment "copy-port will block until EOF is read from in-port")
     (copy-port in-port (current-output-port)))))

(thread-wait client-thread)
]

Sometimes, you want to attach result behavior directly to the event passed to
@racket[sync].  In the following example, the worker thread synchronizes on three
channels, but each channel must be handled differently.  Using
@racket[handle-evt] associates a callback with the given event.  When
@racket[sync] selects the given event, it calls the callback to generate the
synchronization result, rather than using the event's normal synchronization
result.  Since the event is handled in the callback, there is no need to
dispatch on the return value of @racket[sync].

@racketblock[
(define add-channel (make-channel))
(define multiply-channel (make-channel))
(define append-channel (make-channel))

(define (work)
  (let loop ()
    (sync (handle-evt add-channel
                      (lambda (list-of-numbers)
                        (printf "Sum of ~a is ~a~n"
                                list-of-numbers
                                (apply + list-of-numbers))))
          (handle-evt multiply-channel
                      (lambda (list-of-numbers)
                        (printf "Product of ~a is ~a~n"
                                list-of-numbers
                                (apply * list-of-numbers))))
          (handle-evt append-channel
                      (lambda (list-of-strings)
                        (printf "Concatenation of ~s is ~s~n"
                                list-of-strings
                                (apply string-append list-of-strings)))))
    (loop)))

(define worker (thread work))
(channel-put add-channel '(1 2))
(channel-put multiply-channel '(3 4))
(channel-put multiply-channel '(5 6))
(channel-put add-channel '(7 8))
(channel-put append-channel '("a" "b"))
]

The result of @racket[handle-evt] invokes its callback in tail position
with respect to @racket[sync], so it is safe to
use recursion as in the following example.

@racketblock[
(define control-channel (make-channel))
(define add-channel (make-channel))
(define subtract-channel (make-channel))
(define (work state)
  (printf "Current state: ~a~n" state)
  (sync (handle-evt add-channel
                    (lambda (number)
                      (printf "Adding: ~a~n" number)
                      (work (+ state number))))
        (handle-evt subtract-channel
                    (lambda (number)
                      (printf "Subtracting: ~a~n" number)
                      (work (- state number))))
        (handle-evt control-channel
                    (lambda (kill-message)
                      (printf "Done~n")))))

(define worker (thread (lambda () (work 0))))
(channel-put add-channel 2)
(channel-put subtract-channel 3)
(channel-put add-channel 4)
(channel-put add-channel 5)
(channel-put subtract-channel 1)
(channel-put control-channel 'done)
(thread-wait worker)
]

The @racket[wrap-evt] function is like @racket[handle-evt], except
that its handler is not called in tail position with respect to
@racket[sync]. At the same time, @racket[wrap-evt] disables break
exceptions during its handler's invocation.

