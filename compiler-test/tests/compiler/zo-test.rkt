#!/bin/sh
#|
exec racket -t "$0" -- -s -t 60 -v -R $*
|#

#lang racket
(require setup/dirs
         racket/runtime-path
         racket/future
         compiler/find-exe
         "zo-test-util.rkt")

(define ((make-recorder! ht) file phase)
  (hash-update! ht phase (curry list* file) empty))

(define stop-on-first-error (make-parameter #f))
(define verbose-mode (make-parameter #f))
(define care-about-nonserious? (make-parameter #t))
(define invariant-output (make-parameter #f))
(define time-limit (make-parameter +inf.0))
(define randomize (make-parameter #f))
(define num-processes (make-parameter (processor-count)))

(define errors (make-hash))
(define (record-common-error! exn-msg)
  (hash-update! errors (common-message exn-msg) add1 0))

(define (common-message exn-msg)
  (define given-messages (regexp-match #rx".*given" exn-msg))
  (if (and given-messages (not (empty? given-messages)))
      (first given-messages)
      exn-msg))

(define success-ht (make-hasheq))
(define success! (make-recorder! success-ht))
(define failure-ht (make-hasheq))
(define failure! (make-recorder! failure-ht))

(define debugging? (make-parameter #f))

(define (randomize-list l)
  (define ll (length l))
  (define seen? (make-hasheq))
  (let loop ([t 0])
    (if (= t ll)
        empty
        (let ([i (random ll)])
          (if (hash-has-key? seen? i)
              (loop t)
              (begin (hash-set! seen? i #t)
                     (list* (list-ref l i)
                            (loop (add1 t)))))))))

(define (maybe-randomize-list l)
  (if (randomize) (randomize-list l) l))

(define (for-zos ! p)
  (define p-str (if (path? p) (path->string p) p))
  (cond
    [(directory-exists? p)
     (for ([sp (in-list (maybe-randomize-list (directory-list p)))])
       (for-zos ! (build-path p sp)))]
    [(regexp-match #rx"\\.zo$" p-str)
     (! p-str)]))

(define-runtime-path zo-test-worker-path "zo-test-worker.rkt")
(define racket-path (path->string (find-exe)))

(define p
  (command-line #:program "zo-test" 
                #:once-each
                [("-D") 
                 "Enable debugging output"
                 (debugging? #t)]
                [("-s" "--stop-on-first-error") 
                 "Stop testing when first error is encountered"
                 (stop-on-first-error #t)]
                [("-S")
                 "Don't take some errors seriously"
                 (care-about-nonserious? #f)]
                [("-v" "--verbose")
                 "Display verbose error messages"
                 (verbose-mode #t)]
                [("-I")
                 "Invariant output"
                 (invariant-output #t)]
                [("-R")
                 "Randomize"
                 (randomize #t)]
                [("-t") 
                 number
                 "Limit the run to a given amount of time"
                 (time-limit (string->number number))]
                [("-j") 
                 n
                 "Run <n> in parallel"
                 (num-processes (string->number n))]
                #:args p
                (if (empty? p)
                    (list (find-collects-dir))
                    p)))

(define to-worker-ch (make-channel))
(define stop-ch (make-channel))
(define from-worker-ch (make-channel))

(define worker-threads
  (for/list ([i (in-range (num-processes))])
    (thread
     (λ ()
       (let loop ()
         (sync
          (handle-evt to-worker-ch
                      (λ (p)
                        (when (debugging?)
                          (printf "~a\n" p))
                        (define-values
                          (sp stdout stdin stderr)
                          (subprocess #f #f #f racket-path (path->string zo-test-worker-path) p))
                        (define r
                          (dynamic-wind
                           void
                           (λ ()
                             (read stdout))
                           (λ ()
                             (close-input-port stdout)
                             (close-input-port stderr)
                             (close-output-port stdin)
                             (subprocess-kill sp #t))))
                        (channel-put from-worker-ch (cons p r))
                        (loop)))
          (handle-evt stop-ch
                      (λ (die)
                        (void)))))))))          

(define (process-result p r)
  (match r
    [(success phase)
     (success! p phase)]
    [(failure phase serious? exn-msg)
     (record-common-error! exn-msg)
     (failure! p phase)
     
     (unless (and (not (care-about-nonserious?)) (not serious?))
       (when (or (verbose-mode) (stop-on-first-error))
         (eprintf "~a -- ~a: ~a\n" p phase exn-msg))
       (when (stop-on-first-error)
         (stop!)))]))

(define timing-thread
  (thread
   (λ ()
     (sync 
      (alarm-evt (+ (current-inexact-milliseconds)
                    (* 1000 (time-limit)))))
     (stop!))))

(define server-thread
  (thread
   (λ ()
     (let loop ([ts worker-threads])
       (if (empty? ts)
           (stop!)
           (apply
            sync
            (handle-evt from-worker-ch
                        (match-lambda
                          [(cons p rs)
                           (for-each (curry process-result p) rs)
                           (loop ts)]))
            (for/list ([t (in-list ts)])
              (handle-evt t (λ _ (loop (remq t ts)))))))))))

(define (spawn-worker p)
  (channel-put to-worker-ch p))

(define (zo-test paths)
  (for-each (curry for-zos spawn-worker) paths)
  
  (for ([i (in-range (processor-count))])
    (channel-put stop-ch #t)))

(define root-thread
  (thread
   (λ ()
     (zo-test p))))

(define final-sema (make-semaphore 0))
(define (stop!)
  (semaphore-post final-sema))

(define (hash-keys ht)
  (hash-map ht (λ (k v) k)))

(define final-thread
  (thread
   (λ ()
     (semaphore-wait final-sema)
     (for-each kill-thread
               (list* root-thread server-thread worker-threads))
     (unless (invariant-output)
       (newline)
       (for ([kind-name 
              (remove-duplicates 
               (append
                (hash-keys failure-ht)
                (hash-keys success-ht)))])
         (define fails (length (hash-ref failure-ht kind-name empty)))
         (define succs (length (hash-ref success-ht kind-name empty)))
         (define all (+ fails succs))
         (unless (zero? all)
           (printf "~S\n"
                   `(,kind-name
                     (#f ,fails)
                     (#t ,succs)
                     ,all))))
       (newline)
       (printf "~a tests passed\n" (length (hash-ref success-ht 'everything empty)))
       
       (let ([common-errors 
              (sort (filter (λ (p) ((car p) . > . 10))
                            (hash-map errors (λ (k v) (cons v k))))
                    > #:key car)])
         (unless (empty? common-errors)
           (printf "Common Errors:\n")
           (for ([p (in-list common-errors)])
             (printf "~a:\n~a\n\n" (car p) (cdr p)))))))))

(thread-wait final-thread)

;; Test mode:
(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "-I" "-S" "-t" "60" "-v" "-R")])
    (dynamic-require (quote-module-path "..") #f)))
