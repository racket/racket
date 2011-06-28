#lang racket
(require "../lib/pqueue.rkt"
         "../lib/db.rkt"
         "scm.rkt")

(define-syntax-rule (atomic e ...) 
  (begin e ...))

(define (main . argv)
  (define push-queue (make-parameter #f))
  (define the-db (make-parameter #f))
  (define repo (make-parameter #f))
  (define monitoring-interval (make-parameter 60))
  (command-line 
   #:program "monitor"
   #:argv argv
   #:once-each
   [("--interval") num "Monitoring interval" (monitoring-interval (string->number num))]
   [("--repo") dir "Local Git repository" (repo (string->path dir))]
   [("--pushes") dir "Persistent queue of pushes" (push-queue dir)]
   [("--db") spec "Specification of database" (the-db spec)])
  ; Setup the queue to receive push information
  (define pushes (pqueue (push-queue)))
  (pqueue-init! pushes)
  
  (define db (db-connect (the-db)))
  ; While true
  (let loop () 
    ; Read the short term database to find out what push we're at
    (define current (db-ref db "monitor" "last-push"))
    ; Update the git repository
    (git-update (repo))
    ; Check the online push counter
    (for ([new (in-list (git-pushes-after current))])
      ; Get the information about a push
      (define push-info (get-git-push (repo) new))
      (atomic
       ;  Add it to the queue
       (pqueue-enqueue! pushes push-info)
       ; Add it to the long term database
       (db-set! db push-info "push-info" new)
       ; Update the latest push in the short term database
       (db-set! db new "monitor" "last-push")))
    ; Wait
    (sleep (monitoring-interval))
    (loop))
  (db-close! db))

(provide main)
