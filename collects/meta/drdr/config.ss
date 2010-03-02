#lang scheme

(require "cache.ss"
         "dirstruct.ss"
         "svn.ss"
         "monitor-svn.ss")

(plt-directory "/opt/plt")
(drdr-directory "/opt/svn/drdr")
(svn-path "/usr/bin/svn")
(Xvfb-path "/usr/bin/Xvfb")
(current-make-install-timeout-seconds (* 60 60))
(current-make-timeout-seconds (* 60 60))                                       
(current-subprocess-timeout-seconds 90)
(current-monitoring-interval-seconds 60)
(number-of-cpus 12)

(define (string->number* s)
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (let ([v (string->number s)])
      (and (number? v)
           v))))

(define revisions #f)

(define (init-revisions!)
  (set! revisions
        (sort 
         (filter-map
          (compose string->number* path->string)
          (directory-list (plt-build-directory)))
         <)))

(define (newest-revision)
  (last revisions))

(define (second-to-last l)
  (list-ref l (- (length l) 2)))

(define (second-newest-revision)
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (second-to-last revisions)))

(define (newest-completed-revision)
  (define n (newest-revision))
  (if (read-cache* (build-path (revision-dir n) "analyzed"))
      n
      (second-newest-revision)))

(provide/contract
 [revisions (or/c false/c (listof exact-nonnegative-integer?))]
 [init-revisions! (-> void)]
 [newest-revision (-> exact-nonnegative-integer?)]
 [second-newest-revision (-> (or/c false/c exact-nonnegative-integer?))]
 [newest-completed-revision (-> (or/c false/c exact-nonnegative-integer?))])
