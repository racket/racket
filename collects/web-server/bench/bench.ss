#lang scheme/base
(require #;mzlib/file
         mzlib/list
         mzlib/process
         web-server/web-server
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (planet "csv.ss" ("neil" "csv.plt" 1 1)))

(define data-path (collection-path "web-server" "bench" "data"))
(define port (make-parameter 9480))

(define *test-file* (make-temporary-file))
(with-output-to-file *test-file*
  (lambda ()
    (write (make-string 1024 #\a)))
  #:exists 'truncate/replace)      

(define *data-file* (make-temporary-file))

(define server-cust (make-custodian))
(define shutdown
  (parameterize ([current-custodian server-cust])
    (serve #:port (port)
           #:dispatch
           (files:make #:url->path (lambda _ (values *test-file* empty))))))

(define before/s (current-memory-use server-cust))
(define before (current-memory-use))

(define start (current-seconds))  
(define cmd (format "ab -c 10 -t 120 -e ~a http://localhost:~a/file" 
                    (path->bytes *data-file*)
                    (port)))
(system cmd)
(define stop (current-seconds))

(define after/s (current-memory-use server-cust))
(define after (current-memory-use))
(collect-garbage) (collect-garbage) (collect-garbage) (collect-garbage)
(define after-gc/s (current-memory-use server-cust))
(define after-gc (current-memory-use))

(shutdown)
(define shut (current-seconds))

(define response-percentiles
  (map string->number
       (rest
        (call-with-input-file *data-file*
          (lambda (ip)
            (csv-map second ip))))))

(define data
  `(benchmark (cmd ,cmd)
              (timing (start ,start)
                      (stop ,stop)
                      (shutdown ,shut))
              (memory-usage
               (mzscheme (before ,before)
                         (after ,after)
                         (after-gc ,after-gc))
               (server (before ,before/s)
                       (after ,after/s)
                       (after-gc ,after-gc/s)))
              (response-times
               ,response-percentiles)))

(printf  "~S~n" data)

(with-output-to-file (build-path data-path (format "~a.ss" (number->string (current-seconds))))
  (lambda () (printf  "~S~n" data)))
