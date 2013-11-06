#lang racket/base
(require file/cache
         racket/file)

(define-logger cache)


(define tmp-dir (make-temporary-file "rkttmp~a"
                                     'directory
                                     (find-system-path 'temp-dir)))

(define keep-in-cache '(3 7 13))
(define complain-if-uncached? #f)
(define concurrent? #f)
(define nonread? #f)

(define (ok-error? s)
  (cond
   [concurrent?
    (regexp-match? #rx"could not acquire .* lock" s)]
   [nonread?
    (regexp-match? #rx"copy-file: cannot open source file" s)]
   [else #f]))

(define (get n #:result [result void]
             #:dir [dir-suffix ""])
  (define files-dir (build-path tmp-dir (format "files~a" dir-suffix)))
  (make-directory* files-dir)
  (define fn (build-path files-dir (format "~a" n)))
  (define content (make-bytes (* n 100) n))
  (when (file-exists? fn) (delete-file fn))
  (define got? #f)
  (define errored? #f)
  (cache-file fn
              n
              (build-path tmp-dir "cache")
              (lambda ()
                (call-with-output-file*
                 fn
                 (lambda (o)
                   (write-bytes content o))))
              #:evict-before? (lambda (a b)
                                (define a-keep? (member (hash-ref a 'key) keep-in-cache))
                                (define b-keep? (member (hash-ref b 'key) keep-in-cache))
                                (cond
                                 [(and (not a-keep?) b-keep?) #t]
                                 [(and (not b-keep?) a-keep?) #f]
                                 [else
                                  (< (hash-ref a 'modify-seconds)
                                     (hash-ref b 'modify-seconds))]))
              #:max-cache-size 10000
              #:log-debug-string (lambda (s) (log-cache-debug s))
              #:log-error-string (lambda (s)
                                   (if (ok-error? s)
                                       (set! errored? #t)
                                       (log-error s)))
              #:notify-cache-use (lambda (s)
                                   (set! got? #t)))
  (unless (= (file-size fn) (bytes-length content))
    (error 'test "wrong file size for ~a" fn))
  (unless (equal? content (file->bytes fn))
    (error 'test "wrong content for ~a" fn))
  (unless (or got? errored?)
    (when complain-if-uncached?
      (when (member n keep-in-cache)
        (error 'test "wasn't in cache: ~a" n))))
  (result got?))

;; --------------------------------------------------
;; Test basic caching

(for ([i keep-in-cache]) (get i))
(set! complain-if-uncached? #t)

(for ([i 20]) (get i))
(for ([i (in-range 7 12)]) (get i))
(for ([i (in-range 7 12)]) (get i))
(for ([i (in-range 7 12)]) (get i))

 (for ([i 20]) (get i))

(for ([i 100])
  (for ([i 20]) (get i)))

(cache-remove (car keep-in-cache)
              (build-path tmp-dir "cache"))
(set! complain-if-uncached? #f)
(when (get (car keep-in-cache) #:result values)
  (error 'test "should not have been in cache"))
(set! complain-if-uncached? #t)
(for ([i keep-in-cache]) (get i))

(cache-remove #f
              (build-path tmp-dir "cache"))
(set! complain-if-uncached? #f)
(for ([i keep-in-cache])
  (when (get i #:result values)
    (error 'test "should not have been in cache")))
(set! complain-if-uncached? #t)
(for ([i keep-in-cache]) (get i))

;; --------------------------------------------------
;; Test concurrent use

(set! concurrent? #t)
(for-each
 sync
 (for/list ([j 100])
   (thread (lambda ()
             (for ([i 20]) (get i #:dir j))))))
(set! concurrent? #f)

;; --------------------------------------------------
;; Test uncooperative filesystem

(define (all-file-perms perms)
  (define dir (build-path tmp-dir "cache"))
  (for ([f (directory-list dir)])
    (when (regexp-match? #rx"^cached" f)
      (file-or-directory-permissions (build-path dir f) perms))))

(set! nonread? #t)
(for ([j 2])
  (for ([i 20])
    (get i)
    (all-file-perms 0)))

;; make files readable and writable again, and cache should recover:
(all-file-perms #o777)
(set! nonread? #f)    
(set! complain-if-uncached? #f)
(for ([i 20]) (get i))

(set! complain-if-uncached? #t)
(for ([i 20]) (get i))

;; --------------------------------------------------

(delete-directory/files tmp-dir)
