#lang racket/base
(require aws/keys
         aws/s3
         racket/file
         racket/cmdline
         racket/set
         racket/format
         racket/port
         net/url
         http/head
         pkg/lib)

(define empty-source "empty.zip")
(define empty-source-checksum "9f098dddde7f217879070816090c1e8e74d49432")
;; Versions to map to the empty source:
(define compatibility-versions '("5.3.4" "5.3.5" "5.3.6"))

(define-values (src-dir s3-hostname bucket dest-catalog)
  (command-line
   #:args
   (src-dir s3-hostname bucket dest-catalog)
   (values src-dir s3-hostname bucket dest-catalog)))

(ensure-have-keys)
(s3-host s3-hostname)

(define-values (catalog-email catalog-password)
  (call-with-input-file* 
   (build-path (find-system-path 'home-dir) ".pkg-catalog-login")
   (lambda (i) (values (read i) (read i)))))

(printf "Getting current packages at ~a...\n" dest-catalog)
(define current-pkgs
  (parameterize ([current-pkg-catalogs (list (string->url dest-catalog))])
    (get-all-pkg-details-from-catalogs)))
(printf "... got it.\n")

(define new-pkgs
  (let ([dir (build-path src-dir "catalog" "pkg")])
    (for/hash ([i (in-list (directory-list dir))])
      (define ht (call-with-input-file* (build-path dir i) read))
      (values (path->string i)
              (hash-set ht
                        'source
                        (format "http://~a.~a/pkgs/~a/~a.zip"
                                bucket
                                s3-hostname
                                (hash-ref ht 'checksum)
                                i))))))

;; Compute the package in the main distribution
(define main-dist-pkgs
  ;; A union-find would be better...
  (let loop ([pkgs (set)] [check-pkgs (set "main-distribution")])
    (cond
     [(set-empty? check-pkgs) pkgs]
     [else
      (define a (set-first check-pkgs))
      (define r (set-rest check-pkgs))
      (if (set-member? pkgs a)
          (loop pkgs r)
          (loop (set-add pkgs a)
                (set-union
                 r
                 (set-remove
                  (apply set (map (lambda (p) (if (pair? p) (car p) p)) 
                                  (hash-ref (hash-ref new-pkgs a)
                                            'dependencies
                                            '())))
                  "racket"))))])))

(printf "Getting current S3 content...\n")
(define old-content (list->set (ls (string-append bucket "/pkgs"))))
(printf "... got it.\n")

;; A list of `(cons checksum p)':
(define new-checksums&files
  (let ([dir (build-path src-dir "pkgs")])
    (for*/list ([checksum (in-list (directory-list dir))]
                [p (in-list (directory-list (build-path dir checksum)))])
      (cons (path->string checksum) (path->string p)))))

;; A tag that we install for each checksum that is used.
;; We can detect obsolte checksums as not having a recent
;; enough tag (i.e., older than an era). An "era" is
;; currently defined as a week.
(define now-era (quotient (current-seconds) (* 7 24 60 60)))
(define now (~a now-era))
(define recently (~a (sub1 now-era)))

;; ----------------------------------------

;; Push one file at a given chcksum to the bucket
(define (sync-one checksum p)
  (printf "Checking ~a @ ~a\n" p checksum)

  (define (at-checksum p)
    (string-append "pkgs/" checksum "/" p))
  (define (at-bucket&checksum p)
    (string-append bucket "/" (at-checksum p)))

  (define (put p content)
    (printf "Putting ~a\n" p)
    (define s (put/bytes p
                         content
                         "application/octet-stream"
                         #hash((x-amz-storage-class . "REDUCED_REDUNDANCY")
                               (x-amz-acl . "public-read"))))
    (unless (member (extract-http-code s) '(200))
      (printf "put failed for ~s: ~s\n" p s)))

  (unless (set-member? old-content (at-checksum now))
    (put (at-bucket&checksum now)
         #"ok"))

  (unless (set-member? old-content (at-checksum p))
    (put (at-bucket&checksum p)
         (file->bytes (build-path src-dir "pkgs" checksum p)))))

;; Discard an obsolete file
(define (purge-one checksum raw-p)
  (printf "Removing ~a @ ~a\n" raw-p checksum)

  (define p (string-append bucket "/pkgs/" checksum "/" raw-p))

  (define s (delete p))
  (unless (member (extract-http-code s) '(200 204))
    (printf "delete failed for ~s: ~s\n" p s)))

;; Update the package catalog:
(define (update-catalog the-email the-password the-post)
  (define the-url
    (let ([u (string->url dest-catalog)])
      (struct-copy url u
                   [path
                    (append
                     (url-path u)
                     (list (path/param "api" null)
                           (path/param "upload" null)))])))
  (define bs
    (call/input-url the-url
                    (λ (url)
                      (post-pure-port the-url
                                      (with-output-to-bytes
                                       (λ ()
                                         (write (list the-email
                                                      (string->bytes/utf-8 the-password)
                                                      the-post))))))
                    port->bytes))
  (read (open-input-bytes bs)))

(define (add-compatibility-pkgs ht)
  (hash-set ht 'versions
            (for/fold ([ht2 (hash-ref ht 'versions (hash))]) ([v compatibility-versions])
              (hash-set ht2 v (hash 'source
                                    (format "http://~a.~a/pkgs/~a"
                                            bucket
                                            s3-hostname
                                            empty-source)
                                    'checksum
                                    empty-source-checksum)))))

(define (add-ring-0 ht)
  (hash-set ht 'ring 0))

;; ------------------------------

;; Upload current files:
(for ([p (in-list new-checksums&files)])
  (sync-one (car p) (cdr p)))

;; Use 'default version, if any
(define (hash-ref* ht k def)
  (define ht2 (hash-ref (hash-ref ht 'versions (hash))
                        'default
                        ht))
  (hash-ref ht2 k (hash-ref ht k def)))

;; Update the catalog:
(let ([changed-pkgs
       (for/hash ([(k v) (in-hash new-pkgs)]
                  #:unless (let ([ht (hash-ref current-pkgs k #hash())])
                             (and (equal? (hash-ref v 'source)
                                          (hash-ref* ht 'source #f))
                                  (equal? (hash-ref v 'checksum)
                                          (hash-ref* ht 'checksum #f)))))
         (define (add-tag v t)
           (define l (hash-ref v 'tags '()))
           (if (member t l)
               v
               (hash-set v 'tags (cons t l))))
         (values k (add-ring-0
                    (add-compatibility-pkgs
                     (cond
                      [(set-member? main-dist-pkgs k)
                       (add-tag v "main-distribution")]
                      [(let ([m (regexp-match #rx"^(.*)-test$" k)])
                         (and m
                              (set-member? main-dist-pkgs (cadr m))))
                       (add-tag v "main-tests")]
                      [(equal? k "racket-test")
                       (add-tag v "main-tests")]
                      [else v])))))])
  (unless (zero? (hash-count changed-pkgs))
    (printf "Updating catalog:\n")
    (for ([k (in-hash-keys changed-pkgs)])
      (printf "  ~a\n" k))
    (define r (update-catalog catalog-email catalog-password changed-pkgs))
    (unless (equal? r #t)
      (printf "unexpected result from catalog update: ~s\n" r))))
(printf "Catalog updated\n")

;; Look for files that can be discarded:
(let ([new-checksums
       (for/set ([pr (in-list new-checksums&files)])
         (car pr))])
  (for ([p (in-set old-content)])
    (define m (regexp-match #rx"^pkgs/([^/]*)/([^/]*)$" p))
    (when m
      (define checksum (cadr m))
      (define p (caddr m))
      (cond
       [(set-member? new-checksums checksum)
        ;; Keep this checksum, but look for old timestamp files.
        (when (regexp-match? #rx"^[0-9]*$" p)
          (unless (or (equal? p now)
                      (equal? p recently))
          ;; Looks like we can delete it
            (purge-one checksum p)))]
       [(or (set-member? old-content (string-append "pkgs/" checksum "/" recently))
            (set-member? old-content (string-append "pkgs/" checksum "/" recently)))
        ;; Recent enough timestamp; don't discard
        (void)]
       [else
        ;; Old checksum, so discard
        (purge-one checksum p)]))))
