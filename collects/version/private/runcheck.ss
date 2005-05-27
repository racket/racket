(module runcheck mzscheme
  (require (lib "unitsig.ss") (lib "list.ss") (lib "url.ss" "net")
           (lib "getinfo.ss" "setup")
           (lib "string-constant.ss" "string-constants")
           "checksigs.ss" (lib "patchlevel.ss" "version"))
  (provide runcheck@)
  (define runcheck@
    (unit/sig empty^ (import extra-params^ defs^)

      (define download-url-string "http://download.plt-scheme.org/")

      (define sync-sem (make-semaphore 0))

      (define ok-thunk
        (if sync? (lambda () (semaphore-post sync-sem)) void))

      (define current-format (string-constant vc-current-format))

      (define (make-url-string vcs)
        (string-append
         (apply string-append
                "http://download.plt-scheme.org/cgi-bin/check-version?"
                (map (lambda (cv)
                       (string-append
                        "package=" (car cv) "*" (cadr cv) "*" (caddr cv) "&"))
                     vcs))
         "binary-version="
         (if (> patchlevel 0)
           (format "~ap~a" (version) patchlevel)
           (version))))

      (define timeout-value 60)

      (define the-port #f)

      (define (get-collect-version-info collect)
        (let ([info-proc (get-info (list collect))])
          (if (not info-proc)
            (list 'no-info-file 'no-info-file)
            (list (info-proc 'release-version
                             (lambda _ 'no-release-info))
                  (info-proc 'release-iteration
                             (lambda _ 'no-iteration-info))))))

      (define (cvi-triples)
        ;; find all collections
        (let ([collects-dirs (current-library-collection-paths)])
          (let outer-loop ([collects-dirs collects-dirs])
            (if (null? collects-dirs)
              '()
              (let* ([curr-collects-dir (car collects-dirs)]
                     [dirs (filter
                            (lambda (d)
                              (directory-exists?
                               (build-path curr-collects-dir d)))
                            (if (directory-exists? curr-collects-dir)
                              (directory-list curr-collects-dir)
                              '()))])
                (let inner-loop ([dirs dirs])
                  (if (null? dirs)
                    (outer-loop (cdr collects-dirs))
                    (let* ([curr-dir (car dirs)]
                           [dir-version-and-iteration
                            (get-collect-version-info curr-dir)])
                      (if (andmap string? dir-version-and-iteration) ; not a symbol indicating an error
                        (cons (cons curr-dir dir-version-and-iteration)
                              (inner-loop (cdr dirs)))
                        (inner-loop (cdr dirs)))))))))))

      (define (go)
        (let* ([wait-dialog #f]
               [dialog-sem (make-semaphore 0)]
               [got-cancel? #f]
               [timer-proc
                (lambda ()
                  (let loop ([n 0])
                    (if (> n timeout-value)
                      (begin
                        (when wait-dialog (hide-wait-dialog wait-dialog))
                        ;; will force exception on pending read
                        (if the-port
                          (close-input-port the-port)
                          (set! got-cancel? #t))
                        (run-thunk
                         (lambda ()
                           (show-ok (string-constant vc-network-timeout)
                                    (list (string-constant vc-cannot-connect))
                                    #f
                                    ok-thunk))))
                      (begin (sleep 1) (loop (add1 n))))))]
               [timeout-thread (thread timer-proc)])

          (run-thunk
           (lambda ()
             (set! wait-dialog
                   (make-wait-dialog
                    #f
                    (string-constant vc-please-wait)
                    (string-constant vc-connecting-version-server)
                    (lambda ()
                      (set! got-cancel? #t)
                      (with-handlers ([void void]) ; thread might already be dead
                       (kill-thread timeout-thread)
                       (when the-port
                         (close-input-port the-port))))))
             (show-wait-dialog wait-dialog)
             (semaphore-post dialog-sem)))

          (semaphore-wait dialog-sem)

          (set! the-port
                (with-handlers ([void
                                 (lambda _
                                   (kill-thread timeout-thread)
                                   (hide-wait-dialog wait-dialog)
                                   (unless got-cancel?
                                     (run-thunk
                                      (lambda ()
                                        (show-error-ok
                                         (string-constant vc-network-failure)
                                         (string-constant vc-cannot-connect)))))
                                   (raise 'network-error))])
                  (get-pure-port
                   (string->url (make-url-string (cvi-triples))))))

          (when got-cancel? ; force exn in read, below
            (close-input-port the-port))

          (let ([responses
                 (let loop ()
                   (let ([r (read the-port)])
                     (if (eof-object? r)
                       (begin (kill-thread timeout-thread)
                              (hide-wait-dialog wait-dialog)
                              '())
                       (cons r (loop)))))]
                [needs-update #f])

            (close-input-port the-port)

            ;; responses are a list of lists of symbol/string pairs:
            ;;  (((package name)
            ;;    (installed-version v)
            ;;    (installed-iteration v)
            ;;    (latest-version v)
            ;;    (latest-iteration v)
            ;;    (verdict s))
            ;;    ... )

            ;; first handle binary info, which is always first in responses

            (let-values ([(_ binary-version binary-iteration
                             latest-binary-version latest-binary-iteration
                             binary-verdict)
                          (apply values (map cadr (car responses)))])

              (if (eq? binary-verdict 'update)

                ;; inform user of new binary

                (show-ok
                 (string-constant vc-update-dialog-title)
                 (list
                  (string-constant vc-old-binaries)
                  (format
                   (string-constant vc-binary-information-format)
                   binary-version binary-iteration)
                  (format
                   (string-constant vc-latest-binary-information-format)
                   latest-binary-version latest-binary-iteration)
                  (string-append (string-constant vc-updates-available) " " download-url-string))
                  #f
                  ok-thunk)

                ;; else offer info for installed packages

                (let* ([details
                        (map
                         (lambda (r)
                           (let*-values
                               ([(data) (map cadr r)]
                                [(package installed-version installed-iteration
                                          latest-version latest-iteration verdict)
                                 (apply values data)])
                             (cond
                              [(eq? verdict 'up-to-date)
                               (format current-format
                                       package installed-version installed-iteration)]
                              [(eq? verdict 'update)
                               (set! needs-update #t)
                               (format (string-constant vc-update-format)
                                       package
                                       installed-version installed-iteration
                                       latest-version latest-iteration)]
                              [else ""])))
                          (cdr responses))])

                   (show-ok
                    (string-constant vc-update-dialog-title)
                    (list (if needs-update
                            (string-constant vc-need-update-string)
                            (string-constant vc-no-update-string))
                          (format current-format
                                  (string-constant vc-binary-name)
                                  binary-version binary-iteration))
                    (if needs-update
                      (append details
                              (list
                               ""
                               (string-constant vc-updates-available)
                               download-url-string))
                      details)
                    ok-thunk)))))))

      ;; exceptions are used to report errors elsewhere
      ;; just ignore here

      (run-thunk
       (lambda ()
         (with-handlers ([void (lambda _
                                 (when sync? (semaphore-post sync-sem)))])
          (go))))

      (when sync? (semaphore-wait sync-sem)))))
