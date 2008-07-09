
(module tool mzscheme
  (require drscheme/tool
           mred
           mzlib/class
           mzlib/unit
           framework)

  (provide tool@)

  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)

      (define test-box-recovery-snipclass%
        (class snip-class%

          (inherit reading-version)

          (define/private (strings? e)
            (not (send e find-next-non-string-snip #f)))

          (define/private (extract-text e)
            (regexp-replace* #rx"\r\n" (send e get-flattened-text) " "))

          (define (make-string-snip s)
            (make-object string-snip% s))

          (define (make-comment-box . elems)
            (let* ([s (new comment-box:snip%)]
                   [e (send s get-editor)])
              (for-each (lambda (elem)
                          (cond
                           [(string? elem) (send e insert elem)]
                           [(elem . is-a? . text%)
                            (let loop ()
                              (let ([s (send elem find-first-snip)])
                                (when s
                                  (send elem release-snip s)
                                  (send e insert s)
                                  (loop))))]
                           [else (void)]))
                        elems)
              s))

          (define/override (read f)
            (let ([enabled?-box (box 0)]
                  [collapsed?-box (box 0)]
                  [error-box?-box (box 0)]
                  [to-test (new text%)]
                  [expected (new text%)]
                  [predicate (new text%)]
                  [should-raise (new text%)]
                  [error-message (new text%)])
              (let ([vers (reading-version f)])
                (case vers
                  [(1)
                   ;; Discard comment:
                   (send (new text%) read-from-file f)
                   (send* to-test (erase) (read-from-file f))
                   (send* expected (erase) (read-from-file f))
                   ;; Nothing else is in the stream in version 1,
                   ;;  so leave the defaults
                   ]
                  [(2)
                   (send* to-test (erase) (read-from-file f))
                   (send* expected (erase) (read-from-file f))
                   (send* predicate (erase) (read-from-file f))
                   (send* should-raise (erase) (read-from-file f))
                   (send* error-message (erase) (read-from-file f))
                   (send f get enabled?-box)
                   (send f get collapsed?-box)
                   (send f get error-box?-box)]))
              (if (zero? (unbox error-box?-box))
                  (if (and (strings? to-test)
                           (strings? expected))
                      (make-string-snip
                       (format "(check-expect ~a ~a)"
                               (extract-text to-test)
                               (extract-text expected)))
                      (make-comment-box "(check-expect "
                                        to-test
                                        " "
                                        expected
                                        ")"))
                  (if (strings? to-test)
                      (make-string-snip
                       (format "(check-error ~a ~s)"
                               (extract-text to-test)
                               (extract-text error-message)))
                      (make-comment-box "(check-error "
                                        to-test
                                        " "
                                        (extract-text error-message)
                                        ")")))))

          (super-new)))

      (define (phase1)
        (let ([sc (new test-box-recovery-snipclass%)])
          (send sc set-classname "test-case-box%")
          (send sc set-version 2)
          (send (get-the-snip-class-list) add sc)))
      
      (define (phase2)
        (void)))))

