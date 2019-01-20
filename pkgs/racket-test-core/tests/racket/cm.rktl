
(load-relative "loadtest.rktl")

(Section 'cm)

(require compiler/cm
         racket/file)

(define dir (build-path (find-system-path 'temp-dir)
                        "plt-cm-test"))
(make-directory* dir)

(define compiled-dir (car (use-compiled-file-paths)))

(define (try files   #; (list (list path content-str compile?) ...)
             recomps #; (list (list (list touch-path ...) 
                                    (list rebuild-path ...)
                                    (list check-rebuilt-path ...)))
             )
  (delete-directory/files dir)
  (make-directory* dir)
  (printf "working in ~s\n" dir)
  (for-each (lambda (f)
              (printf "writing ~a\n" (car f))
              (with-output-to-file (build-path dir (car f))
                (lambda ()
                  (display (cadr f)))))
            files)
  (for-each (lambda (f)
              (when (caddr f)
                (printf "making ~a\n" (car f))
                (managed-compile-zo (build-path dir (car f)))))
            files)
  (let ([timestamps
         (hash-copy
          (for/hash ([f (in-list files)])
            (values (car f)
                    (file-or-directory-modify-seconds
                     (build-path dir compiled-dir (path-add-suffix (car f) #".zo"))
                     #f
                     (lambda () -inf.0)))))])
    (for ([touch-mode '(touch-zo normal)])
      (for-each (lambda (recomp)
                  (define (pause)
                    (printf "pausing...\n")
                    (sleep 1)) ;; timestamps have a 1-second granularity on most filesystems
                  (pause)
                  (let ([to-touch (list-ref recomp 0)]
                        [to-make (list-ref recomp 1)])
                    (for-each (lambda (f)
                                (printf "touching ~a\n" f)
                                (with-output-to-file (build-path dir f)
                                  #:exists 'append
                                  (lambda () (display " ")))
                                (when (eq? touch-mode 'touch-zo)
                                  ;; Make sure a new typestamp on the bytecode file doesn't
                                  ;; prevent a recompile
                                  (define d (build-path dir compiled-dir (path-add-suffix f #".zo")))
                                  (when (file-exists? d)
                                    (printf "touching .zo for ~a\n" f)
                                    (file-or-directory-modify-seconds d (current-seconds))
                                    (hash-set! timestamps f (file-or-directory-modify-seconds d)))))
                              to-touch)
                    (for-each (lambda (f)
                                (let* ([d (build-path dir compiled-dir (path-add-suffix f #".zo"))]
                                       [ts (file-or-directory-modify-seconds d #f (lambda () #f))])
                                  (when ts
                                    (printf "mangling .zo for ~a\n" f)
                                    (with-output-to-file d
                                      #:exists 'truncate
                                      (lambda () (display "#~bad")))
                                    (file-or-directory-modify-seconds d ts))))
                              (caddr recomp))
                    (when (eq? touch-mode 'touch-zo)
                      (pause))
                    (for-each (lambda (f)
                                (printf "re-making ~a\n" f)
                                (managed-compile-zo (build-path dir f)))
                              to-make)
                    (for-each (lambda (f)
                                (let* ([d (build-path dir compiled-dir (path-add-suffix f #".zo"))]
                                       [ts (hash-ref timestamps f)]
                                       [new-ts
                                        (file-or-directory-modify-seconds
                                         d
                                         #f
                                         (lambda () -inf.0))]
                                       [updated? (lambda (a b) a)])
                                  (test (and (member f (caddr recomp)) #t)
                                        updated?
                                        (new-ts . > . ts)
                                        f)
                                  (hash-set! timestamps f new-ts)))
                              (map car files))))
                recomps))))

(try '(("a.rkt" "(module a scheme/base (require \"b.rkt\" \"d.rkt\" \"g.rkt\"))" #t)
       ("b.rkt" "(module b scheme/base (require scheme/include) (include \"c.sch\"))" #t)
       ("d.rkt" "#reader \"e.rkt\" 10" #t)
       ("c.sch" "5" #f)
       ("e.rkt" "(module e syntax/module-reader \"f.rkt\")" #t)
       ("f.rkt" "(module f scheme/base (provide (all-from-out scheme/base)))" #t)
       ("g.rkt" "(module g scheme/base (require (for-syntax scheme/base scheme/include \"i.rkt\")) (define-syntax (f stx) (include \"h.sch\")))" #t)
       ("h.sch" "(quote-syntax 12)" #f)
       ("i.rkt" "(module i scheme/base)" #t)
       ("j.rkt" "(module j racket/base (module+ main (require \"b.rkt\")))" #t))
     '([("a.rkt") ("a.rkt") ("a.rkt")]
       [("b.rkt") ("a.rkt" "j.rkt") ("a.rkt" "b.rkt" "j.rkt")]
       [("b.rkt") ("b.rkt") ("b.rkt")]
       [() ("j.rkt") ("j.rkt")]
       [() ("a.rkt") ("a.rkt")]
       [("c.sch") ("j.rkt") ("b.rkt" "j.rkt")]
       [() ("a.rkt") ("a.rkt")]
       [("f.rkt") ("a.rkt") ("a.rkt" "d.rkt" "f.rkt")]
       [("e.rkt") ("e.rkt") ("e.rkt")]
       [() ("a.rkt") ("a.rkt" "d.rkt")]
       [("i.rkt") ("a.rkt") ("a.rkt" "g.rkt" "i.rkt")]
       [("h.sch") ("a.rkt") ("a.rkt" "g.rkt")]))

;; test that deleting a relevant file makes compilation fail:
(define (try-remove rmv chk)
  (printf "breaking ~a\n" rmv)
  (delete-file (build-path dir rmv))
  (for ([sfx '(#".zo" #".dep")])
    (let ([f (build-path dir compiled-dir (path-add-suffix rmv sfx))])
      (when (file-exists? f)
        (delete-file f))))
  (test 'correctly-failed
        'try-compile
        (parameterize ([current-namespace (make-base-namespace)])
          (with-handlers ([(lambda (exn)
                             (or (exn:missing-module? exn)
                                 (exn:fail:syntax? exn)))
                           (lambda (exn) 'correctly-failed)])
            (managed-compile-zo (build-path dir chk))))))
(try-remove "e.rkt" "d.rkt")
(try-remove "d.rkt" "a.rkt")
(try-remove "c.sch" "b.rkt")

;; test manager-skip-file-handler
(parameterize ([manager-skip-file-handler
                (λ (x)
                  (let-values ([(base name dir) (split-path x)])
                    (cond
                      [(equal? (path->string name) "b.rkt")
                       (cons (file-or-directory-modify-seconds x)
                             "")]
                      [else #f])))])
  (try '(("a.rkt" "(module a scheme/base (require \"b.rkt\"))" #f)
         ("b.rkt" "(module b scheme/base)" #f))
       '([("b.rkt") ("a.rkt") ("a.rkt")])))

;; test current-path->mode
(let ()
  (try '(("cpm-a.rkt" "#lang racket/base\n(require \"cpm-b.rkt\")\n" #f)
         ("cpm-b.rkt" "#lang racket/base\n(require \"cpm-c.rkt\")\n" #f)
         ("cpm-c.rkt" "#lang racket/base\n" #f))
       '())
  (parameterize ([current-path->mode
                  (λ (x)
                    (define-values (base name dir?) (split-path x))
                    (define (same-path? p1 p2)
                      (equal? (map path->string (explode-path p1))
                              (map path->string (explode-path p2))))
                    (cond
                      [(same-path? base dir)
                       (cond
                         [(equal? (path->string name) "cpm-b.rkt")
                          (build-path compiled-dir "subdir")]
                         [else
                          (build-path compiled-dir)])]
                      [else (build-path compiled-dir)]))])
    (managed-compile-zo (build-path dir "cpm-a.rkt")))
  (define compiled-dir-strs (map path->string (explode-path compiled-dir)))
  (test (hash '("cpm-a.rkt") #t
              '("cpm-b.rkt") #t
              '("cpm-c.rkt") #t
              `(,@compiled-dir-strs "cpm-a_rkt.zo") #t
              `(,@compiled-dir-strs "cpm-a_rkt.dep") #t
              `(,@compiled-dir-strs "subdir" "cpm-b_rkt.zo") #t
              `(,@compiled-dir-strs "subdir" "cpm-b_rkt.dep") #t
              `(,@compiled-dir-strs "cpm-c_rkt.zo") #t
              `(,@compiled-dir-strs "cpm-c_rkt.dep") #t)
        'current-path->mode
        (for/hash ([x (in-directory dir)]
                   #:when (file-exists? x))
          (values (map path->string (explode-path (find-relative-path dir x)))
                  #t))))

;; ----------------------------------------

;; test `file-stamp-in-paths'
(test (file-or-directory-modify-seconds (build-path (path-only (collection-file-path "zip.rkt" "file"))
                                                    compiled-dir
                                                    "zip_rkt.zo"))
      car
      (file-stamp-in-collection
       (collection-file-path "zip.rkt" "file")))
;; check bytecode without a source:
(let ([f (build-path dir compiled-dir "nosrc_rkt.zo")])
  (with-output-to-file f #:exists 'truncate (lambda () (write (compile #'(module nosrc racket/base)))))
  (test (file-or-directory-modify-seconds f)
        car
        (file-stamp-in-paths
         (build-path dir "nosrc.rkt")
         (list dir))))
;; setup/main doesn't have a .zo:
(test (file-or-directory-modify-seconds (collection-file-path "main.rkt" "setup"))
      car
      (file-stamp-in-collection
       (collection-file-path "main.rkt" "setup")))

;; ----------------------------------------

(delete-directory/files dir)

;; ----------------------------------------
;; Check that cm sets reader for .dep files:

(parameterize ([current-readtable (make-readtable #f #\( 'terminating-macro void)])
  (parameterize ([current-namespace (make-base-namespace)])
    (parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)])
      (test (void) dynamic-require 'compiler/cm #f))))

;; ----------------------------------------
;; test for make-compile-lock

(let ()
  #|
  
This test creates a file to compile that, during compilation, conditionally
freezes forever. It first creates a thread to compile the file in freeze-forever
mode, and then, when the thread is stuck, creates a second thread to compile
the file and kills the first thread. The second compile should complete properly
and the test makes sure that it does and that the first thread doesn't complete.

  |#
  
  (define (sexps=>file file #:lang [lang #f] . sexps)
    (call-with-output-file file 
      (λ (port)
        (when lang (fprintf port "~a\n" lang))
        (for ([x (in-list sexps)]) (fprintf port "~s\n" x)))
      #:exists 'truncate))

  (define (poll-file file for)
    (let loop ([n 100])
      (when (zero? n)
        (error 'compiler/cm::poll-file "never found ~s in ~s" for file))
      (define now (call-with-input-file file (λ (port) (read-line port))))
      (unless (equal? now for)
        (sleep .1)
        (loop (- n 1)))))
  
  (define file-to-compile (make-temporary-file "cmtest-file-to-compile~a.rkt"))
  (define control-file (make-temporary-file "cmtest-control-file-~a.rktd"))
  (define about-to-get-stuck-file (make-temporary-file "cmtest-about-to-get-stuck-file-~a.rktd"))
  
  (sexps=>file file-to-compile #:lang "#lang racket"
               `(define-syntax (m stx)
                  (call-with-output-file ,(path->string about-to-get-stuck-file) 
                    (λ (port) (fprintf port "about\n"))
                    #:exists 'truncate)
                  (if (call-with-input-file ,(path->string control-file) read)
                      (semaphore-wait (make-semaphore 0))
                      #'1))
               '(void (m)))
  (sexps=>file control-file #t)
  
  (define p-l-c (compile-lock->parallel-lock-client (make-compile-lock) (current-custodian)))
  (define t1-finished? #f)
  (parameterize ([parallel-lock-client p-l-c]
                 [current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)])
    (define finished (make-channel))
    (define t1 (thread (λ () (dynamic-require file-to-compile #f) (set! t1-finished? #t))))
    (poll-file about-to-get-stuck-file "about")
    (sexps=>file control-file #f)
    (define t2 (thread (λ () (dynamic-require file-to-compile #f) (channel-put finished #t))))
    (sleep .1) ;; give thread t2 time to get stuck waiting for t1 to compile
    (kill-thread t1)
    (channel-get finished)

    (test #f 't1-finished? t1-finished?)
    
    (test #t 
          'compile-lock::compiled-file-exists
          (file-exists?
           (let-values ([(base name dir?) (split-path file-to-compile)])
             (build-path base 
                         compiled-dir
                         (bytes->path (regexp-replace #rx"[.]rkt" (path->bytes name) "_rkt.zo"))))))
    
    (define compiled-dir-to-discard
      (let-values ([(base name dir?) (split-path file-to-compile)])
        (build-path base compiled-dir)))
    (delete-file file-to-compile)
    (delete-file control-file)
    (delete-file about-to-get-stuck-file)
    (delete-directory/files compiled-dir-to-discard)))

;; ----------------------------------------

(report-errs)
