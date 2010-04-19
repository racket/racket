
(load-relative "loadtest.ss")

(Section 'cm)

(require compiler/cm
         scheme/file)

(define dir (build-path (find-system-path 'temp-dir)
                        "plt-cm-test"))
(make-directory* dir)

(define (try files   #; (list (list path content-str compile?) ...)
             recomps #; (list (list (list touch-path ...) 
                                    (list rebuild-path ...)
                                    (list check-rebuilt-path ...)))
             )
  (delete-directory/files dir)
  (make-directory* dir)
  (printf "wrorking in ~s\n" dir)
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
                     (build-path dir "compiled" (path-add-suffix (car f) #".zo"))
                     #f
                     (lambda () -inf.0)))))])
    (for-each (lambda (recomp)
                (printf "pausing...\n")
                (sleep 1) ;; timestamps have a 1-second granularity on most filesystems
                (let ([to-touch (list-ref recomp 0)]
                      [to-make (list-ref recomp 1)])
                  (for-each (lambda (f)
                              (printf "touching ~a\n" f)
                              (with-output-to-file (build-path dir f)
                                #:exists 'append
                                (lambda () (display " "))))
                            to-touch)
                  (for-each (lambda (f)
                              (printf "re-making ~a\n" f)
                              (managed-compile-zo (build-path dir f)))
                            to-make)
                  (for-each (lambda (f)
                              (let ([ts (hash-ref timestamps f)]
                                    [new-ts
                                     (file-or-directory-modify-seconds
                                      (build-path dir "compiled" (path-add-suffix f #".zo"))
                                      #f
                                      (lambda () -inf.0))]
                                    [updated? (lambda (a b) a)])
                                (test (and (member f (caddr recomp)) #t)
                                      updated?
                                      (new-ts . > . ts)
                                      f)
                                (hash-set! timestamps f new-ts)))
                            (map car files))))
              recomps)))

(try '(("a.ss" "(module a scheme/base (require \"b.ss\" \"d.ss\" \"g.ss\"))" #t)
       ("b.ss" "(module b scheme/base (require scheme/include) (include \"c.sch\"))" #t)
       ("d.ss" "#reader \"e.ss\" 10" #t)
       ("c.sch" "5" #f)
       ("e.ss" "(module e syntax/module-reader \"f.ss\")" #t)
       ("f.ss" "(module f scheme/base (provide (all-from-out scheme/base)))" #t)
       ("g.ss" "(module g scheme/base (require (for-syntax scheme/base scheme/include \"i.ss\")) (define-syntax (f stx) (include \"h.sch\")))" #t)
       ("h.sch" "(quote-syntax 12)" #f)
       ("i.ss" "(module i scheme/base)" #t))
     '([("a.ss") ("a.ss") ("a.ss")]
       [("b.ss") ("a.ss") ("a.ss" "b.ss")]
       [("b.ss") ("b.ss") ("b.ss")]
       [() ("a.ss") ("a.ss")]
       [("c.sch") ("a.ss") ("a.ss" "b.ss")]
       [("f.ss") ("a.ss") ("a.ss" "d.ss" "f.ss")]
       [("e.ss") ("e.ss") ("e.ss")]
       [() ("a.ss") ("a.ss" "d.ss")]
       [("i.ss") ("a.ss") ("a.ss" "g.ss" "i.ss")]
       [("h.sch") ("a.ss") ("a.ss" "g.ss")]))

;; test manager-skip-file-handler
(parameterize ([manager-skip-file-handler
                (λ (x)
                  (let-values ([(base name dir) (split-path x)])
                    (cond
                      [(equal? (path->string name) "b.ss")
                       (file-or-directory-modify-seconds x)]
                      [else #f])))])
  (try '(("a.ss" "(module a scheme/base (require \"b.ss\"))" #f)
         ("b.ss" "(module b scheme/base)" #f))
       '([("b.ss") ("a.ss") ("a.ss")])))

;; ----------------------------------------

;; test `file-date-in-paths'
(test (file-or-directory-modify-seconds (build-path (collection-path "file") 
                                                    "compiled"
                                                    "gif_ss.zo"))
      file-date-in-collection
      (build-path (collection-path "file") "gif.ss"))
;; gl-info.ss doesn't have a .ss source:
(test (file-or-directory-modify-seconds (build-path (collection-path "sgl") 
                                                    "compiled"
                                                    "gl-info_ss.zo"))
      file-date-in-collection
      (build-path (collection-path "sgl") "gl-info.ss"))
;; setup/main doesn't have a .zo:
(test (file-or-directory-modify-seconds (build-path (collection-path "setup") 
                                                    "main.ss"))
      file-date-in-collection
      (build-path (collection-path "setup") "main.ss"))

;; ----------------------------------------

(report-errs)
