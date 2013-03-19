(module foldo mzscheme
  (require (lib "file.rkt") (lib "match.rkt"))
  (provide apply-to-scheme-files)

   (define-syntax (define-excluder stx)

    (define (path->clause c)
      (syntax-case c ()
        [(item ...)
         #`[`(#,@(reverse (syntax-e #'(item ...))) ,_ (... ...)) #t]]
        [item
         #`[`(item) #t]]))

    (syntax-case stx ()
      [(_ name path ...)
       (with-syntax ([(match-clause ...) (map path->clause (syntax-e #'(path ...)))])
         #`(define (name p )
             (let* ([dirnames (map path->string (explode-path p))])
               (match (reverse dirnames) ; goofy backwards matching because ... matches greedily
                 match-clause ...
                 [_ #f]))))]))

  (define-excluder default-excluder
    "compiled" ".git")

  (define exclude-directory?  (make-parameter default-excluder))

  ;; ----------------------------------------
  ;; apply-to-scheme-files: (path[file] -> X) path[directory] -> (listof X)
  ;; applies the given function to each .rkt or .ss or .scm file in the given
  ;; directory hierarchy; returns all results in a list
  (define  (apply-to-scheme-files f root )
     ;;FOLD-FILES

    (fold-files
     (lambda (path kind acc)
       (case kind
         [(file)
          (let ([extension (filename-extension path)])
            (cond
              [(not extension) acc ]
              [(regexp-match #rx"(rkt|rktl|ss|scm)$" extension)
               (let ([resl (f path)])
                 (if resl
                     (cons resl acc)
                     acc ))]
              [else acc ]))]
        [(dir)
          (let* ([p (normalize-path path root)])
            (if ((exclude-directory?) p)
              (values acc #f)
               acc ))]
         [(link) acc ]
         [else (error "never happen")]))
     '()
      root
      ))
  )
