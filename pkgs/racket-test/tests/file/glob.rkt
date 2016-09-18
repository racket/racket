#lang racket/base

(require file/glob
         racket/runtime-path
         racket/path
         rackunit
         (for-syntax racket/base))


(define-runtime-path glob-test-dir "./glob-test-dir")
(define glob-test-subdir (build-path glob-test-dir "glob-test-subdir"))
(define glob-test-sub-sub-dir (build-path glob-test-subdir "glob-test-sub-sub-dir"))

(define (check-test-dir)
  (unless (directory-exists? glob-test-dir)
    (raise-user-error 'file/glob
                      "could not locate directory '~a', cannot run tests"
                      glob-test-dir)))

(module+ test
  (check-test-dir)
  (current-directory glob-test-dir)

  (define unsupplied-arg (gensym))
  (define (glob/cwd g #:capture-dotfiles? [cd? unsupplied-arg])
    (define cwd (list (current-directory)))
    (define p* (if (eq? cd? unsupplied-arg) (glob g) (glob g #:capture-dotfiles? cd?)))
    (for/list ([p (in-list p*)])
      (path->string (shrink-path-wrt p cwd))))

  (define (add-cwd* p*)
    (define cwd (current-directory))
    (for/list ([p (in-list p*)])
      (build-path cwd p)))

  (test-case "glob:current-dir"

    (check-equal?
      (sort (glob/cwd "*") string<?)
      (sort
        (for/list ([p (in-list (directory-list glob-test-dir))]
                   #:when (not (eq? #\. (string-ref (path->string p) 0))))
          (path->string p))
        string<?))

    (check-equal? (glob "README.md") (add-cwd* '("README.md")))
    (check-equal? (glob/cwd "*.md") '("README.md"))
    (check-equal? (glob/cwd "README.m?") '("README.md"))
    (check-equal? (glob/cwd "README.?d") '("README.md"))
    (check-equal? (glob/cwd "RE?*ME.md") '("README.md"))
    (check-equal? (glob/cwd "RE*?ME.md") '("README.md"))
    (check-equal? (glob/cwd "REA[DM]ME.md") '("README.md"))
    (check-equal? (glob/cwd "README.md") '("README.md"))
    (check-equal? (glob/cwd "README.md?") '())
    (check-equal? (glob/cwd "RE[AD]ME.md?") '())
    (check-equal? (glob/cwd "README.[md][d]") '("README.md"))
    (check-equal? (glob/cwd "*.txt") '("A.txt" "B.txt" "C.txt"))
    (check-equal? (glob/cwd "*.t*t") '("A.txt" "B.txt" "C.txt"))
    (check-equal? (glob/cwd "[AB].txt") '("A.txt" "B.txt"))
    (check-equal? (glob/cwd "./[AB].txt") '("A.txt" "B.txt"))
    (check-equal? (glob/cwd ".//[AB].txt") '("A.txt" "B.txt"))
    (check-equal? (glob/cwd ".//[].txt") '())

    (check-equal? (glob "no-dir/*") '())
    (check-equal? (glob "REED.md") '())
    (check-equal? (glob "R*.*.md") '()))

  (test-case "glob:dotfiles"
    (check-equal? (glob "*.rkt") '())
    (check-equal? (glob/cwd ".*.rkt") '(".secret1.rkt" ".secret2.rkt"))
    (check-equal? (glob/cwd ".*") '(".secret1.rkt" ".secret2.rkt"))
    (check-equal? (glob/cwd "?*.rkt") '())
    (check-equal? (glob/cwd "[.?]*") '())

    (check-equal?
      (glob/cwd "?*.rkt" #:capture-dotfiles? #t)
      '(".secret1.rkt" ".secret2.rkt"))
    (check-equal?
      (glob/cwd "*rkt" #:capture-dotfiles? #t)
      '(".secret1.rkt" ".secret2.rkt"))

    (parameterize ([glob-capture-dotfiles? #t])
      (check-equal? (glob/cwd "?*.rkt") '(".secret1.rkt" ".secret2.rkt"))
      (check-equal? (glob/cwd "*rkt") '(".secret1.rkt" ".secret2.rkt"))
      (check-equal? (glob/cwd "?*.rkt" #:capture-dotfiles? #f) '())))

  (test-case "glob:subdir"
    (check-equal? (glob/cwd "*/*.txt") '("A1.txt" "A2.txt" "A3.txt"))
    (check-equal? (glob/cwd "*/*/*ee*") '("deep.c"))

    (let ([d (build-path glob-test-subdir)])
      (check-equal?
        (glob/cwd (build-path d "*.txt"))
        '("A1.txt" "A2.txt" "A3.txt"))
      (check-equal? (glob/cwd (build-path d "A1*.txt")) '("A1.txt")))

    (let ([str (path->string glob-test-subdir)])
      (check-equal?
        (glob (format "~a/A2.txt" str))
        (list (simplify-path (build-path glob-test-subdir "A2.txt"))))
      (check-equal? (glob/cwd (format "~a//A2.txt" str)) '("A2.txt"))
      (check-equal? (glob/cwd (build-path str ".." "A*.txt")) '("A.txt"))))

  (test-case "glob:**"
    (let ([subdir-dotfiles '(".secret3.rkt" ".secret4.rkt")]
          [sub-subdir-dotfiles '(".deep.secret")]
          [subdir-txtfiles '("A1.txt" "A2.txt" "A3.txt")]
          [sub-subdir-txtfiles '("A4.txt")])
      (check-equal?
        (glob/cwd "**/.*")
        (append subdir-dotfiles sub-subdir-dotfiles))
      (check-equal? (glob/cwd "*/**/.*") sub-subdir-dotfiles)
      (check-equal?
        (glob/cwd (build-path "**" "*txt"))
        (append subdir-txtfiles sub-subdir-txtfiles))
      (check-equal? (glob/cwd "*/**/*txt") sub-subdir-txtfiles)))

  (test-case "glob:ends-with-path-sep"
    (check-equal? (glob/cwd "*/") '("glob-test-subdir"))
    (check-equal? (glob/cwd "*/*/") '("glob-test-sub-sub-dir")))

  (test-case "glob:multi"
    (check-equal?
      (glob/cwd '("*.txt" "*.md"))
      '("A.txt" "B.txt" "C.txt" "README.md"))
    (check-equal?
      (glob/cwd '("*.{txt,md}"))
      '("A.txt" "B.txt" "C.txt" "README.md"))
    (check-equal?
      (glob/cwd "*.{txt,md}")
      '("A.txt" "B.txt" "C.txt" "README.md")))

  (test-case "in-glob"
    (let ([seq (in-glob "*/.*")])
      (check-false (list? seq))
      (check-equal?
        (for/list ([x seq]) x)
        (for/list ([name (in-list '(".secret3.rkt" ".secret4.rkt"))])
          (simplify-path (build-path glob-test-subdir name)))))
    (check-equal?
      (for/list ([x (in-glob (build-path glob-test-subdir "{.,?}*"))]) x)
      (map (λ (p) (simplify-path (build-path glob-test-subdir p)))
           '(".secret3.rkt" ".secret4.rkt" "A1.txt" "A2.txt" "A3.txt"
             "glob-test-sub-sub-dir")))
    (check-equal?
      (for/list ([x (in-glob (map (λ (p) (build-path glob-test-sub-sub-dir p))
                                  '(".*" "*.c")))])
        x)
      (map (λ (p) (simplify-path (build-path glob-test-sub-sub-dir p)))
           '(".deep.secret" "deep.c"))))

  (test-case "glob-match?"
    (check-true (glob-match? "A*" "A.txt"))
    (check-true (glob-match? "A*" (format "~a/A.txt" glob-test-dir)))
    (check-true (glob-match? (build-path "A*")
                             (build-path glob-test-dir "A.txt")))

    (define (check-glob-match?-theorem g p)
      (if (member (path->complete-path (simplify-path p)) (glob g))
        (check-true
          (glob-match? g p)
          #;(format "glob '~a' should match '~a'" g p))
        (check-false
          (glob-match? g p)
          #;(format "glob '~a' should not match '~a'" g p))))

    (check-glob-match?-theorem
      (build-path glob-test-dir "README.md")
      "README.md")
    (check-glob-match?-theorem
      (build-path glob-test-subdir ".secret*")
      (format "~a/.secret3.rkt" glob-test-subdir))
    (check-glob-match?-theorem
      (build-path glob-test-subdir ".secret*")
      (format "~a/.mismatch" glob-test-subdir)))

  (test-case "glob-match?:dotfiles"
    (check-true (glob-match? ".secret*" ".secret1"))
    (check-false (glob-match? "*" ".secret.1"))
    (check-true (glob-match? "*" ".secret.1" #:capture-dotfiles? #t))
    (parameterize ([glob-capture-dotfiles? #t])
      (check-true (glob-match? "*" ".secret.1")))

    (check-true
      (glob-match? "*/.se?ret[43]*" (build-path glob-test-subdir ".secret3")))
    (check-true
      (glob-match? "*/.se?ret[43]*" (build-path glob-test-subdir ".secret4")))
  )

  (test-case "glob-match?-pun"
    ;; -- Check that `glob-match?` fails if the unambiguous parts don't match
    ;;    (but the ambiguous parts do)
    (check-false (glob-match? (format "~a/*" glob-test-subdir) "README.md"))
    (check-true (glob-match? (format "~a/*" glob-test-dir) "README.md"))
    (check-false (glob-match? (format "~a/*" glob-test-subdir) "A.txt"))
    (check-true
      (glob-match? (build-path glob-test-subdir "*")
                   (format "~a/A1.txt" glob-test-subdir))))

  (test-case "glob-match?:unamb"
    (check-true (glob-match? "foo.rkt" "foo.rkt"))
    (check-true (glob-match? '("foo.rkt") "foo.rkt"))
    (check-true (glob-match? '("foo.rkt" "foo.rkt") "foo.rkt"))
    (check-true (glob-match? "." "."))
    (check-true (glob-match? "longerfilename" "longerfilename"))

    (check-false (glob-match? "A" "x"))
    (check-false (glob-match? '("1" "2" "3") "4"))
  )

  (test-case "glob-match?:recursive"
    (check-true (glob-match? "**.rkt" "foo.rkt"))
    (check-true (glob-match? "**" "yolo"))
    (check-true (glob-match? "**" ".dotfile" #:capture-dotfiles? #t))
    (check-true (glob-match? "/**.rkt" "/a/b/c//d/./../foo.rkt"))
    (check-true (glob-match? "a/b/**/c" "a/b/c/d/e/f/c"))

    (check-false (glob-match? "**.rkt" "foo"))
    (check-false (glob-match? "**" "yo/.lo"))
    (check-false (glob-match? "**" ".yolo"))
    (check-false (glob-match? "**" ".yo/lo"))
    (check-false (glob-match? "a/b/**/c" "a/b/c/d/e")))

  (test-case "glob-match?:split"
    (check-true (glob-match? "a/*.rkt" "a/foo.rkt"))
    (check-true (glob-match? "????" "yolo"))
    (check-true (glob-match? "*f[iou]?e" ".dotfile" #:capture-dotfiles? #t))
    (check-true (glob-match? "/*/*/*/*.rkt" "/a/b/c//d/./../foo.rkt"))
    (check-true (glob-match? "a/b/*/c" "a/b/d/c"))

    (check-false (glob-match? "*.rkt" "foo"))
    (check-false (glob-match? "*/*" "yo/.lo"))
    (check-false (glob-match? "*" ".yolo"))
    (check-false (glob-match? "*/*" ".yo/lo"))
    (check-false (glob-match? "a/b/*/c" "a/b/c/d/")))

  (test-case "glob-match?:ends-with-path-sep"
    (check-true (glob-match? "*/" glob-test-subdir))
    (check-true (glob-match? "glob*/glob*/" glob-test-sub-sub-dir))
    (check-true (glob-match? "foo/" "foo/"))
    (check-true (glob-match? "*/" "foo/"))

    (check-false (glob-match? "foo/" "foo"))
    (check-false (glob-match? "*/" "foo")))

)
