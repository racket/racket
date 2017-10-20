
(load-relative "loadtest.rktl")

(Section 'filelib)

(require racket/file
	 racket/system
	 racket/list)

(define tmp-dir (make-temporary-file "filelib~a" 'directory))
(define tmp-name (build-path tmp-dir "tmp0-filelib"))
(when (file-exists? tmp-name) (delete-file tmp-name))
(display-lines-to-file '("a" "b" "c") tmp-name #:separator #"\r\n" #:mode 'binary)
(test '(a b c) file->list tmp-name)
(test '("a\r" "b\r" "c\r") file->list tmp-name read-line)
(test '("a" "b" "c") file->list tmp-name (lambda (p) (read-line p 'any)))
(test "a\r\nb\r\nc\r\n" file->string tmp-name #:mode 'binary)
(test #"a\r\nb\r\nc\r\n" file->bytes tmp-name)
(test '("a" "b" "c") file->lines tmp-name)
(test '(#"a" #"b" #"c") file->bytes-lines tmp-name)
(test '("a" "b" "c") file->lines tmp-name #:line-mode 'any #:mode 'binary)
(test '(#"a" #"b" #"c") file->bytes-lines tmp-name #:line-mode 'any #:mode 'text)
(err/rt-test (display-to-file #"a\nb" tmp-name) exn:fail:filesystem:exists?)
(display-to-file #"a\nb" tmp-name #:exists 'truncate)
(test #"a\nb" file->bytes tmp-name)
(display-to-file "\u03BB" tmp-name #:exists 'truncate)
(test #"\316\273" file->bytes tmp-name)
(write-to-file "\u03BB" tmp-name #:exists 'truncate)
(test #"\"\316\273\"" file->bytes tmp-name)
(test "\u03BB" file->value tmp-name)
(when (file-exists? tmp-name) (delete-file tmp-name))
(delete-directory tmp-dir)

(define-syntax-rule (err/rt-chk-test (op arg ...))
  (err/rt-test (op arg ...) (check-msg 'op)))
(define (check-msg op)
  (lambda (exn)
    (regexp-match (format "^~a: " op) (exn-message exn))))
(err/rt-chk-test (file->string 'x))
(err/rt-chk-test (file->bytes 'x))
(err/rt-chk-test (file->string "x" #:mode 'other))
(err/rt-chk-test (file->bytes "x" #:mode 'other))
(err/rt-chk-test (file->value "x" #:mode 'other))
(err/rt-chk-test (display-lines-to-file 10 "x"))
(err/rt-chk-test (display-lines-to-file '(10) "x" #:mode 'other))
(err/rt-chk-test (display-lines-to-file '(10) "x" #:exists 'other))
(err/rt-chk-test (file->lines "x" #:line-mode 'junk))
(err/rt-chk-test (file->lines "x" #:mode 'other))
(err/rt-chk-test (file->bytes-lines "x" #:line-mode 'junk))
(err/rt-chk-test (file->bytes-lines "x" #:mode 'other))
(err/rt-chk-test (display-to-file "y" "x" #:exists 'other))
(err/rt-chk-test (display-to-file "y" "x" #:mode 'other))
(err/rt-chk-test (write-to-file #"y" "x" #:exists 'other))
(err/rt-chk-test (write-to-file #"y" "x" #:mode 'other))
(err/rt-chk-test (display-lines-to-file 'y "x"))
(err/rt-chk-test (display-lines-to-file '(y) "x" #:exists 'other))
(err/rt-chk-test (display-lines-to-file '(y) "x" #:mode 'other))

;; ----------------------------------------

(parameterize ([current-directory (current-load-relative-directory)])
  (let ([rel (find-files values)]
	[abs (find-files values (current-directory))])
    (test #t = (length rel) (sub1 (length abs)))
    (test #f member "filelib.rktl" abs)
    (test #f null? (member "filelib.rktl" rel))
    (test #f null? (member (build-path (current-directory) "filelib.rktl") abs))

    (test (list (string->path "filelib.rktl")) find-files (lambda (f) (regexp-match "^filelib[.]rktl$" (path->string f))))
    (test (list (build-path (current-directory) "filelib.rktl"))
	  find-files (lambda (f) (regexp-match "filelib[.]rktl$" (path->string f)))
	  (current-directory))
    ;; check that path as string gives paths (not strings) to checker and result:
    (test (list (current-directory)
                (build-path (current-directory) "filelib.rktl"))
	  find-files (lambda (f) (or (equal? f (current-directory))
                                     (regexp-match "filelib[.]rktl$" (path->string f))))
	  (path->string (current-directory)))

    (let ([rel2 (fold-files (lambda (name kind accum)
			      (test kind name (if (file-exists? name)
                                                  'file
                                                  'dir))
			      (cons name accum))
			    null)]
	  [sort (lambda (l)
		  (sort l (lambda (a b)
			    (bytes<? (path->bytes a) (path->bytes b)))))])
      (test #t equal? (sort rel) (sort rel2))

      (unless (eq? (system-type) 'windows)
        (define tmp-dir (make-temporary-file "filelib~a" 'directory))
        (define (touch . elems)
          (call-with-output-file
           (apply build-path elems)
           void))
        
        (copy-file "filelib.rktl" (build-path tmp-dir "filelib.rktl"))
        (make-directory (build-path tmp-dir "sub"))
        (touch tmp-dir "a")
        (touch tmp-dir "b")
        (touch tmp-dir "sub" "x")
         
        (parameterize ([current-directory tmp-dir])
          (define rel2 (fold-files (lambda (name kind accum)
                                     (test kind name (if (file-exists? name)
                                                         'file
                                                         'dir))
                                     (cons name accum))
                                   null))
          
          (make-file-or-directory-link "filelib.rktl" "filelib-link")
          (make-file-or-directory-link "." "loop-link")

          (test (+ 2 (length rel2))
                fold-files 
                (lambda (name kind accum)
                  (test kind values (cond
                                     [(link-exists? name) 'link]
                                     [(file-exists? name) 'file]
                                     [(directory-exists? name) 'dir]
                                     [else '???]))
                  (when (member name '("filelib-link" "loop-link"))
                    (test kind name 'link))
                  (add1 accum))
                0
                #f
                #f)

          (test (+ 2 (length rel2))
                fold-files 
                (lambda (name kind accum)
                  (test kind values (cond
                                     [(link-exists? name) 'link]
                                     [(file-exists? name) 'file]
                                     [(directory-exists? name) 'dir]
                                     [else '???]))
                  (when (member name '("filelib-link" "loop-link"))
                    (test kind name 'link))
                  (values (add1 accum) #t))
                0
                #f
                #f)

          (delete-file "loop-link")

          (test (+ 1 (length rel2))
                fold-files 
                (lambda (name kind accum)
                  (test kind values (cond
                                     [(file-exists? name) 'file]
                                     [else 'dir]))
                  (when (member name '("filelib-link"))
                    (test kind name 'file))
                  (add1 accum))
                0
                #f
                #t)

          (delete-file "filelib-link")

          'done)
        (delete-directory/files tmp-dir)))))

;; ----------------------------------------

;;----------------------------------------------------------------------
;; File Locks
(define tempfile (make-temporary-file))
(err/rt-test (call-with-file-lock/timeout 10 'shared (lambda () #t) (lambda () #f)))
(err/rt-test (call-with-file-lock/timeout tempfile 'bogus (lambda () #t) (lambda () #f)))
(err/rt-test (call-with-file-lock/timeout tempfile 'shared (lambda (x) #t) (lambda () #f)))
(err/rt-test (call-with-file-lock/timeout tempfile 'exclusive (lambda () #t) (lambda (x) #f)))

(test #t call-with-file-lock/timeout tempfile 'shared (lambda () #t) (lambda () #f))
(test #t call-with-file-lock/timeout tempfile 'exclusive (lambda () #t) (lambda () #f))

(err/rt-test (call-with-file-lock/timeout tempfile 'exclusive (lambda ()
    (call-with-file-lock/timeout tempfile 'exclusive (lambda () #f) (lambda () (error))))
  (lambda () 'uhoh)))
(err/rt-test (call-with-file-lock/timeout tempfile 'exclusive (lambda ()
    (call-with-file-lock/timeout tempfile 'shared (lambda () #f) (lambda () (error))))
  (lambda () 'uhon)))
(err/rt-test (call-with-file-lock/timeout tempfile 'shared (lambda ()
    (call-with-file-lock/timeout tempfile 'exclusive (lambda () #f) (lambda () (error))))
  (lambda () 'uhoh)))
(test #t call-with-file-lock/timeout tempfile 'shared (lambda ()
    (call-with-file-lock/timeout tempfile 'shared (lambda () #t) (lambda () #f)))
  (lambda () 'uhoh))

(test (string->path (if (eq? (system-type) 'windows) "_LOCKstuff" ".LOCKstuff"))
      make-lock-file-name
      "stuff")
(test (string->path (if (eq? (system-type) 'windows) "_LOCKstuff" ".LOCKstuff"))
      make-lock-file-name
      "stuff")
(test (build-path "dir" (if (eq? (system-type) 'windows) "_LOCKstuff" ".LOCKstuff"))
      make-lock-file-name
      (build-path "dir" "stuff"))
(test (build-path "dir" (if (eq? (system-type) 'windows) "_LOCKstuff" ".LOCKstuff"))
      make-lock-file-name
      "dir"
      (string->path "stuff"))

(delete-file tempfile)
(delete-file (make-lock-file-name tempfile))

;;----------------------------------------------------------------------
;; Atomic output

(define (try-atomic-output fn)
  (call-with-output-file*
   fn
   #:exists 'truncate
   (lambda (o) (display "()" o)))
  (define ts
    (append
     ;; Writers
     (for/list ([i 10])
       (thread (lambda ()
                 (for ([j 100])
                   (call-with-atomic-output-file
                    fn
                    (lambda (o tmp-path)
                      (test (or (path-only fn) (current-directory))
                            path-only tmp-path)
                      (display "(" o)
                      (flush-output o)
                      (sync (system-idle-evt))
                      (display ")" o)))))))
     ;; Readers
     (for/list ([i 10])
       (thread (lambda ()
                 (for ([j 100])
                   (sync (system-idle-evt))
                   (test '() call-with-input-file fn read)))))))
  (for-each sync ts)
  (delete-file fn))

(try-atomic-output (make-temporary-file))
;; The user's add-on directory should be writable and might be a
;; different filesystem, so try that:
(let ([addon-dir (find-system-path 'addon-dir)])
  (make-directory* addon-dir)
  (parameterize ([current-directory addon-dir])
    (try-atomic-output (format "atomic-output-~a" (current-inexact-milliseconds)))))

;; ----------------------------------------

(let ([dir (make-temporary-file "pathlist~a" 'directory)])
  (define parents
    (let loop ([dir dir])
      (define-values (base name dir?) (split-path dir))
      (if (path? base)
          (append (loop base) (list (path->directory-path dir)))
          (list dir))))
  (define (p . args)
    (maybe-as-directory
     args
     (apply build-path dir args)))
  (define (maybe-as-directory args p)
    (if (regexp-match? #rx"^d" (last args))
        (path->directory-path p)
        p))
  (define (touch f)
    (call-with-output-file* f void))
  (touch (p "f1"))
  (make-directory (p "d1"))
  (make-directory (p "d2"))
  (touch (p "d1" "f1"))
  (touch (p "d2" "f1"))
  (touch (p "d2" "f2"))

  (unless (eq? 'windows (system-type))
    (make-file-or-directory-link "d1" (p "l3"))
    (make-file-or-directory-link "l3" (p "l4"))
    (make-directory (p "d5"))
    (make-file-or-directory-link (build-path 'up "d2" "f1") (p "d5" "l5")))

  (make-directory (p "d6"))
  (touch (p "d6" "f1"))
  (make-directory (p "d6" "d7"))
  (touch (p "d6" "d7" "f1"))
  (touch (p "d6" "d7" "f2"))

  (define (check p parents)
    (test (append
           parents
           (list (p "d1")
                 (p "d1" "f1")))
          pathlist-closure
          (list (p "d1")))
    (test (append
           parents
           (list (p "d1")
                 (p "d1" "f1")
                 (p "f1")))
          pathlist-closure
          (list (p "d1")
                (p "f1")))
    (test (append
           parents
           (list (p "d1")
                 (p "d2")
                 (p "d2" "f2")))
          pathlist-closure
          (list (p "d1")
                (p "d2"))
          #:path-filter (lambda (f) (not (regexp-match? #rx"f1$" f))))
    (test (append
           parents
           (list (p "d1")
                 (p "d1" "f1")
                 (p "d2")
                 (p "d2" "f2")))
          pathlist-closure
          (list (p "d1")
                (p "d1" "f1")
                (p "d2"))
          #:path-filter (lambda (f) (not (regexp-match? #rx"f1$" f))))
    (test (append
           parents
           (list (p "d6")
                 (p "d6" "f1")))
          pathlist-closure
          (list (p "d6"))
          #:path-filter (lambda (f) (not (regexp-match? #rx"d7$" f))))
    (unless (eq? 'windows (system-type))
      (test (append
             parents
             (list (p "l3")))
            pathlist-closure
            (list (p "l3")))
      (test (append
             parents
             (list (p "l4")))
            pathlist-closure
            (list (p "l4")))
      (test (append
             parents
             (list (p "d5")
                   (p "d5" "l5")))
            pathlist-closure
            (list (p "d5" "l5")))
      (test (append
             parents
             (list (p "d1")
                   (p "d1" "f1")))
            pathlist-closure
            (list (p "l3"))
            #:follow-links? #t)
      (test (append
             parents
             (list (p "d1")
                   (p "d1" "f1")))
            pathlist-closure
            (list (p "l4"))
            #:follow-links? #t)
      (test (append
             parents
             (list (p "d2")
                   (p "d2" "f1")))
            pathlist-closure
            (list (p "d5" "l5"))
            #:follow-links? #t)))
       
  (parameterize ([current-directory dir])
    (check (lambda args (maybe-as-directory args (apply build-path args))) null))
  (check p parents)
  

  (delete-directory/files dir))

;; ----------------------------------------
;; Check 'text mode conversion
;; The content of "text.rktd" once triggered a bug in 'text
;; mode conversion on Windows by having line breaks as just
;; the right place

(let ()
  (define text (call-with-input-file (build-path (current-load-relative-directory)
                                                 "text.rktd")
                                     read))
  (define tmp (make-temporary-file "tmp-data~a"))
  (call-with-output-file tmp #:exists 'truncate (lambda (o) (write-string text o)))
  (define str (file->string tmp #:mode 'text))
  (test #t values (equal? str (if (eq? (system-type) 'windows)
                                  (regexp-replace* #rx"\r\n" text "\n")
                                  text))))

;; ----------------------------------------

(report-errs)
