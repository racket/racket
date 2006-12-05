
(module unpack mzscheme

  (require (lib "etc.ss")
	   (lib "inflate.ss")
	   (lib "file.ss")
           (lib "list.ss")
	   (lib "base64.ss" "net")
	   (lib "getinfo.ss" "setup")
	   "dirs.ss")

  ;; Returns a port and a kill thunk
  (define (port64gz->port p64gz)
    ;; Inflate in a thread so the whole input isn't read at once
    (let-values ([(base64-out base64-in) (make-pipe 4096)]
		 [(guz-out guz-in) (make-pipe 4096)])
      (let ([64t
	     (thread (lambda ()
		       (dynamic-wind
                         void
                         (lambda () (base64-decode-stream p64gz base64-in))
                         (lambda () (close-output-port base64-in)))))]
	    [gzt
	     (thread (lambda ()
		       (dynamic-wind
                         void
                         (lambda () (gunzip-through-ports base64-out guz-in))
                         (lambda () (close-output-port guz-in)))))])
	(values guz-out (lambda () (kill-thread 64t) (kill-thread gzt))))))

  (define (pretty-name f)
    (with-handlers ([void (lambda (x) f)])
      (let-values ([(base name dir?) (split-path f)])
	(format "~a in ~a"
                (path->string name)
                (if (path? base) (path->string base) base)))))

  (define (shuffle-path parent-dir get-dir shuffle? v)
    (if shuffle?
      ;; Re-arrange for "collects', etc.
      (let ([v (remq* '(same) v)])
        (if (null? v)
          (values #f 'same)
          (let ([dir
                 (case (string->symbol (car v))
                   [(collects) (get-dir find-collects-dir find-user-collects-dir)]
                   [(doc)     (get-dir find-doc-dir find-user-doc-dir)]
                   [(lib)     (get-dir find-lib-dir find-user-lib-dir)]
                   [(include) (get-dir find-include-dir find-user-include-dir)]
                   [else #f])])
            (if dir
              (if (null? (cdr v))
                (values dir 'same)
                (values dir (apply build-path (cdr v))))
              (values parent-dir (apply build-path v))))))
      (values parent-dir (if (null? v) 'same (apply build-path v)))))

  (define (unmztar p filter parent-dir get-dir shuffle? print-status)
    (define bufsize 4096)
    (define buffer (make-bytes bufsize))
    (let loop ()
      (let ([kind (read p)])
	(unless (eof-object? kind)
	  (case kind
	    [(dir) (let-values ([(target-dir s)
                                 (shuffle-path parent-dir get-dir shuffle? (read p))])
		     (unless (or (eq? s 'same) (relative-path? s))
		       (error "expected a directory name relative path string, got" s))
		     (when (or (eq? s 'same) (filter 'dir s target-dir))
		       (let ([d (build-path target-dir s)])
			 (unless (directory-exists? d)
			   (print-status
                            (format "  making directory ~a" (pretty-name d)))
			   (make-directory* d)))))]
	    [(file file-replace)
	     (let-values ([(target-dir s)
                           (shuffle-path parent-dir get-dir shuffle? (read p))])
	       (unless (relative-path? s)
		 (error "expected a file name relative path string, got" s))
	       (let ([len (read p)])
		 (unless (and (number? len) (integer? len))
		   (error "expected a file name size, got" len))
		 (let* ([write? (filter kind s target-dir)]
			[path (build-path target-dir s)])
		   (let ([out (and write?
				   (if (file-exists? path)
                                     (if (eq? kind 'file)
                                       #f
                                       (open-output-file path 'truncate))
                                     (open-output-file path)))])
		     (when (and write? (not out))
		       (print-status (format "  skipping ~a; already exists" (pretty-name path))))
		     (when out
		       (print-status (format "  unpacking ~a" (pretty-name path))))
		     ;; Find starting *
		     (let loop ()
		       (let ([c (read-char p)])
			 (cond [(char=? c #\*) (void)] ; found it
                               [(char-whitespace? c) (loop)]
                               [(eof-object? c) (void)] ; signal the error below
                               [else (error 
                                      (format "unexpected character setting up ~a, looking for *"
                                              path)
                                      c)])))
		     ;; Copy file data
                     (let loop ([n len])
                       (unless (zero? n)
                         (let ([l (read-bytes! buffer p 0 (min n bufsize))])
                           (when (eof-object? l)
                             (error (format
                                     "unexpected end-of-file while ~a ~a (at ~a of ~a)"
                                     (if out "unpacking" "skipping")
                                     path
                                     (- len n -1) len)))
                           (when out (write-bytes buffer out 0 l))
                           (loop (- n l)))))
		     (when out (close-output-port out))))))]
	    [else (error "unknown file tag" kind)])
	  (loop)))))

  (define (call-info info flag mk-default test)
    (if info
      (let ([v (info flag mk-default)]) (test v) v)
      (mk-default)))

  (define unpack
    (opt-lambda (archive [main-collects-parent-dir (current-directory)]
                         [print-status (lambda (x) (printf "~a\n" x))]
			 [get-target-directory (lambda () (current-directory))]
                         [force? #f]
			 [get-target-plt-directory
                          (lambda (preferred main-collects-parent-dir options)
                            preferred)])
      (let*-values ([(p64gz) (open-input-file archive)]
		    [(p kill) (port64gz->port p64gz)])
	(dynamic-wind
          void
          (lambda ()
            (unless (and (eq? #\P (read-char p))
                         (eq? #\L (read-char p))
                         (eq? #\T (read-char p)))
              (error "not an unpackable distribution archive"))
            (let* ([n (make-namespace)]
                   [info (let ([orig (current-namespace)])
                           (parameterize ([current-namespace n])
                             (namespace-require '(lib "unit200.ss"))
                             (eval (read p))))])
              (unless (and (procedure? info)
                           (procedure-arity-includes? info 2))
                (error "expected a procedure of arity 2, got" info))
              (let ([name (call-info info 'name (lambda () #f)
                                     (lambda (n) 
                                       (unless (string? n)
                                         (if n
                                           (error "couldn't find the package name")
                                           (error "expected a string")))))]
                    [unpacker (call-info info 'unpacker (lambda () #f)
                                         (lambda (n) 
                                           (unless (eq? n 'mzscheme)
                                             (error "unpacker isn't mzscheme:" n))))]
                    [target-dir-info
                     (let ([rel? (call-info info 'plt-relative? (lambda () #f) values)]
                           [not-user-rel? (call-info info 'plt-home-relative? (lambda () #f) values)]
                           [test-dirs (call-info info 'test-plt-dirs (lambda () #f) values)])
                       (if rel?
                         ;; Shuffling...
                         (if (and not-user-rel? 
                                  ;; Check for void because old unpacker didn't use
                                  ;;  the failure thunk.
                                  (not (void? not-user-rel?))
                                  ;; Non-user optional if test-dirs are writable
                                  (or (not test-dirs)
                                      (andmap
                                       (lambda (p)
                                         (and (string? p)
                                              (let ([dir (let-values ([(base dir)
                                                                       (shuffle-path main-collects-parent-dir
                                                                                     (lambda (a b) (a))
                                                                                     #t (list p))])
                                                           (build-path base dir))])
                                                (memq 'write
                                                      (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
                                                        (file-or-directory-permissions dir))))))
                                       test-dirs)))
                           ;; Shuffle to main directory always:
                           (let ([dir (get-target-plt-directory main-collects-parent-dir
                                                                main-collects-parent-dir
                                                                (list main-collects-parent-dir))])
                             (list dir (lambda (sys user)
                                         (let ([a (sys)])
                                           (get-target-plt-directory a a (list a))))))
                           ;; Prefer to shuffle to user directory:
                           (let ([addons (find-user-collects-dir)])
                             (let ([dir (get-target-plt-directory
                                         addons
                                         main-collects-parent-dir
                                         (list addons main-collects-parent-dir))])
                               (list dir (lambda (sys user)
                                           (let ([a (sys)]
                                                 [b (user)])
                                             (get-target-plt-directory b a (list b a))))))))
                         ;; No shuffling --- install to target directory:
                         (list (get-target-directory))))])

                ;; Stop if no target directory:
                (if (car target-dir-info)

                  ;; Check declared dependencies (none means v103)
                  (begin
                    (call-info info 'requires (lambda () null)
                               (lambda (l) 
                                 (define (bad)
                                   (error "`requires' info is corrupt:" l))
                                 (when (void? l)
                                   (if force?
                                     (print-status "warning: archive is for an older version of PLT Scheme")
                                     (error "cannot install; archive is for an older version of PLT Scheme")))
                                 (unless (or (list? l) (and force? (void? l)))
                                   (bad))
                                 ;; Check each dependency:
                                 (when (list? l)
                                   (for-each
                                    (lambda (d)
                                      (unless (and (list? d) (= 2 (length d)))
                                        (bad))
                                      (let ([coll-path (car d)]
                                            [version (cadr d)])
                                        (unless (and (pair? coll-path)
                                                     (list? coll-path)
                                                     (andmap string? coll-path)
                                                     (list? version)
                                                     (andmap number? version))
                                          (bad))
                                        (with-handlers ([exn:fail:filesystem?
                                                         (lambda (x)
                                                           (if force?
                                                             (print-status 
                                                              (format "warning: missing required collection ~s" coll-path))
                                                             (error "cannot install; missing required collection" coll-path)))])
                                          (apply collection-path coll-path))
                                        (let ([inst-version 
                                               (with-handlers ([void (lambda (x) 
                                                                       (if (exn:break? x)
                                                                         (raise x)
                                                                         null))])
                                                 (let ([info (get-info coll-path)])
                                                   (info 'version (lambda () null))))])
                                          (let loop ([v version][iv inst-version])
                                            (unless (null? v)
                                              (when (or (null? iv)
                                                        (not (= (car v) (car iv))))
                                                (let ([msg (format "version ~a of collection ~s is required, but version ~a is installed"
                                                                   version coll-path 
                                                                   (if (null? inst-version)
                                                                     '<unknown>
                                                                     inst-version))])
                                                  (if force?
                                                    (print-status (format "warning: ~a" msg))
                                                    (error (format "cannot install; ~a" msg)))))
                                              (loop (cdr v) (cdr iv)))))))
                                    l))))

                    ;; Check for conflicts:
                    (call-info info 'conflicts (lambda () null)
                               (lambda (l) 
                                 (define (bad)
                                   (error "`conflicts' info is corrupt:" l))
                                 (unless (or (list? l) (and force? (void? l)))
                                   (bad))
                                 (when (list? l)
                                   (for-each
                                    (lambda (coll-path)
                                      (unless (and (pair? coll-path)
                                                   (list? coll-path)
                                                   (andmap string? coll-path))
                                        (bad))
                                      (when (with-handlers ([exn:fail? (lambda (x) #f)])
                                              (apply collection-path coll-path))
                                        (error "cannot install; conflict with installed collection"
                                               coll-path)))
                                    l))))

                    (unless (and name unpacker)
                      (error "bad name or unpacker"))
                    (print-status (format "Unpacking ~a from ~a" name archive))
                    (let ([u (eval (read p) n)])
                      (unless (eval `(unit? ,u) n)
                        (error "expected a v200 unit, got" u))
                      (make-directory* (car target-dir-info))
                      (let ([unmztar (lambda (filter)
                                       (unmztar p filter 
                                                (car target-dir-info) 
                                                (lambda (a b)
                                                  ((cadr target-dir-info) a b))
                                                ((length target-dir-info) . > . 1)
                                                print-status))])
                        (eval `(invoke-unit ,u ,(car target-dir-info) ,unmztar) n))))

                  ;; Cancelled: no collections
                  null))))
          (lambda () (kill) (close-input-port p64gz))))))

  (provide unpack))
