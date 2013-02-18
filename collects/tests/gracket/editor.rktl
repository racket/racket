
(load-relative "loadtest.rktl")

(require racket/gui/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Editor Tests                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; File and port save/load tests

(define (run-save/load-tests editor% insert reset?)
  (when reset?
    (map (lambda (f)
	   (when (file-exists? f) (delete-file f)))
	 '("tmp99" "tmp98" "tmp97" "tmp96" "tmp95")))
  (let* ([mode #f]
	 [editor+% (class editor%
		     (define/augment (on-load-file path -mode)
		       (set! mode -mode))
		     (define/augment (on-save-file path -mode)
		       (set! mode -mode))
		     (super-new))]
	 [e (make-object editor+%)]
	 [ck-text (lambda (path)
		    (when (eq? editor% text%)
		      (st 'text e get-file-format)
		      (when path
			(test "Hello" 'content (with-input-from-file path
						 (lambda ()
						   (read-string 100)))))))]
	 [ck-binary (lambda (path)
		      (when (eq? editor% text%)
			(st 'standard e get-file-format))
		      (when path
			(test #"#reader(lib\"read.ss\"\"wxme\")WXME" 'content (with-input-from-file path
                                                                                (lambda ()
                                                                                  (read-bytes 31))))))]
	 [ck-mode (lambda (-mode)
		    (test (if (eq? editor% pasteboard%) 
			      (if (eq? -mode 'copy)
				  'copy
				  'standard) 
			      -mode) 
			  'mode mode))])
    (insert e "Hello")
    (st #t e is-modified?)
    (st #f e get-filename)
    (when (eq? editor% text%)
      (st 'standard e get-file-format))
    (st #t e save-file "tmp99" 'text)
    (ck-mode 'text)
    (st (string->path "tmp99") e get-filename)
    (st #f e is-modified?)
    (ck-text "tmp99")
    (insert e "Ack")
    (st #t e is-modified?)
    (st #t e load-file "tmp99" 'guess)
    (ck-mode 'guess)
    (ck-text #f)
    (st #f e is-modified?)
    (st "Hello" e get-flattened-text)
    (let ([now (file-or-directory-modify-seconds "tmp99")])
      (st #t e save-file "tmp99" 'same)
      (ck-text "tmp99")
      (st (string->path "tmp99") e get-filename)
      (let ([later (file-or-directory-modify-seconds "tmp99")])
	(test #t 'file-date (now . <= . later))
	(st #t e save-file "tmp98" 'standard)
	(test #f 'file-date (later . < . (file-or-directory-modify-seconds "tmp99")))
	(ck-binary "tmp98")
	(st #t e load-file "tmp98" 'guess)
	(ck-mode 'guess)
	(ck-binary #f)
	(st #t e load-file "tmp98" 'text)
	(ck-mode 'text)
	(ck-text #f)
	(when (eq? editor% text%)
          (st "#reader(lib\"read.ss\"\"wxme\")WXME"  e get-text 0 31))
	(st #t e load-file "tmp98" 'same)
	(ck-mode 'same)
	(ck-text #f)
        (when (eq? editor% text%)
          (st "#reader(lib\"read.ss\"\"wxme\")WXME"  e get-text 0 31))
	(st #t e load-file "tmp98" 'guess)
	(ck-mode 'guess)
	(ck-binary #f)
	(st "Hello" e get-flattened-text)))
    (let ([target "tmp97"])
      ;; Check [non-]temporary file names
      (set! e (make-object (class editor+%
			     (define/override (put-file file dir)
			       (string->path target))
			     (super-new))))
      (insert e "Howdy")
      (st #t e is-modified?)
      (set! mode #f)
      (st #t e save-file #f 'copy)
      (ck-mode 'copy)
      (set! target "tmp95")
      (st #t e is-modified?)
      (ck-binary "tmp97")
      (stv e set-filename "tmp96")
      (st #t e save-file #f)
      (st #f e is-modified?)
      (st (string->path "tmp96") e get-filename)
      (ck-binary "tmp96")
      (stv e set-filename "tmp96" #t)
      (st #t e save-file #f)
      (ck-mode 'same)
      (st (string->path "tmp95") e get-filename)
      (stv e set-filename "tmp96" #t)
      (st #t e save-file "")
      (st (string->path "tmp95") e get-filename)
      (ck-binary "tmp95")
      (st "Howdy" e get-flattened-text)
      (when (eq? editor% text%)
	(stv e set-position 100))
      (st #t e insert-file "tmp98")
      (st "HowdyHello" e get-flattened-text)
      (st #t e insert-file "tmp99")
      (st "HowdyHelloHello" e get-flattened-text)
      (st (string->path "tmp95") e get-filename))))
    
(define text-insert (lambda (e t) (send e insert t)))
(define pb-insert (lambda (e t) (send e insert (make-object string-snip% t))))

(map (lambda (reset?)
       (run-save/load-tests text% text-insert reset?)
       (run-save/load-tests pasteboard% pb-insert reset?))
     '(#t #f))

;; Test DrRacket-style format change in `on-save':
(define (run-on-save-tests editor% insert)
  (let* ([editor+% (if (eq? editor% text%)
		       (class editor%
			 (inherit set-file-format)
			 (define/augment (on-save-file path -mode)
			   (set-file-format 'standard))
			 (super-new))
		       editor%)]
	 [e (make-object editor+%)])
    (insert e "Hello")
    (st #t e is-modified?)
    (st #f e get-filename)
    (when (eq? editor% text%)
      (stv e set-file-format 'text)
      (st 'text e get-file-format))
    (st #t e save-file "tmp99" 'same)
    (when (eq? editor% text%)
      (st 'standard e get-file-format))
    (send e load-file "tmp99" 'guess)
    (st "Hello" e get-flattened-text)))
(run-on-save-tests text% text-insert)
(run-on-save-tests pasteboard% pb-insert)

;;;;;; Undo tests

(define e (make-object text%))
(send e set-max-undo-history 1024)

(stv e insert "Hello")
(st #t e is-modified?)
(stv e undo)
(st #f e is-modified?)
(stv e redo)
(st #t e is-modified?)
(stv e set-modified #f)
(st #f e is-modified?)
(stv e undo)
(st #t e is-modified?)
(stv e redo)
(st #f e is-modified?)
(stv e undo)
(st #t e is-modified?)
(stv e redo)
(st #f e is-modified?)
(stv e undo)
(st "" e get-text)
(stv e set-modified #f)
(st #f e is-modified?)
(stv e redo) ; somehow cancel's redo action to set modified to #f...
(st #t e is-modified?)
(st "Hello" e get-text)
(define undone? #f)
(stv e add-undo (letrec ([f (lambda ()
			      (set! undone? #t)
			      (send e add-undo f) ; reinstall self!
			      #f)])
		  f))
(stv e undo)
(st "Hello" e get-text)
(test #t 'undone? undone?)
(stv e undo)
(st "" e get-text)
(set! undone? #f)
(stv e redo)
(st "Hello" e get-text)
(test #f 'undone? undone?)
(stv e redo)
(st "Hello" e get-text)
(test #t 'undone? undone?)
(set! undone? #f)
(stv e redo)
(st "Hello" e get-text)
(test #f 'undone? undone?)
(stv e insert "x")
(st "Hellox" e get-text)
(stv e add-undo (lambda ()
		  (set! undone? #t)
		  #t)) ; do next one, too
(stv e undo)
(test #t 'undone? undone?)
(st "Hello" e get-text)

;; Editor ports

(let ([e (make-object text%)]
      [res-mode? #f])
  (stv e insert "hello")
  (let ([p (open-input-text-editor e)])
    (test 'hello 'read (read p))
    (test eof 'read (read p)))
  (stv e insert " there")
  (let ([p (open-input-text-editor e)])
    (test 'hello 'read (read p))
    (test 'there 'read (read p))
    (test eof 'read (read p)))
  (stv e insert (make-object
		 (class* snip% 
		   (readable-snip<%>)
		   (define/public (read-special src line col pos)
		     (if res-mode?
			 'res
			 (error 'ack)))
		   (super-new))))
  (let ([p (open-input-text-editor e)])
    (port-count-lines! p)
    (test '(1 0 1) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test 'hello 'read (read p))
    (test '(1 5 6) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test 'there 'read (read p))
    (test '(1 11 12) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test 'got-ack 'read (with-handlers ([exn:fail? (lambda (x)
						      'got-ack)])
			   (read p)))
    (test '(1 12 13) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test eof 'read (read p)))
  (set! res-mode? #t)
  (let ([p (open-input-text-editor e)])
    (port-count-lines! p)
    (test 'hello 'read (read p))
    (test 'there 'read (read p))
    (test 'res 'read (read p))
    (test '(1 12 13) 'pos (call-with-values (lambda () (port-next-location p)) list))
    (test eof 'read (read p)))
  (stv e insert (make-object image-snip% (build-path
					  (collection-path "icons") 
					  "plt.gif")))
  (let ([p (open-input-text-editor e)])
    (test 'hello 'read (read p))
    (test 'there 'read (read p))
    (test 'res 'read (read p))
    (test #t 'read (is-a? (read p) image-snip%)))
  
  
  (let ()
    (define t (new text%))
    (send t insert (make-string 5000 #\a))
    (define p (open-input-text-editor t #:lock-while-reading? #t))
    (define locked-first (send t is-locked?))
    (void (read p))  ;; read the (big) symbol
    (void (read p))  ;; read eof
    (define locked-last (send t is-locked?))
    (test #t 'lock-while-reading?1 (and locked-first (not locked-last))))
  
  (let ()
    (define t (new text%))
    (send t insert (make-string 5000 #\a))
    (send t insert (make-object image-snip%))
    (define p (open-input-text-editor t #:lock-while-reading? #t))
    (define locked-first (send t is-locked?))
    (void (read p))  ;; read the (big) symbol
    (void (read p))  ;; read the image
    (void (read p))  ;; read eof
    (define locked-last (send t is-locked?))
    (test #t 'lock-while-reading?2 
          (and locked-first
               (not locked-last)))))

(let ()
  (define x (new text%))
  (define s (make-object image-snip% "no-such-image.jpg" 
			 'unknown #f #f))
  (define t (make-object image-snip% "no-such-image.jpg" 
			 'unknown #f #f))
  (send x insert s 0 'same #t)
  (send x insert t 1 'same #t)
  (send x insert "1" 2 'same #t)
  
  (let ([i (open-input-text-editor x 0 'end (lambda (x) (eq? x s)))])
    (test #t 'peek-s (peek-byte-or-special i 0))
    (test #t 'read-s (read-byte-or-special i))
    (test #f 'peek-t (peek-byte-or-special i 0))
    (test 49 'read-1 (peek-byte-or-special i 1))))

(let ()
  (define t (new text%))
  (send t insert "aa\nbb\ncc\ndd\nee\nff\n")
  (send t insert (make-object image-snip%
                   (collection-file-path "recycle.png" "icons")))
  
  (define p (open-input-text-editor t))
  
  (define rev-at-start (send t get-revision-number))
  (define line1 (read-line p))
  
  (define sl (send t get-style-list))
  (define d (make-object style-delta% 'change-bold))
  (define s (send sl find-or-create-style (send sl basic-style) d))
  (send t change-style s 6 7)
  
  (define rev-after-cs (send t get-revision-number))
  (define line2 (read-line p))
  
  (test #t 'revision-changed (> rev-after-cs rev-at-start))
  (test "aa" 'revision-changed-line1 line1)
  (test "bb" 'revision-changed-line1 line2))

(let ()
  (define t (new text%))
  (send t insert "abcd\n")
  (send t insert (make-object image-snip%
                   (collection-file-path "recycle.png" "icons")))
  
  (define (count-snips)
    (let loop ([s (send t find-first-snip)])
      (cond
        [s (+ 1 (loop (send s next)))]
        [else 0])))
  
  (send t split-snip 1)
  (define before-snip-count (count-snips))
  (define rev-at-start (send t get-revision-number))
  
  (define p (open-input-text-editor t))
  
  (define char1 (read-char p))
  
  (define s (send (send t get-style-list) basic-style))
  (send t change-style s 0 4)
  (define after-snip-count (count-snips))
  (define rev-after-cs (send t get-revision-number))
  
  (define chars (string (read-char p) (read-char p) (read-char p)))
  
  (test 4     'snips-joined1 before-snip-count)
  (test 3     'snips-joined2 after-snip-count)
  (test #t     'snips-joined3 (> rev-after-cs rev-at-start))
  (test #\a    'snips-joined4 char1)
  (test "bcd" 'snips-joined5 chars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Snips and Streams                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mk-number-snip-class% term?)
  (class snip-class%
    (define/override (read f)
      (let* ([number-str (if term?
			     (send f get-bytes)
			     (send f get-unterminated-bytes))]
             [number (string->number (bytes->string/utf-8 number-str))]
             [decimal-prefix (bytes->string/utf-8 (if term?
						      (send f get-bytes)
						      (send f get-unterminated-bytes)))]
             [snip
              (instantiate number-snip% ()
                [number number]
                [decimal-prefix decimal-prefix])])
        snip))
    (super-instantiate ())))


(define snip-class (make-object (mk-number-snip-class% #t)))
(send snip-class set-classname (format "~s" `(lib "number-snip.ss" "drscheme" "private")))
(send (get-the-snip-class-list) add snip-class)

(define snip-class2 (make-object (mk-number-snip-class% #f)))
(send snip-class2 set-classname (format "~s" `(lib "number-snip-two.ss" "drscheme" "private")))
(send (get-the-snip-class-list) add snip-class2)

(define (mk-number-snip% snip-class term?)
  (define self%
    (class snip%
      (init-field number)
      (define/public (get-number) number)
      (define/public (get-prefix) decimal-prefix)
      (init-field [decimal-prefix ""])
      (define/override (write f)
	(let ([num (string->bytes/utf-8 (number->string number))]
	      [pfx (string->bytes/utf-8 decimal-prefix)])
	  (if term?
	      (begin
		(send f put num)
		(send f put pfx))
	      (begin
		(unless (eq? 'ok
			     (with-handlers ([exn:fail? (lambda (x) 'ok)])
			       (send f put 5 #"123")
			       'not-ok))
		  (error "too-long write should have failed"))
		(send f put (bytes-length num) num)
		(send f put (bytes-length pfx) pfx)))))
      (define/override (copy)
	(instantiate self% ()
		     [number number]
		     [decimal-prefix decimal-prefix]))
      (inherit get-style)
      (super-instantiate ())
      (inherit set-snipclass set-flags get-flags)
      (set-snipclass snip-class)))
  self%)

(define number-snip% (mk-number-snip% snip-class #t))
(define number-snip2% (mk-number-snip% snip-class2 #f))

(define (snip-test term?)
  (define t (new text%))
  (define t2 (new text%))
  (send t insert (new (if term? number-snip% number-snip2%)
		      [number 1/2]))
  (send t set-position 0 1)
  (send t copy)
  ;; Under X, force snip to be marshalled:
  (let ([s (send the-clipboard get-clipboard-data "WXME" 0)])
    (send the-clipboard set-clipboard-client
	  (make-object (class clipboard-client%
			 (define/override (get-data fmt)
			   (and (string=? fmt "WXME")
				s))
			 (inherit add-type)
			 (super-new)
			 (add-type "WXME")))
	  0))
  (send t2 paste)
  (let ([s (send t2 find-first-snip)])
    (st 1/2 s get-number)
    (st "" s get-prefix)))

(snip-test #t)
(snip-test #f)

(let ()
  (define orig-snip (make-object string-snip% "hello"))
  
  (define out (make-object editor-stream-out-bytes-base%))
  (define out-stream (make-object editor-stream-out% out))
  
  (define _ (send orig-snip write out-stream))
  
  (define in (make-object editor-stream-in-bytes-base% (send out get-bytes)))
  (define in-stream (make-object editor-stream-in% in))
  
  (define new-snip
    (send (send (get-the-snip-class-list) 
		find 
		(send (send (new string-snip%) get-snipclass) get-classname))
	  read in-stream))
  
  (st "hello" new-snip get-text 0 10))

;; ----------------------------------------
;; Check CRLF conversion

(let ([crlf-s (apply
	       string-append
	       (let loop ([n 100])
		 (if (zero? n)
		     null
		     (cons (make-string (random 40) #\a)
			   (cons "\r\n"
				 (loop (sub1 n)))))))]
      [e (new text%)])
  (let ([lf-s (regexp-replace* #rx"\r\n" crlf-s "\n")]
	[lflf-s (regexp-replace* #rx"\r\n" crlf-s "\n\n")])
    (st "" e get-text 0 'eof)
    (send e insert-port (open-input-string crlf-s))
    (st lf-s e get-text 0 'eof)
    (send e erase)
    (send e insert crlf-s)
    (st lflf-s e get-text 0 'eof)))

;; ----------------------------------------
;; Check lines and paras without display, but with a max width

(define t (new text%))
(send t insert "abc\ndef\nghi\n")
(send t set-max-width 955)
(st 0 t line-start-position 0)
(st 4 t line-start-position 1)
(st 8 t line-start-position 2)
(st 0 t paragraph-start-position 0)
(st 4 t paragraph-start-position 1)
(st 8 t paragraph-start-position 2)
(st 3 t paragraph-end-position 0)
(st 7 t paragraph-end-position 1)
(st 11 t paragraph-end-position 2)
(st 0 t paragraph-start-line 0)
(st 1 t paragraph-start-line 1)
(st 2 t paragraph-start-line 2)
(st 0 t line-paragraph 0)
(st 1 t line-paragraph 1)
(st 2 t line-paragraph 2)
(st 0 t position-paragraph 0)
(st 0 t position-paragraph 3)
(st 1 t position-paragraph 4)
(st 1 t position-paragraph 7)
(st 2 t position-paragraph 8)
(st 2 t position-paragraph 11)

;; ----------------------------------------

;; More undo tests, checking mainly that internal undo array grows
;;  correctly

(define (test-undos local-undo?)
  
  (define e (new text%))

  (send e insert (make-string 1024 #\x))

  (send e set-max-undo-history 10240)
  (send e set-position 0)

  (let loop ([n 1024])
    (unless (zero? n)
      (send e set-position (add1 (send e get-start-position)))
      (send e delete)
      (send e insert #\y)
      (when local-undo?
	(send e undo)
	(send e redo))
      (loop (sub1 n))))

  (st (make-string 1024 #\y) e get-text 0 'eof)
  
  (let loop ([n 1023])
    (unless (zero? n)
      (send e undo)
      (send e undo)
      (loop (sub1 n))))

  (st (string-append "y" (make-string 1023 #\x) )
      e get-text 0 'eof))

(test-undos #f)
(test-undos #t)

;; ----------------------------------------

(let ([pb (new pasteboard%)]
      [es (new editor-snip%)])
  (send pb insert es)
  (st es pb find-first-snip)
  (send pb remove es)
  (st #f es is-owned?)
  (st #f pb find-first-snip)
  (send pb insert es)
  (st es pb find-first-snip)
  (st #t es is-owned?))

;; ----------------------------------------
;; edit-sequences and undo

(let ([t (new text%)])
  (send t set-max-undo-history 100)
  (send t begin-edit-sequence)
  (send t begin-edit-sequence)
  (send t insert "abcd\n")
  (send t set-modified #f)
  (send t end-edit-sequence)
  (send t delete 0 1)
  (send t end-edit-sequence)
  (send t undo)
  (st "" t get-text))
  
(let ([t (new text%)])
  (send t set-max-undo-history 100)
  (send t begin-edit-sequence)
  (send t begin-edit-sequence)
  (send t insert "abcd\n")
  (send t end-edit-sequence)
  (send t set-position 0 1)
  (send t delete)
  (send t set-position 0 1)
  (send t delete)
  (send t end-edit-sequence)
  (send t undo)
  (st "" t get-text))

;; ----------------------------------------
;; undo and clickbacks

(let ([t (new text%)])
  (send t set-max-undo-history 100)
  (send t insert "abcdef")
  (send t set-clickback 1 3 void)
  (send t delete 0 5)
  (send t undo))

;; ----------------------------------------
;; notification callbacks, weak links, and chaperones:

(let ()
  (define id 0)
  (define count 0)
  (define count2 0)

  (define sl (new style-list%))

  (define N 100)

  (define cbs
    (for/list ([i (in-range N)])
      (define cb (lambda (x) (set! id i) (set! count (add1 count))))
      ;; procedure retained:
      (void (send sl notify-on-change (chaperone-procedure cb (lambda (v) v))))
      ;; procedure not retained:
      (void (send sl notify-on-change (lambda (x) (set! id i) (set! count2 (add1 count2)))))
      cb))

  (define (try name)
    (send sl new-named-style name (send sl find-named-style "Basic")))

  (try "X")
  (set! count 0)
  (set! count2 0)

  (collect-garbage)
  (try "Y") ;; expect 2 callbacks per notifier

  (define v #f)
  (set! v cbs) ;; forces retention of `cbs'
  (unless (= (length v) N) (error "test is broken"))

  (unless (= count (* 2 N))
    (error 'notifications "too weak? ~e" count))
  (unless (<= 0 count2 (/ N 2))
    (error 'notifications "not weak enough? ~e" count2)))
  
;; ----------------------------------------

(report-errs)

