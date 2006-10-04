(module keys mzscheme
  (require "test-suite-utils.ss")
  (require (lib "include.ss"))
  
  (test
   'keymap:aug-keymap%/get-table
   (lambda (x)
     (equal? '((c:k "abc")) x))
   (lambda ()
     (send-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)])
         (send k add-function "abc" void)
         (send k map-function "c:k" "abc")
         (hash-table-map (send k get-map-function-table) list)))))
  
  (test
   'keymap:aug-keymap%/get-table/ht
   (lambda (x)
     (equal? x '((c:k "def"))))
   (lambda ()
     (send-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)]
             [ht (make-hash-table)])
         (send k add-function "abc" void)
         (send k map-function "c:k" "abc")
         (hash-table-put! ht 'c:k "def")
         (hash-table-map (send k get-map-function-table/ht ht) list)))))
  
  (test
   'keymap:aug-keymap%/get-table/chain1
   (lambda (x)
     (equal? x '((c:k "abc-k2"))))
   (lambda ()
     (send-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)]
             [k1 (make-object keymap:aug-keymap%)]
             [k2 (make-object keymap:aug-keymap%)])
         (send k1 add-function "abc-k1" void)
         (send k1 map-function "c:k" "abc-k1")
         (send k2 add-function "abc-k2" void)
         (send k2 map-function "c:k" "abc-k2")
         (send k chain-to-keymap k1 #t)
         (send k chain-to-keymap k2 #t)
         (hash-table-map (send k get-map-function-table) list)))))
  
  (test
   'keymap:aug-keymap%/get-table/chain/2
   (lambda (x)
     (equal? x '((c:k "abc-k"))))
   (lambda ()
     (send-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)]
             [k1 (make-object keymap:aug-keymap%)])
         (send k1 add-function "abc-k1" void)
         (send k1 map-function "c:k" "abc-k1")
         (send k add-function "abc-k" void)
         (send k map-function "c:k" "abc-k")
         (send k chain-to-keymap k1 #t)
         (hash-table-map (send k get-map-function-table) list)))))
  
  (define (test-canonicalize name str1 str2)
    (test
     (string->symbol (format "keymap:canonicalize-keybinding-string/~a" name))
     (lambda (x)
       (string=? x str2))
     (lambda ()
       (send-sexp-to-mred
        `(keymap:canonicalize-keybinding-string ,str2)))))
  
  (test-canonicalize 1 "c:a" "c:a")
  (test-canonicalize 2 "d:a" "d:a")
  (test-canonicalize 3 "m:a" "m:a")
  (test-canonicalize 4 "a:a" "a:a")
  (test-canonicalize 5 "s:a" "s:a")
  (test-canonicalize 6 "c:a" "c:a")
  (test-canonicalize 7 "s:m:d:c:a:a" "a:c:d:m:s:a")
  (test-canonicalize 8 "~s:~m:~d:~c:~a:a" "~a:~c:~d:~m:~s:a")
  (test-canonicalize 9 ":a" "~a:~c:~d:~m:~s:a")
  (test-canonicalize 10 ":d:a" "~a:~c:d:~m:~s:a")
  (test-canonicalize 11 "esc;s:a" "esc;s:a")
  (test-canonicalize 12 "s:a;esc" "s:a;esc")
  
  (define-struct key-spec (before after macos unix windows))
  (define-struct buff-spec (string start end))
  
  (define global-specs
    (list
     (make-key-spec (make-buff-spec "abc" 1 1)
                    (make-buff-spec "abc" 2 2)
                    (list '(#\f control) '(right))
                    (list '(#\f control) '(right))
                    (list '(#\f control) '(right)))))
  
  (define (build-open-bracket-spec str pos char)
    (make-key-spec (make-buff-spec str pos pos)
                   (make-buff-spec 
                    (string-append (substring str 0 pos)
                                   (string char)
                                   (substring str pos (string-length str)))
                    (+ pos 1)
                    (+ pos 1))
                   (list (list #\[))
                   (list (list #\[))
                   (list (list #\[))))
  
  (define scheme-specs
    (list 
     (make-key-spec (make-buff-spec "(abc (def))" 4 4)
                    (make-buff-spec "(abc (def))" 10 10)
                    (list '(right alt))
                    (list '(right alt))
                    (list '(right alt)))
     (make-key-spec (make-buff-spec "'(abc (def))" 1 1)
                    (make-buff-spec "'(abc (def))" 12 12)
                    (list '(right alt))
                    (list '(right alt))
                    (list '(right alt)))
     (build-open-bracket-spec "" 0 #\()
     (build-open-bracket-spec "(f cond " 8 #\()
     (build-open-bracket-spec "(f let (" 8 #\()
     (build-open-bracket-spec "(let (" 6 #\[)
     (build-open-bracket-spec "(let (" 5 #\()
     (build-open-bracket-spec "(provide/contract " 18 #\[)
     (build-open-bracket-spec "(kond " 5 #\()
     (build-open-bracket-spec "(cond " 5 #\[)
     (build-open-bracket-spec "(case-lambda " 13 #\[)
     (build-open-bracket-spec "(let ([]" 8 #\[)
     (build-open-bracket-spec "(let ({}" 8 #\{)
     (build-open-bracket-spec "()" 2 #\()
     (build-open-bracket-spec "(let (;;" 8 #\[)
     (build-open-bracket-spec ";" 1 #\[)
     (build-open-bracket-spec "\"" 1 #\[)
     (build-open-bracket-spec "\"\"" 1 #\[)
     (build-open-bracket-spec "||" 1 #\[)
     (build-open-bracket-spec "" 0 #\()
     (build-open-bracket-spec "(let (" 6 #\[)
     (build-open-bracket-spec "(new x% " 8 #\[)
     (build-open-bracket-spec "#\\" 2 #\[)
     (build-open-bracket-spec "#\\a" 2 #\[)
     (build-open-bracket-spec "(let ([let (" 12 #\()
     (build-open-bracket-spec "ab" 1 #\()
     (build-open-bracket-spec "|ab|" 2 #\[)
     (build-open-bracket-spec "(let loop " 10 #\()
     (build-open-bracket-spec "(let loop (" 11 #\[)
     (build-open-bracket-spec "(case x " 8 #\[)
     (build-open-bracket-spec "(case x [" 9 #\()
     (build-open-bracket-spec "(let ([])(" 10 #\()))
  
  
  (send-sexp-to-mred `(send (make-object frame:basic% "dummy to trick frame group") show #t))
  (wait-for-frame "dummy to trick frame group")
  
  (define (test-key key-spec)
    (let* ([keys ((case (system-type)
                    [(macos macosx) key-spec-macos]
                    [(unix) key-spec-unix]
                    [(windows) key-spec-windows])
                  key-spec)]
           [before (key-spec-before key-spec)]
           [after (key-spec-after key-spec)]
           [process-key
            (lambda (key)
              (let ([text-expect (buff-spec-string after)]
                    [start-expect (buff-spec-start after)]
                    [end-expect (buff-spec-end after)])
                (test key
                      (lambda (x) (equal? x (vector text-expect start-expect end-expect)))
                      `(let* ([text (send (get-top-level-focus-window) get-editor)])
                         (send text erase)
                         (send text insert ,(buff-spec-string before))
                         (send text set-position ,(buff-spec-start before) ,(buff-spec-end before))
                         (test:keystroke ',(car key) ',(cdr key))
                         (vector (send text get-text)
                                 (send text get-start-position)
                                 (send text get-end-position))))))])
      (for-each process-key keys)))
  
  (define (test-specs frame-name frame-class specs)
    (send-sexp-to-mred `(send (make-object ,frame-class ,frame-name) show #t))
    (wait-for-frame frame-name)
    (for-each test-key specs)
    (send-sexp-to-mred `(send (get-top-level-focus-window) close)))
  
  (test-specs "global keybingings test" 'frame:text% global-specs)
  (test-specs "scheme mode keybindings test" 
              '(class frame:editor%
                 (define/override (get-editor%) scheme:text%)
                 (super-new))
              scheme-specs))

