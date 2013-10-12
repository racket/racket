#lang racket/base

(require "test-suite-utils.rkt")
  
  (test
   'keymap:aug-keymap%/get-table
   (lambda (x)
     (equal? '((c:k "abc")) x))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)])
         (send k add-function "abc" void)
         (send k map-function "c:k" "abc")
         (hash-map (send k get-map-function-table) list)))))
  
  (test
   'keymap:aug-keymap%/get-table/ht
   (lambda (x)
     (equal? x '((c:k "def"))))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)]
             [ht (make-hasheq)])
         (send k add-function "abc" void)
         (send k map-function "c:k" "abc")
         (hash-set! ht 'c:k "def")
         (hash-map (send k get-map-function-table/ht ht) list)))))
  
  (test
   'keymap:aug-keymap%/get-table/chain1
   (lambda (x)
     (equal? x '((c:k "abc-k2"))))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)]
             [k1 (make-object keymap:aug-keymap%)]
             [k2 (make-object keymap:aug-keymap%)])
         (send k1 add-function "abc-k1" void)
         (send k1 map-function "c:k" "abc-k1")
         (send k2 add-function "abc-k2" void)
         (send k2 map-function "c:k" "abc-k2")
         (send k chain-to-keymap k1 #t)
         (send k chain-to-keymap k2 #t)
         (hash-map (send k get-map-function-table) list)))))
  
  (test
   'keymap:aug-keymap%/get-table/chain/2
   (lambda (x)
     (equal? x '((c:k "abc-k"))))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)]
             [k1 (make-object keymap:aug-keymap%)])
         (send k1 add-function "abc-k1" void)
         (send k1 map-function "c:k" "abc-k1")
         (send k add-function "abc-k" void)
         (send k map-function "c:k" "abc-k")
         (send k chain-to-keymap k1 #t)
         (hash-map (send k get-map-function-table) list)))))
  
  (test
   'keymap:aug-keymap%/get-table/normalize-case
   (lambda (x)
     (equal? x '((|esc;p| "abc-k2"))))
   (lambda ()
     (queue-sexp-to-mred
      '(let ([k (make-object keymap:aug-keymap%)]
             [k1 (make-object keymap:aug-keymap%)])
         (send k1 add-function "abc-k1" void)
         (send k1 map-function "esc;p" "abc-k1")
         (send k add-function "abc-k2" void)
         (send k map-function "ESC;p" "abc-k2")
         (send k chain-to-keymap k1 #t)
         (hash-map (send k get-map-function-table) list)))))
  
  (define (test-canonicalize name str1 str2)
    (test
     (string->symbol (format "keymap:canonicalize-keybinding-string/~a" name))
     (lambda (x)
       (string=? x str2))
     (lambda ()
       (queue-sexp-to-mred
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
  (test-canonicalize 13 "ESC;p" "esc;p")
  
  
  ;; a key-spec is (make-key-spec buff-spec buff-spec (listof ?) (listof ?) (listof ?))
  ;; a key-spec represents a test case for a key; 'before' contains the
  ;; content of a buffer, and 'after' represents the desired content of the
  ;; buffer after the keypress.  The keypress(es) in question are specified
  ;; independently for the three platforms by the respective 'macos', 'unix',
  ;; and 'windows' fields.
  (define-struct key-spec (before after macos unix windows) #:prefab)
  
  ;; an abstraction to use when all platforms have the same sequence of keys
  (define (make-key-spec/allplatforms before after keys)
    (make-key-spec before after keys keys keys))
  
  ;; a buff-spec is (make-buff-spec string nat nat)
  ;; a buff-spec represents a buffer state; the content of the buffer,
  ;; and the start and end of the highlighted region.
  ;; the overwrite? field specifies if the overwrite mode is enabled during the test
  ;;    (its value is ignored for the result checking)
  (define-struct buff-spec (string start end overwrite?) #:prefab)
  
  (define (build-buff-spec string start end #:overwrite? [overwrite? #f])
    (make-buff-spec string start end overwrite?))

  ;; the keybindings test cases applied to frame:text% editors
  (define global-specs
    (list
     (make-key-spec (build-buff-spec "abc" 1 1)
                    (build-buff-spec "abc" 2 2)
                    (list '((#\f control)) '((right)))
                    (list '((#\f control)) '((right)))
                    (list '((#\f control)) '((right))))
     
     (make-key-spec/allplatforms (build-buff-spec "\n\n\n\n" 2 2)
                                 (build-buff-spec "\n" 0 0)
                                 '(((#\x control) (#\o control))))
     (make-key-spec/allplatforms (build-buff-spec "  \n  \n  \n  \n" 7 7)
                                 (build-buff-spec "  \n" 1 1)
                                 '(((#\x control) (#\o control))))
     (make-key-spec/allplatforms (build-buff-spec "\n\n\n\n" 0 0)
                                 (build-buff-spec "\n" 0 0)
                                 '(((#\x control) (#\o control))))
     (make-key-spec/allplatforms (build-buff-spec "abcdef\n\n\n\nxyzpdq\n" 8 8)
                                 (build-buff-spec "abcdef\n\nxyzpdq\n" 7 7)
                                 '(((#\x control) (#\o control))))
     
     ;; TeX-compress tests
     (make-key-spec/allplatforms 
      (build-buff-spec "\\ome" 4 4)
      (build-buff-spec "ω" 1 1)
      '(((#\\ control))))
     (make-key-spec/allplatforms 
      (build-buff-spec "\\sub" 4 4)
      (build-buff-spec "\\subset" 7 7)
      '(((#\\ control))))
     (make-key-spec/allplatforms 
      (build-buff-spec "\\subset" 7 7)
      (build-buff-spec "⊂" 1 1)
      '(((#\\ control))))
     (make-key-spec/allplatforms 
      (build-buff-spec "\\sub" 4 4)
      (build-buff-spec "⊆" 1 1)
      '(((#\\ control) (#\e) (#\\ control))))))
  
  (define (build-open-bracket-spec str pos char)
    (make-key-spec (build-buff-spec str pos pos)
                   (build-buff-spec 
                    (string-append (substring str 0 pos)
                                   (string char)
                                   (substring str pos (string-length str)))
                    (+ pos 1)
                    (+ pos 1))
                   (list (list (list #\[)))
                   (list (list (list #\[)))
                   (list (list (list #\[)))))
  
  (define (ascii-art-box-spec before after)
    (make-key-spec/allplatforms (build-buff-spec before 0 0) 
                                (build-buff-spec after 0 0)
                                (list '((#\x control) (#\r) (#\a)))))
  
  ;; the keybindings test cases applied to racket:text% editors
  (define scheme-specs
    (list 
     (make-key-spec (build-buff-spec "(abc (def))" 4 4)
                    (build-buff-spec "(abc (def))" 10 10)
                    (list '((right alt)))
                    (list '((right alt)))
                    (list '((right alt))))
     (make-key-spec (build-buff-spec "'(abc (def))" 1 1)
                    (build-buff-spec "'(abc (def))" 12 12)
                    (list '((right alt)))
                    (list '((right alt)))
                    (list '((right alt))))
     #|
     (make-key-spec (build-buff-spec "'(abc (def))" 0 0)
                    (build-buff-spec "'(abc (def))" 12 12)
                    (list '(right alt))
                    (list '(right alt))
                    (list '(right alt)))
     (make-key-spec (build-buff-spec "'(abc (def))" 12 12)
                    (build-buff-spec "'(abc (def))" 0 0)
                    (list '(left alt))
                    (list '(left alt))
                    (list '(left alt)))
|#
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
     (build-open-bracket-spec "(let ([])(" 10 #\()
     (build-open-bracket-spec "(local " 7 #\[)
     (build-open-bracket-spec "(local []" 9 #\()
     ;; test to show that multi-keystrokes works:
     (make-key-spec/allplatforms
      (build-buff-spec "" 0 0) 
      (build-buff-spec "zx" 2 2)
      (list '((#\z) (#\x))))
     ;; remove-enclosing-parens :
     (make-key-spec/allplatforms
      (build-buff-spec "(abc def)" 1 1)
      (build-buff-spec "abc" 0 0)
      (list '((#\c control) (#\o control))))
     ;; (is this the desired behavior?):
     (make-key-spec/allplatforms
      (build-buff-spec "(abc def)" 2 3)
      (build-buff-spec "bc" 0 0)
      (list '((#\c control) (#\o control))))
     ;; insert-()-pair :
     (make-key-spec
      (build-buff-spec "abc" 0 0)
      (build-buff-spec "()abc" 1 1)
      (list '((escape) (#\()))
      (list '((#\( meta)))
      (list '((escape) (#\())))
     (make-key-spec
      (build-buff-spec "abc" 0 2)
      (build-buff-spec "(ab)c" 1 1)
      (list '((escape) (#\()))
      (list '((#\( meta)))
      (list '((escape) (#\())))
     ;; toggle-square-round-parens :
     ; () -> []
     (make-key-spec/allplatforms
      (build-buff-spec "(a)" 0 0)
      (build-buff-spec "[a]" 0 0)
      (list '((#\c control) (#\[ control))))
     ; [] -> ()
     (make-key-spec/allplatforms
      (build-buff-spec "[a]" 0 0)
      (build-buff-spec "(a)" 0 0)
      (list '((#\c control) (#\[ control))))
     ; enclosed sexps
     (make-key-spec/allplatforms
      (build-buff-spec "[a (def )b]" 0 0)
      (build-buff-spec "(a (def )b)" 0 0)
      (list '((#\c control) (#\[ control))))
     ; extra preceding whitespace
     (make-key-spec/allplatforms
      (build-buff-spec "  \n [a (def )b]" 0 0)
      (build-buff-spec "  \n (a (def )b)" 0 0)
      (list '((#\c control) (#\[ control))))
     ; cursor not at beginning of buffer
     (make-key-spec/allplatforms
      (build-buff-spec "  \n [a (def )b]" 1 1)
      (build-buff-spec "  \n (a (def )b)" 1 1)
      (list '((#\c control) (#\[ control))))
     ; intervening non-paren sexp
     (make-key-spec/allplatforms
      (build-buff-spec "  \nf [a (def )b]" 1 1)
      (build-buff-spec "  \nf [a (def )b]" 1 1)
      (list '((#\c control) (#\[ control))))
     ;; at end of buffer (hence sexp-forward returns #f):
     (make-key-spec/allplatforms
      (build-buff-spec "[a]" 3 3)
      (build-buff-spec "[a]" 3 3)
      (list '((#\c control) (#\[ control))))
     
     (make-key-spec/allplatforms
      (build-buff-spec "a" 0 0 #:overwrite? #t)
      (build-buff-spec "b" 1 1)
      (list '((#\b))))
     
     (make-key-spec/allplatforms
      (build-buff-spec "a" 0 0 #:overwrite? #t)
      (build-buff-spec "|" 1 1)
      (list '((#\|))))
     
     (make-key-spec/allplatforms
      (build-buff-spec "a" 0 0 #:overwrite? #t)
      (build-buff-spec "(" 1 1)
      (list '((#\())))
     
     (make-key-spec/allplatforms
      (build-buff-spec "a" 0 0 #:overwrite? #t)
      (build-buff-spec ")" 1 1)
      (list '((#\)))))
     
     ;; needs to be in auto-adjut open paren mode
     (make-key-spec/allplatforms
      (build-buff-spec "a" 0 0 #:overwrite? #t)
      (build-buff-spec "(" 1 1)
      (list '((#\[))))
     
     (ascii-art-box-spec "+" "═")
     (ascii-art-box-spec "x" "x")
     (ascii-art-box-spec "+-+" "═══")
     (ascii-art-box-spec "+\n|\n+\n" "║\n║\n║\n")
     (ascii-art-box-spec (string-append "+-+\n"
                                        "| |\n"
                                        "+-+\n")
                         (string-append "╔═╗\n"
                                        "║ ║\n"
                                        "╚═╝\n"))
     (ascii-art-box-spec (string-append "+---+\n"
                                        "| - |\n"
                                        "|+ ||\n"
                                        "+---+\n")
                         (string-append "╔═══╗\n"
                                        "║ - ║\n"
                                        "║+ |║\n"
                                        "╚═══╝\n"))
     (ascii-art-box-spec (string-append "+-+-+\n"
                                        "| | |\n"
                                        "+-+-+\n"
                                        "| | |\n"
                                        "+-+-+\n")
                         (string-append "╔═╦═╗\n"
                                        "║ ║ ║\n"
                                        "╠═╬═╣\n"
                                        "║ ║ ║\n"
                                        "╚═╩═╝\n"))))
  
  (define automatic-scheme-specs
    (list (make-key-spec/allplatforms (build-buff-spec "" 0 0) 
                                      (build-buff-spec "()" 1 1)
                                      '(((#\())))
          (make-key-spec/allplatforms (build-buff-spec "" 0 0) 
                                      (build-buff-spec "[]" 1 1)
                                      '(((#\[))))
          (make-key-spec/allplatforms (build-buff-spec "" 0 0) 
                                      (build-buff-spec "{}" 1 1)
                                      '(((#\{))))
          (make-key-spec/allplatforms (build-buff-spec "" 0 0) 
                                      (build-buff-spec "\"\"" 1 1)
                                      '(((#\"))))
          (make-key-spec/allplatforms (build-buff-spec "" 0 0) 
                                      (build-buff-spec "||" 1 1)
                                      '(((#\|))))))
  
  (queue-sexp-to-mred `(send (make-object frame:basic% "dummy to trick frame group") show #t))
  (wait-for-frame "dummy to trick frame group")
  
  ;; test-key : key-spec -> 
  ;;   evaluates a test case represented as a key-spec
  (define (test-key key-spec i)
    (let* ([key-sequences 
            ((case (system-type)
               [(macos macosx) key-spec-macos]
               [(unix) key-spec-unix]
               [(windows) key-spec-windows])
             key-spec)]
           [before (key-spec-before key-spec)]
           [after (key-spec-after key-spec)]
           [process-key-sequence
            (lambda (key-sequence)
              (let ([text-expect (buff-spec-string after)]
                    [start-expect (buff-spec-start after)]
                    [end-expect (buff-spec-end after)])
                (test (list key-sequence i)
                      (lambda (x) (equal? x (vector text-expect start-expect end-expect)))
                      `(let* ([qc (λ (t) (let ([c (make-channel)])
                                           (queue-callback (λ () (channel-put c (t))))
                                           (channel-get c)))]
                              [text (qc (λ () (send (get-top-level-focus-window) get-editor)))])
                         (qc (λ () 
                               (send text set-overwrite-mode ,(buff-spec-overwrite? before))
                               (send text erase)
                               (send text insert ,(buff-spec-string before))
                               (send text set-position ,(buff-spec-start before) ,(buff-spec-end before))))
                         ,@(map (lambda (key) `(test:keystroke ',(car key) ',(cdr key)))
                                key-sequence)
                         (qc (λ ()
                               (vector (send text get-text)
                                       (send text get-start-position)
                                       (send text get-end-position))))))))])
      (for-each process-key-sequence key-sequences)))
  
  
  (define (test-specs frame-name frame-class specs)
    (queue-sexp-to-mred `(send (make-object ,frame-class ,frame-name) show #t))
    (wait-for-frame frame-name)
    (for ([spec (in-list specs)]
          [i (in-naturals)])
      (test-key spec i))
    (queue-sexp-to-mred `(send (get-top-level-focus-window) close)))

(define old-paren-adjusting-prefs
  (queue-sexp-to-mred `(list (preferences:get 'framework:fixup-open-parens)
                             (preferences:get 'framework:automatic-parens))))

  
  (queue-sexp-to-mred `(preferences:set 'framework:fixup-open-parens #t))
  (queue-sexp-to-mred `(preferences:set 'framework:automatic-parens #f))
  (test-specs "global keybindings test" 'frame:text% global-specs)
  (test-specs "scheme mode keybindings test" 
              '(class frame:editor%
                 (define/override (get-editor%) racket:text%)
                 (super-new))
              scheme-specs)
  (queue-sexp-to-mred `(preferences:set 'framework:automatic-parens #t))
  (queue-sexp-to-mred `(preferences:set 'framework:fixup-open-parens #f))
  (test-specs "scheme mode automatic-parens on keybindings test" 
              '(class frame:editor%
                 (define/override (get-editor%) racket:text%)
                 (super-new))
              automatic-scheme-specs)

(queue-sexp-to-mred
 `(begin (preferences:set 'framework:fixup-open-parens ,(list-ref old-paren-adjusting-prefs 0))
         (preferences:set 'framework:automatic-parens ,(list-ref old-paren-adjusting-prefs 1))))
