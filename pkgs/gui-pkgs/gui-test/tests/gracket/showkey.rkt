#lang racket/base
(require racket/gui/base
         racket/class
         racket/cmdline)

(command-line
 #:once-each
 [("--option") "set special Option key"
  (special-option-key #t)]
 [("--control") "set special Control key"
  (special-control-key #t)])

(let ()
  (define iter 0)
  (define c%
    (class canvas%
      (super-new)
      (define/override (on-event ev)
        (printf "~a~a MOUSE ~a (~a,~a)\n  mods:~a~a~a~a~a~a~a~a\n  buttons:~a~a~a~a~a~a~a\n" 
                (es-check)
                iter
                (send ev get-event-type)
                (send ev get-x)
                (send ev get-y)
                (if (send ev get-meta-down) " META" "")
                (if (send ev get-control-down) " CTL" "")
                (if (send ev get-alt-down) " ALT" "")
                (if (send ev get-shift-down) " SHIFT" "")
                (if (send ev get-caps-down) " CAPS" "")
                (if (send ev get-mod3-down) " MOD3" "")
                (if (send ev get-mod4-down) " MOD4" "")
                (if (send ev get-mod5-down) " MOD5" "")
                (if (send ev get-left-down) " LEFT" "")
                (if (send ev get-middle-down) " MIDDLE" "")
                (if (send ev get-right-down) " RIGHT" "")
                (if (send ev dragging?)
                    " dragging"
                    "")
                (if (send ev moving?)
                    " moving"
                    "")
                (if (send ev entering?)
                    " entering"
                    "")
                (if (send ev leaving?)
                    " leaving"
                    "")))
      (define/override (on-char ev)
        (set! iter (add1 iter))
        (printf "~a~a KEY: ~a\n  rel-code: ~a\n  other-codes: ~a\n  mods:~a~a~a~a~a~a~a~a~a\n"
                (es-check)
                iter
                (let ([v (send ev get-key-code)])
                  (if (symbol? v)
                      v
                      (format "~s = ASCII ~a" (string v) (char->integer v))))
                (let ([v (send ev get-key-release-code)])
                  (if (symbol? v)
                      v
                      (format "~s = ASCII ~a" (string v) (char->integer v))))
                (let ([vs (list (send ev get-other-shift-key-code)
                                (send ev get-other-altgr-key-code)
                                (send ev get-other-shift-altgr-key-code)
                                (send ev get-other-caps-key-code))])
                  (map (lambda (v)
                         (and v
                              (if (symbol? v)
                                  v
                                  (format "~s = ASCII ~a" (string v) (char->integer v)))))
                       vs))
                (if (send ev get-meta-down) " META" "")
                (if (send ev get-control-down) " CTL" "")
                (if (send ev get-control+meta-is-altgr) " = ALTGR" "")
                (if (send ev get-alt-down) " ALT" "")
                (if (send ev get-shift-down) " SHIFT" "")
                (if (send ev get-caps-down) " CAPS" "")
                (if (send ev get-mod3-down) " MOD3" "")
                (if (send ev get-mod4-down) " MOD4" "")
                (if (send ev get-mod5-down) " MOD5" "")))))
  (define f (make-object (class frame%
                           (inherit accept-drop-files)
                           (define/override (on-drop-file file)
                             (printf "Dropped: ~a\n" file))
                           (super-make-object  "tests" #f 100 100)
                           (accept-drop-files #t))))
  (define c (make-object c% f))
  (define (es-check) (if (eq? (send f get-eventspace) (current-eventspace))
			 ""
			 ">>WRONG EVENTSPACE<<\n"))
  (send c focus)
  (send f show #t))
