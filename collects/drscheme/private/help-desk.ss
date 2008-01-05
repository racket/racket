#lang scheme/unit

(require (lib "string-constant.ss" "string-constants")
         (lib "mred.ss" "mred")
         (lib "external.ss" "browser")
         (lib "bug-report.ss" "help")
         (lib "buginfo.ss" "help" "private")
         (lib "framework.ss" "framework")
         (lib "class.ss")
         (lib "list.ss")
         (lib "search.ss" "help")
         "drsig.ss")

(import [prefix drscheme:frame: drscheme:frame^]
        [prefix drscheme:language-configuration: drscheme:language-configuration/internal^])
(export drscheme:help-desk^)

(define (-add-help-desk-font-prefs b) '(add-help-desk-font-prefs b))

;; : -> string
(define (get-computer-language-info)
  (let* ([language/settings (preferences:get 
                             drscheme:language-configuration:settings-preferences-symbol)]
         [language (drscheme:language-configuration:language-settings-language
                    language/settings)]
         [settings (drscheme:language-configuration:language-settings-settings
                    language/settings)])
    (format
     "~s"
     (list
      (send language get-language-position)
      (send language marshall-settings settings)))))

(set-bug-report-info! "Computer Language" get-computer-language-info)

(define lang-message%
  (class canvas%
    (init-field button-release font)
    (define/override (on-event evt)
      (when (send evt button-up?)
        (button-release)))
    (field [msg ""])
    (define/public (set-msg l) (set! msg l) (on-paint))
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (let ([dc (get-dc)]
            [dots "..."])
        (let-values ([(tw th _1 _2) (send dc get-text-extent msg)]
                     [(dw dh _3 _4) (send dc get-text-extent dots)]
                     [(cw ch) (get-client-size)])
          (send dc set-brush (send the-brush-list find-or-create-brush (get-panel-background) 'panel))
          (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
          (send dc set-font font)
          (send dc draw-rectangle 0 0 cw ch)
          (cond
            [(tw . <= . cw)
             (send dc draw-text msg 0 (- (/ ch 2) (/ th 2)))]
            [(cw . <= . dw)  ;; just give up if there's not enough room to draw the dots
             (void)]
            [else
             (send dc set-clipping-rect 0 0 (- cw dw 2) ch)
             (send dc draw-text msg 0 (- (/ ch 2) (/ th 2)))
             (send dc set-clipping-region #f)
             (send dc draw-text dots (- cw dw) (- (/ ch 2) (/ th 2)))]))))
    (super-new)))

(define (goto-manual-link a b) (error 'goto-maual-link "~s ~s" a b))
(define (goto-hd-location b) (error 'goto-hd-location "~s" b))

(define (goto-help manual link) (goto-manual-link manual link))
(define (goto-tour) (goto-hd-location 'hd-tour))
(define (goto-release-notes) (goto-hd-location 'release-notes))
(define (goto-plt-license) (goto-hd-location 'plt-license))

(define (get-docs) 
  ;(error 'help-desk.ss "get-docs")
  '())

(define help-desk
  (case-lambda
    [() (send-main-page)]
    [(key) (generate-search-results (list key))]))
