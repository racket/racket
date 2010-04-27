#lang scheme/base
(require framework
         unstable/gui/prefs)
(provide (all-defined-out))

;; Frame size preferences

(preferences:set-default 'schemeunit:frame:width 400 exact-positive-integer?)
(preferences:set-default 'schemeunit:frame:height 400 exact-positive-integer?)
(define pref:width (pref:get/set 'schemeunit:frame:width))
(define pref:height (pref:get/set 'schemeunit:frame:height))

;; CONSTANTS
;; Some of these are obsolete, given the preferences above.

(define DETAILS-CANVAS-INIT-WIDTH 400)
(define FRAME-LABEL "SchemeUnit")
(define FRAME-INIT-HEIGHT 400)
(define TREE-INIT-WIDTH 240)
(define TREE-COLORIZE-CASES #t)
(define DIALOG-ERROR-TITLE "SchemeUnit: Error")
(define STATUS-SUCCESS 'success)
(define STATUS-FAILURE 'failure)
(define STATUS-ERROR 'error)
(define STATUS-UNEXECUTED 'unexecuted)
(define VIEW-PANE-PERCENTS 
  (let [(total (+ DETAILS-CANVAS-INIT-WIDTH TREE-INIT-WIDTH))]
    (list (/ TREE-INIT-WIDTH total) (/ DETAILS-CANVAS-INIT-WIDTH total))))

;; Conventional assertion-info keys.
;; These must be kept in sync with assert-base.ss.
(define prop:failure-assertion 'name)
(define prop:failure-parameters 'params)
(define prop:failure-location 'location)
(define prop:failure-message 'message)
(define prop:test-case-location 'test-case-location)

;; / CONSTANTS

(define (known-property? s)
  (case s
    ((name params location message test-case-location) #t)
    ((actual expected) #t)
    ((expression) #t)
    (else #f)))
