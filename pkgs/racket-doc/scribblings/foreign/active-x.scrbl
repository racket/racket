#lang scribble/doc
@(require scribble/manual
          "com-common.rkt"
          (for-label racket/base
                     ffi/unsafe/com
                     xml))

@title[#:tag "active-x"]{ActiveX Controls}

An ActiveX control is a COM object that needs a container to manage
its graphical representation. Although @racketmodname[ffi/com] does
not provide direct support for ActiveX controls, you can use
@racketmodname[ffi/com] to drive Internet Explorer as an ActiveX
container.

The following code demonstrates using Internet Explorer to instantiate
the ``Sysmon'' ActiveX control that is included with Windows.

@codeblock{
#lang racket
(require ffi/com
	 xml)

;; The control we want to run:
(define control-progid "Sysmon")

;; Start IE:
(define ie (com-create-instance "InternetExplorer.Application.1"))

;; Set up an event callback so that we know when the initial document
;; is ready:
(define ex (com-make-event-executor))
(void (thread (lambda () (let loop () ((sync ex)) (loop)))))
(define ready (make-semaphore))
(com-register-event-callback ie "DocumentComplete" 
                             (lambda (doc url) (semaphore-post ready)) 
                             ex)

;; Navigate to get an initial document:
(com-invoke ie "Navigate" "about:blank")
(define READYSTATE_COMPLETE 4)
(unless (= (com-get-property ie "READYSTATE") READYSTATE_COMPLETE)
  (semaphore-wait ready))
(define doc (com-get-property ie "Document"))

;; Install HTML to show the ActiveX control:
(com-invoke doc "write"
            (xexpr->string
             `(html
               (head (title "Demo"))
               (body
                (object ((class "object")
                         (CLASSID ,(format 
                                    "CLSID:~a"
                                    (let ([s (guid->string 
                                              (progid->clsid 
                                               control-progid))])
                                      ;; must remove curly braces:
                                      (define len 
                                        (string-length s))
                                      (substring s 1 (sub1 len)))))))))))

;; Configure the IE window and show it:
(com-set-property! ie "MenuBar" #f)
(com-set-property! ie "ToolBar" 0)
(com-set-property! ie "StatusBar" #f)
(com-set-property! ie "Visible" #t)

;; Extract the ActiveX control from the IE document:
(define ctl (com-get-property 
	     (com-invoke (com-invoke doc "getElementsByTagName" "object") 
                         "item" 
                         0)
	     "object"))

;; At this point, `ctl' is the ActiveX control;
;; demonstrate by getting a list of method names:
(com-methods ctl)
}
