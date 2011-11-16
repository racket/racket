#lang racket/base
(require ffi/unsafe
	 "types.rkt"
	 "utils.rkt")

(provide (protect-out (all-defined-out)))

(define-user32 LoadCursorW (_wfun _HINSTANCE _intptr -> _HCURSOR))
(define-user32 LoadIconW (_wfun _HINSTANCE _intptr -> _HICON))

(define (MAKEINTRESOURCE n) n)

(define IDI_APPLICATION     (MAKEINTRESOURCE 32512))
(define IDI_HAND            (MAKEINTRESOURCE 32513))
(define IDI_QUESTION        (MAKEINTRESOURCE 32514))
(define IDI_EXCLAMATION     (MAKEINTRESOURCE 32515))
(define IDI_WINLOGO         (MAKEINTRESOURCE 32517))
(define IDI_WARNING         IDI_EXCLAMATION)
(define IDI_ERROR           IDI_HAND)

(define IDC_ARROW           (MAKEINTRESOURCE 32512))
(define IDC_CROSS           (MAKEINTRESOURCE 32515))
(define IDC_HAND            (MAKEINTRESOURCE 32649))
(define IDC_IBEAM           (MAKEINTRESOURCE 32513))
(define IDC_WAIT            (MAKEINTRESOURCE 32514))
(define IDC_APPSTARTING     (MAKEINTRESOURCE 32650))
(define IDC_UPARROW         (MAKEINTRESOURCE 32516))
(define IDC_SIZENWSE        (MAKEINTRESOURCE 32642))
(define IDC_SIZENESW        (MAKEINTRESOURCE 32643))
(define IDC_SIZEWE          (MAKEINTRESOURCE 32644))
(define IDC_SIZENS          (MAKEINTRESOURCE 32645))
(define IDC_SIZEALL         (MAKEINTRESOURCE 32646))
(define IDC_NO              (MAKEINTRESOURCE 32648))
(define IDC_HELP            (MAKEINTRESOURCE 32651))
