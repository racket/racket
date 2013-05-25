#lang racket/base
(require ffi/unsafe)

;; Unfortunately, we sometimes need to do something different
;;  under Windows XP

(provide xp?)

(define xp? 
  (and (eq? 'windows (system-type))
       (let* ([abi (and (equal? "win32\\i386" 
				(path->string (system-library-subpath #f)))
			'stdcall)]
	      [GetVersion (get-ffi-obj 'GetVersion
				       (ffi-lib "kernel32.dll")
				       (_fun #:abi abi -> _int32))])
	 (= 5 (bitwise-and #xFF (GetVersion))))))
