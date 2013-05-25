#lang racket/base
(require ffi/unsafe
         racket/class
         racket/string
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt"
	 "../../lock.rkt")

(provide 
 (protect-out file-selector))

(define-cstruct _OPENFILENAME
  ([lStructSize _DWORD]
   [hwndOwner _HWND]
   [hInstance _HINSTANCE]
   [lpstrFilter _permanent-string/utf-16]
   [lpstrCustomFilter _permanent-string/utf-16]
   [nMaxCustFilter _DWORD]
   [nFilterIndex _DWORD]
   [lpstrFile _pointer]
   [nMaxFile _DWORD]
   [lpstrFileTitle _pointer]
   [nMaxFileTitle _DWORD]
   [lpstrInitialDir _permanent-string/utf-16]
   [lpstrTitle _permanent-string/utf-16]
   [Flags _DWORD]
   [nFileOffset _WORD]
   [nFileExtension _WORD]
   [lpstrDefExt _permanent-string/utf-16]
   [lCustData _LPARAM]
   [lpfnHook _fpointer]
   [lpTemplateName _permanent-string/utf-16]
   [pvReserved _pointer]
   [dwReserved _DWORD]
   [FlagsEx _DWORD]))

(define-comdlg32 GetSaveFileNameW (_wfun _OPENFILENAME-pointer -> _BOOL))
(define-comdlg32 GetOpenFileNameW (_wfun _OPENFILENAME-pointer -> _BOOL))

(define OFN_READONLY                 #x00000001)
(define OFN_OVERWRITEPROMPT          #x00000002)
(define OFN_HIDEREADONLY             #x00000004)
(define OFN_NOCHANGEDIR              #x00000008)
(define OFN_SHOWHELP                 #x00000010)
(define OFN_ENABLEHOOK               #x00000020)
(define OFN_ENABLETEMPLATE           #x00000040)
(define OFN_ENABLETEMPLATEHANDLE     #x00000080)
(define OFN_ALLOWMULTISELECT         #x00000200)
(define OFN_EXTENSIONDIFFERENT       #x00000400)
(define OFN_PATHMUSTEXIST            #x00000800)
(define OFN_FILEMUSTEXIST            #x00001000)
(define OFN_NOREADONLYRETURN         #x00008000)
(define OFN_EXPLORER                 #x00080000)

(define BUFFER-LEN 4096)

(define-cstruct _BROWSEINFO
  ([hwndOwner _HWND]
   [pidlRoot _pointer]
   [pszDisplayName _pointer]
   [lpszTitle _permanent-string/utf-16]
   [ulFlags _UINT]
   [lpfn _pointer]
   [lParam _LPARAM]
   [iImage _int]))

(define BIF_RETURNONLYFSDIRS #x00000001)
(define BIF_NEWDIALOGSTYLE #x00000040)

(define-cstruct _IUnknownVtbl
  ([QueryInterface _fpointer]
   [AddRef _fpointer]
   [Release (_wfun _pointer -> _ULONG)]))

(define-cstruct (_IMallocVtbl _IUnknownVtbl)
  ([Alloc _fpointer]
   [Realloc _fpointer]
   [Free (_wfun _pointer _pointer -> _void)]
   [GetSize _fpointer]
   [DidAlloc _fpointer]
   [HeapMinimize _fpointer]))

(define-cstruct _IMalloc
  ([vtbl _IMallocVtbl-pointer]))

(define (IMalloc-Free im p)
  ((IMallocVtbl-Free (IMalloc-vtbl im)) im p))
(define (IMalloc-Release im)
  ((IUnknownVtbl-Release (IMalloc-vtbl im)) im))

(define-shell32 SHBrowseForFolderW (_wfun _BROWSEINFO-pointer -> _pointer))
(define-shell32 SHGetPathFromIDListW (_wfun _pointer _pointer -> _BOOL))
(define-shell32 SHGetMalloc (_wfun (p : (_ptr o _IMalloc-pointer)) -> (r : _HRESULT)
                                   -> (if (negative? r)
                                          (error 'SHGetMalloc "failed: ~s" (bitwise-and #xFFFF r))
                                          p)))

(define (file-selector message directory filename 
                       extension
                       filters style parent)
  (if (memq 'dir style)
      (dialog-selector message directory
                       style parent)
      (do-file-selector message directory filename 
                        extension
                        filters style parent)))

(define (do-file-selector message directory filename 
                          extension
                          filters style parent)
  (atomically
   (let* ([pre-ofn
           (make-OPENFILENAME
            (ctype-sizeof _OPENFILENAME)
            (and parent
                 (send parent get-hwnd))
            hInstance
            (string-append
             (string-join
              (for/list ([f (in-list filters)])
                (format "~a\0~a" (car f) (cadr f)))
              "\0")
             "\0")
            #f
            0 
            0 ; nFilterIndex
            (malloc 'raw (* BUFFER-LEN (ctype-sizeof _short)))
            BUFFER-LEN
            #f
            0
            (and directory
                 (path->string (simplify-path directory #f)))
            message
            (bitwise-ior
             OFN_HIDEREADONLY
             (if (memq 'put style) OFN_OVERWRITEPROMPT 0)
             (if (memq 'multi style) (bitwise-ior OFN_ALLOWMULTISELECT OFN_EXPLORER) 0)
             (if directory OFN_NOCHANGEDIR 0))
            0
            0
            extension
            0
            #f
            #f
            #f
            0
            0)]
          [ofn (malloc 'raw (ctype-sizeof _OPENFILENAME))])
     (set-cpointer-tag! ofn OPENFILENAME-tag)
     (memcpy ofn pre-ofn 1 _OPENFILENAME)
     (if filename
         (let* ([filename (path->string (simplify-path filename #f))]
                [len (utf-16-length filename)])
           (memcpy (OPENFILENAME-lpstrFile ofn)
                   (cast filename _string/utf-16 _gcpointer)
                   (+ len 1)
                   _uint16))
         (ptr-set! (OPENFILENAME-lpstrFile ofn) _uint16 0))
     (let ([r (if (memq 'put style)
                  (GetSaveFileNameW ofn)
                  (GetOpenFileNameW ofn))])
       (begin0
        (and r
             (if (memq 'multi style)
                 (let ([strs
                        (let ([p (OPENFILENAME-lpstrFile ofn)])
                          (let loop ([pos 0])
                            (cond
                             [(and (zero? (ptr-ref p _byte pos))
                                   (zero? (ptr-ref p _byte (add1 pos))))
                              null]
                             [else (let ([end-pos
                                          (let loop ([pos (+ pos 2)])
                                            (cond
                                             [(and (zero? (ptr-ref p _byte pos))
                                                   (zero? (ptr-ref p _byte (add1 pos))))
                                              pos]
                                             [else (loop (+ pos 2))]))])
                                     (cons
                                      (cast (ptr-add p pos) _pointer _string/utf-16)
                                      (loop (+ end-pos 2))))])))])
		   (let ([len (length strs)])
		     (cond
		      [(len . < . 1) #f]
		      [(= len 1) (list (string->path (car strs)))]
		      [else
		       (map (lambda (p) (build-path (car strs) p))
			    (cdr strs))])))
                 (string->path (cast (OPENFILENAME-lpstrFile ofn) _pointer _string/utf-16))))
        (when directory
          (free (OPENFILENAME-lpstrInitialDir ofn)))
        (when message
          (free (OPENFILENAME-lpstrTitle ofn)))
        (free (OPENFILENAME-lpstrFilter ofn))
        (free (OPENFILENAME-lpstrFile ofn)))))))

(define MAX_PATH 4096)

(define (dialog-selector message directory
                         style parent)
  (atomically
   (let ([pre-bi (make-BROWSEINFO
                  (and parent
                       (send parent get-hwnd))
                  #f
                  (malloc 'raw MAX_PATH _uint16)
                  message
                  (bitwise-ior BIF_NEWDIALOGSTYLE BIF_RETURNONLYFSDIRS)
                  #f
                  0
                  0)]
         [bi (malloc 'raw (ctype-sizeof _BROWSEINFO))])
     (set-cpointer-tag! bi BROWSEINFO-tag)
     (memcpy bi pre-bi 1 _BROWSEINFO)
     (let ([r (SHBrowseForFolderW bi)])
       (begin0
        (and r
             (let ([ok (SHGetPathFromIDListW r (BROWSEINFO-pszDisplayName bi))])
               (and ok
                    (let ([mi (SHGetMalloc)])
                      (IMalloc-Free mi r)
                      (IMalloc-Release mi))
                    (string->path
                     (cast (BROWSEINFO-pszDisplayName bi) _pointer _string/utf-16)))))
        (free (BROWSEINFO-pszDisplayName bi))
        (when message
          (free (BROWSEINFO-lpszTitle bi))))))))
