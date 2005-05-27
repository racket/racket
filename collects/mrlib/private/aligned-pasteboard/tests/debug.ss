(module debug mzscheme
  (require
   (lib "class.ss"))
  
  (provide
   debug-snip
   debug-pasteboard
   debug-canvas)
  
  ;;debug-snip: -> (void)
  ;;get the relevant info about the snip that contains the two others pasteboards
  (define debug-snip
    (lambda (snip)
      (printf "--- aligned-editor-snip% --\n")
      (let ((l (box 0))
            (t (box 0))
            (r (box 0))
            (b (box 0)))
        (send snip get-inset l t r b)
        (printf "get-inset: ~sX~s  ~sX~s\n" (unbox l) (unbox r) (unbox t) (unbox b)))
      
      (let ((l (box 0))
            (t (box 0))
            (r (box 0))
            (b (box 0)))
        (send snip get-margin l t r b)
        (printf "get-margin: ~sX~s  ~sX~s\n" (unbox l) (unbox r) (unbox t) (unbox b)))
      
      (printf "get-max-height: ~s~n" (send snip get-max-height))
      (printf "get-max-width: ~s~n" (send snip get-max-width))
      (printf "get-min-height: ~s~n" (send snip get-min-height))
      (printf "get-min-width: ~s~n" (send snip get-min-width))
      ;(printf "snip-width: ~s~n" (send pasteboard snip-width snip))
      ;(printf "snip-height: ~s~n" (send pasteboard snip-height snip))
      ))
  
  ;;debug-pasteboard: -> (void)
  ;;displays to the repl the sizes i'm interested in
  (define debug-pasteboard
    (lambda (pasteboard)
      (printf "--- aligned-pasteboard% ---\n")
      (let ((tmp1 (box 0))
            (tmp2 (box 0)))
        (send pasteboard get-extent tmp1 tmp2)
        (printf "get-extent: ~sX~s\n" (unbox tmp1) (unbox tmp2)))
      (printf "get-max-height: ~s\n" (send pasteboard get-max-height))
      (let ((tmp (call-with-values (lambda () (send pasteboard get-max-view-size)) cons)))
        (printf "get-max-view-size: ~sX~s\n" (car tmp) (cdr tmp)))
      (printf "get-max-width: ~s\n" (send pasteboard get-max-width))
      (printf "get-min-height: ~s\n" (send pasteboard get-min-height))
      (printf "get-min-width: ~s\n" (send pasteboard get-min-width))
      (let ((tmp1 (box 0))
            (tmp2 (box 0)))
        (send pasteboard get-view-size tmp1 tmp2)
        (printf "get-view-size: ~sX~s\n" (unbox tmp1) (unbox tmp2)))
      ))
  
  ;;debug-canvas: -> (void)
  ;;just some help counting pixels
  (define debug-canvas
    (lambda (canvas)
      (printf "--- aligned-editor-canvas% ---\n")
      ;;values
      (let ((tmp (call-with-values (lambda () (send canvas get-client-size)) cons)))
        (printf "~a: ~sX~s\n" (symbol->string (quote get-client-size)) (car tmp) (cdr tmp)))
      (let ((tmp (call-with-values (lambda () (send canvas get-graphical-min-size)) cons)))
        (printf "~a: ~sX~s\n" (symbol->string (quote get-graphical-min-size)) (car tmp) (cdr tmp)))
      (let ((tmp (call-with-values (lambda () (send canvas get-size)) cons)))
        (printf "~a: ~sX~s\n" (symbol->string (quote get-size)) (car tmp) (cdr tmp)))
      ;;1 value
      (printf "~a: ~s\n" (symbol->string (quote get-height)) (send canvas get-height))
      (printf "~a: ~s\n" (symbol->string (quote get-width)) (send canvas get-width))
      (printf "~a: ~s\n" (symbol->string (quote horiz-margin)) (send canvas horiz-margin))
      (printf "~a: ~s\n" (symbol->string (quote min-client-height)) (send canvas min-client-height))
      (printf "~a: ~s\n" (symbol->string (quote min-client-width)) (send canvas min-client-width))
      (printf "~a: ~s\n" (symbol->string (quote min-height)) (send canvas min-height))
      (printf "~a: ~s\n" (symbol->string (quote min-width)) (send canvas min-width))
      (printf "~a: ~s\n" (symbol->string (quote vert-margin)) (send canvas vert-margin))
      ))
  )