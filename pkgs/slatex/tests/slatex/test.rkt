#lang racket
(require slatex/slatex-wrapper
         tests/eli-tester
         scheme/runtime-path)

(define-runtime-path slatex-file-pth '(lib "slatex"))
(define slatex-pth (path-only slatex-file-pth))

(define tmp-file (build-path (current-directory) "test.tex")
  #;(make-temporary-file "slatex~a.tex" #f (current-directory)))

(test
 (putenv "TEXINPUTS" (format "~a:" (path->string slatex-pth)))
 tmp-file
 
 (with-output-to-file tmp-file #:exists 'truncate/replace
   (lambda ()
     (display #<<END
\documentclass{article} 
\usepackage{slatex} 
% 
%That was LaTeX2e.  If you use 
%LaTeX2.09, or LaTeX2e in 
%2.09 compatibility mode, use 
% 
%\documentstyle[slatex]{article} 
% 
%or 
% 
%\documentstyle{article} 
%\input slatex.sty 
\begin{document} 
 
In Scheme, the expression 
\scheme|(set! x 42)| returns 
an unspecified value, rather 
than \scheme'42'.  However, 
one could get a \scheme{set!} 
of the latter style with: 
 
\begin{schemedisplay} 
(define-syntax setq 
  (syntax-rules () 
    [(setq x a) 
     (begin (set! x a) 
            x)])) 
\end{schemedisplay} 
 
\end{document} 
END
              )))
 
 (slatex (path->string tmp-file))
 
 (with-handlers ([exn:fail:filesystem? void])
   (delete-file (path-replace-suffix tmp-file #".aux")))
 (with-handlers ([exn:fail:filesystem? void])
   (delete-file (path-replace-suffix tmp-file #".log")))
 (with-handlers ([exn:fail:filesystem? void])
   (delete-file (path-replace-suffix tmp-file #".dvi")))
 
 (delete-file tmp-file))
