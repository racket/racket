#lang racket/base

(require tests/eli-tester scribble/text/wrap)

#; ; generate test cases from Emacs
(defmacro wrap-line (str col &rest dummy)
  (let ((r (with-temp-buffer
            (fundamental-mode)
            (insert str)
            (let (; straightforward filling, no fanciness
                  (left-margin 0) (fill-column col)
                  (adaptive-fill-regexp "\\`\\'")
                  (adaptive-fill-first-line-regexp "\\`\\'")
                  (filladapt-mode nil))
              (fill-region-as-paragraph (point-min) (point-max) nil t)
              (format "'%S" (split-string
                             (buffer-substring (point-min) (point-max))
                             "\n"))))))
    `(insert ,r)))

(provide wrap-tests)
(module+ main (wrap-tests))
(define (wrap-tests)
  (define (w line width result1 result2)
    (test (wrap-line line width) => result1
          (wrap-line line width
                     (λ (w n) (values (substring w 0 n) (substring w n) #f)))
          => result2))
  (test (w "eli barzilay maze is life" 25
           '("eli barzilay maze is life")
           '("eli barzilay maze is life"))
        (w "eli barzilay maze is life" 10
           '("eli" "barzilay" "maze is" "life")
           '("eli barzil" "ay maze is" "life"))
        (w "eli barzilay maze is life" 11
           '("eli" "barzilay" "maze is" "life")
           '("eli barzila" "y maze is l" "ife"))
        (w "eli barzilay maze is life" 12
           '("eli barzilay" "maze is life")
           '("eli barzilay" "maze is life"))
        (w "eli barzilay maze is life" 13
           '("eli barzilay" "maze is life")
           '("eli barzilay" "maze is life"))
        (w "eli barzilay maze is life" 14
           '("eli barzilay" "maze is life")
           '("eli barzilay m" "aze is life"))
        (w "eli  barzilay maze is life" 15
           '("eli  barzilay" "maze is life")
           '("eli  barzilay m" "aze is life"))
        (w "eli barzilay  maze is life" 15
           '("eli barzilay" "maze is life")
           '("eli barzilay  m" "aze is life"))
        (w "eli barzilay maze  is life" 15
           '("eli barzilay" "maze  is life")
           '("eli barzilay ma" "ze  is life"))
        (w "0123456789" 10
           '("0123456789")
           '("0123456789"))
        (w "     xxxxx" 10
           '("     xxxxx")
           '("     xxxxx"))
        (w "     xxxxxx" 10
           '("     xxxxxx")
           '("     xxxxx" "x"))
        (w "     xxxxxxxxxxx" 10
           '("     xxxxxxxxxxx")
           '("     xxxxx" "xxxxxx"))
        (w "xxxxx     " 10
           '("xxxxx     ")
           '("xxxxx     "))
        (w "xxxxxx     " 10
           '("xxxxxx     ")
           '("xxxxxx    " " "))
        (w " xxxxx     " 10
           '(" xxxxx     ")
           '(" xxxxx    " " "))
        (w "     x     " 10
           '("     x     ")
           '("     x    " " "))
        (w "" 10
           '("")
           '(""))
        (w "     " 10
           '("     ")
           '("     "))
        (w "          " 10
           '("          ")
           '("          "))
        (w "           " 10
           '("           ")
           '("          " " "))
        (w "xxxxxxxxxxx     " 10
           '("xxxxxxxxxxx     ")
           '("xxxxxxxxxx" "x     "))
        (w "     xxxxxxxxxxx " 10
           '("     xxxxxxxxxxx ")
           '("     xxxxx" "xxxxxx "))
        (w "     xxxxxxxxxxx  " 10
           '("     xxxxxxxxxxx  ")
           '("     xxxxx" "xxxxxx  "))
        (w "     xxxxxxxxxxx x" 10
           '("     xxxxxxxxxxx" "x")
           '("     xxxxx" "xxxxxx x"))
        (w "     xxxxxxxxxxx  x" 10
           '("     xxxxxxxxxxx" "x")
           '("     xxxxx" "xxxxxx  x"))
        (w "x x x x x x x x x x x" '(10 . 8)
           '("x x x x x" "x x x x" "x x")
           '("x x x x x" "x x x x" "x x"))
        (w "xx x x x x x x x x x x" '(10 . 8)
           '("xx x x x x" "x x x x" "x x")
           '("xx x x x x" "x x x x" "x x"))
        (w "x x x x xxx x x x x x" '(10 . 8)
           '("x x x x" "xxx x x" "x x x")
           '("x x x x xx" "x x x x" "x x"))
        (w "xxxx xxxx xxxx xxxx" '(10 . 5)
           '("xxxx xxxx" "xxxx" "xxxx")
           '("xxxx xxxx" "xxxx" "xxxx"))
        (w "xxxx xxxxxxxx xxxx" '(10 . 5)
           '("xxxx" "xxxxxxxx" "xxxx")
           '("xxxx xxxxx" "xxx x" "xxx"))))
