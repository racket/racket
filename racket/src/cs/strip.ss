
(for-each (lambda (so)
            (when (file-exists? so)
              (printf "Stripping ~s\n" so)
              (strip-fasl-file so so (fasl-strip-options inspector-source source-annotations))))
          (command-line-arguments))
