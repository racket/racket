(define environment-imports
  `(import (rename (rumble)
                   [correlated? syntax?]
                   [correlated-source syntax-source]
                   [correlated-line syntax-line]
                   [correlated-column syntax-column]
                   [correlated-position syntax-position]
                   [correlated-span syntax-span]
                   [correlated-e syntax-e]
                   [correlated->datum syntax->datum]
                   [datum->correlated datum->syntax]
                   [correlated-property syntax-property]
                   [correlated-property-symbol-keys syntax-property-symbol-keys])
           (thread)
           (io)
           (regexp)
           (linklet)))
