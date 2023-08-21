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
                   [correlated-property-symbol-keys syntax-property-symbol-keys]
                   [raise-argument-error raise-argument-error/primitive]
                   [raise-argument-error/user raise-argument-error]
                   [raise-arguments-error raise-arguments-error/primitive]
                   [raise-arguments-error/user raise-arguments-error]
                   [raise-range-error raise-range-error/primitive]
                   [raise-range-error/user raise-range-error])
           (thread)
           (io)
           (regexp)
           (linklet)
           (rename (only $system
                         $lambda/lift-barrier
                         $begin-unsafe)
                   [$begin-unsafe begin-unsafe])))
