#;#;
#<<END
TR info: fixnum-no-bound.rkt 2:0 (fx+ (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 3:0 (fx- (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 4:0 (fx* (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 5:0 (fxquotient (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 6:0 (fxmodulo (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 7:0 (fxremainder (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
END
#<<END
5
-1
6
0
2
2

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(require racket/fixnum)
(fx+ (ann 2 Fixnum) (ann 3 Fixnum))
(fx- (ann 2 Fixnum) (ann 3 Fixnum))
(fx* (ann 2 Fixnum) (ann 3 Fixnum))
(fxquotient (ann 2 Fixnum) (ann 3 Fixnum))
(fxmodulo (ann 2 Fixnum) (ann 3 Fixnum))
(fxremainder (ann 2 Fixnum) (ann 3 Fixnum))
