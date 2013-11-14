#;#;
#<<END
TR info: fixnum-no-bound.rkt 22:0 (fx+ (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 23:0 (fx- (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 24:0 (fx* (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 25:0 (fxquotient (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 26:0 (fxmodulo (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
TR info: fixnum-no-bound.rkt 27:0 (fxremainder (ann 2 Fixnum) (ann 3 Fixnum)) -- non-optimized fixnum op
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
(require racket/fixnum)
(fx+ (ann 2 Fixnum) (ann 3 Fixnum))
(fx- (ann 2 Fixnum) (ann 3 Fixnum))
(fx* (ann 2 Fixnum) (ann 3 Fixnum))
(fxquotient (ann 2 Fixnum) (ann 3 Fixnum))
(fxmodulo (ann 2 Fixnum) (ann 3 Fixnum))
(fxremainder (ann 2 Fixnum) (ann 3 Fixnum))
