#;#;
#<<END
TR missed opt: invalid-float-comp.rkt 13:0 (< 1.0 2) -- generic comparison -- caused by: 13:7 2
END
#<<END
#t

END

#lang typed/scheme
#:optimize

(< 1.0 2)
