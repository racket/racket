#lang typed/scheme

(ann 1.1+2.0i Inexact-Complex)
(ann 1+2.0i Inexact-Complex)
(ann (real-part 1.1+2.0i) Float)
(ann (real-part 1+2.0i) Float)
(ann (imag-part 1.1+2.0i) Float)
(ann (+ 2.0 2.0+2.0i) Inexact-Complex)
(ann (+ 2 2.0+2.0i) Inexact-Complex)
