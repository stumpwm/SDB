
(defpackage :swm-debugger-mode
  (:use :cl)
  (:export #:*debugger-width*
           #:*debugger-height*
           #:*debugger-coordinates*
           #:invoke-swm-debugger
           #:with-swm-debugger
           #:sdb-mode
))
