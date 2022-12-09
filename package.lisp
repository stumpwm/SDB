
(defpackage :swm-debugger-mode
  (:use :cl)
  (:export #:*debugger-width*
           #:*debugger-height*
           #:*debugger-coordinates*
           #:*backtrace-right-margin*
           #:*override-debugger*
           #:invoke-sdb
           #:with-sdb
           #:sdb-mode
           
           #:*swm-debugger-mode-enable-hook*
           #:*swm-debugger-mode-disable-hook*))
