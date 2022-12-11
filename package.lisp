
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

           #:*position-display*
           #:*snippet-display*
           #:*snippet-lines*
           #:display-position-as-line
           #:display-snippet-lines-around
           
           #:*swm-debugger-mode-enable-hook*
           #:*swm-debugger-mode-disable-hook*))
