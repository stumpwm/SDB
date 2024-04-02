
(defpackage :swm-debugger-mode
  (:use :cl)
  (:export #:*debugger-width*
           #:*debugger-height*
           #:*debugger-coordinates*
           #:*default-eval-package*
           #:*backtrace-right-margin*
           #:*position-display*
           #:*snippet-display*
           #:*snippet-lines*
           #:*appropriate-debugger-hook*
           #:*inform-on-grab-status*
           #:*font-size*
           #:*swm-debugger-mode-enable-hook*
           #:*swm-debugger-mode-disable-hook*
           
           #:invoke-sdb
           #:with-sdb
           #:sdb-mode

           #:display-position-as-line
           #:display-snippet-lines-around
           
           #:swm-debugger-mode))
