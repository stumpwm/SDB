(asdf:defsystem #:swm-debugger-mode
  :name "SWM CLIM Debugger"
  :depends-on (#:stumpwm #:clim-debugger #:clim-listener)
  :serial t
  :components ((:file "package")
               (:file "swm-debugger-mode")))
