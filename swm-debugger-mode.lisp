
(in-package :swm-debugger-mode)

(defvar *debugger-width* 480
  "The width of the debugger")

(defvar *debugger-height* 600
  "The height of the debugger")

(defvar *debugger-coordinates* (cons 10 10)
  "the coordinates the debugger should be located at")

(defvar *default-eval-package* (find-package :stumpwm)
  "The default package to eval in if swank cannot find a package")

(defvar *override-debugger* nil
  "Set to a function to override SDB.")

(defvar *backtrace-right-margin* 100
  "The right margin the pretty printer should respect when printing backtraces.")

(defclass swm-debugger (clim-debugger::clim-debugger) ())

(clim-debugger::define-clim-debugger-command (com-swm-refresh-size-debugger)
    ((width 'integer) (height 'integer))
  (let ((toplevel (clim:frame-top-level-sheet clim:*application-frame*)))
    (when toplevel
      (clim:move-and-resize-sheet toplevel
                                  (car *debugger-coordinates*)
                                  (cdr *debugger-coordinates*)
                                  width
                                  height))))

(defmethod clime:find-frame-type ((frame swm-debugger))
  :override-redirect)

(defmethod clim:default-frame-top-level :before ((frame swm-debugger)
                                                 &key &allow-other-keys)
  ;; Resize the frame.
  (setf (clim:frame-current-layout frame) 'clim-debugger::with-interactor)
  (clim:execute-frame-command frame
                              (list 'com-swm-refresh-size-debugger
                                    (or *debugger-width*
                                        (stumpwm::head-width
                                         (stumpwm::current-head)))
                                    (or *debugger-height*
                                        (stumpwm::head-height
                                         (stumpwm::current-head))))))

(defmethod clim:default-frame-top-level :around ((frame swm-debugger)
                                                 &key &allow-other-keys)
  (let* ((tls (clim:frame-top-level-sheet frame))
         (mirror (clim:sheet-direct-mirror tls))
         (window (clim-clx::window mirror))
         (display (xlib:window-display window))
         (grab (xlib:grab-keyboard window)))
    (declare (ignorable grab))
    (stumpwm:message "SDB grab status: ~A" grab)
    (unwind-protect (call-next-method)
      (xlib:ungrab-keyboard display))))

(defun swm-debugger (condition me-or-my-encapsulation)
  "The StumpWM Debugger. Runs the CLIM debugger on a given condition."
  (declare (ignore me-or-my-encapsulation))
  (let ((debugger-frame (clim:make-application-frame 'swm-debugger))
        ;; dynamically bind these cause swank has trouble finding them.
        (swank::*buffer-package* *default-eval-package*)
        (swank::*buffer-readtable* *readtable*))
    (swank-backend::call-with-debugging-environment
     (lambda ()
       ;; make sure that pointer grabs are properly restored.
       (let ((pointer-grabs stumpwm::*grab-pointer-count*)
             (stumpwm::*grab-pointer-count* 0))
         (when (> pointer-grabs 0)
           (stumpwm:ungrab-pointer))
         (unwind-protect
              (setf (clim-debugger::the-condition debugger-frame)
                    (clim-debugger::make-debugger-info
                     condition
                     (compute-restarts)
                     (clim-debugger::compute-backtrace 0 nil)))
           (unwind-protect
                (let ((*debugger-hook*
                        (lambda (c e)
                          ;; manually unfocus the current frame to make way for
                          ;; the next debugger frame. Needed to make sure that
                          ;; the user can type again after returning from a
                          ;; sub-debugger.
                          (let* ((frame debugger-frame)
                                 (tls (clim:frame-top-level-sheet frame))
                                 (mirror (clim:sheet-direct-mirror tls))
                                 (window (clim-clx::window mirror))
                                 (display (xlib:window-display window)))
                            (xlib:ungrab-keyboard display)
                            (unwind-protect (swm-debugger c e)
                              (let ((grab (xlib:grab-keyboard window)))
                                (stumpwm:message "Regrab status: ~A" grab)))))))
                  (clim:run-frame-top-Level debugger-frame))
             (when (> pointer-grabs 0)
               (stumpwm:grab-pointer (stumpwm:current-screen))))
           (let ((restart (clim-debugger::returned-restart debugger-frame)))
             ;; If we invoked a restart, display a message of which restart we
             ;; invoked. Otherwise, we can safely exit to the toplevel. If no
             ;; toplevel restart is found (it should be, cause it is established
             ;; early on) then abort.
             (if restart
                 (stumpwm:message "invoked restart ~A" restart)
                 (let ((top-level (find-restart 'stumpwm::top-level)))
                   (if top-level
                       (progn (stumpwm:message "~A" top-level)
                              (invoke-restart top-level))
                       (abort)))))))))))

(defun get-debug-function ()
  "Return the current debug function."
  (or *override-debugger*
      (symbol-function 'swm-debugger)))

(defun debugger-hook-call-sdb (c e)
  "Call the current debug function."
  (funcall (get-debug-function) c e))

(defun invoke-sdb (c)
  "explicitly invoke SDB"
  (swm-debugger c (symbol-function 'swm-debugger)))

(defmacro with-sdb (condition-type &body body)
  "Invoke SDB for specific condition types signalled within body."
  `(restart-case
       (handler-bind ((,condition-type #'invoke-sdb))
         ,@body)
     (quit-sdb () nil)))

(clim-debugger::define-clim-debugger-command
    (clim-debugger::com-invoke-restart :name "Invoke restart")
    ((restart 'clim-debugger::restart :gesture :select))
  (setf (clim-debugger::returned-restart clim::*application-frame*) restart)
  (invoke-restart-interactively restart)
  (clim:frame-exit clim::*application-frame*))

(clim-debugger::define-clim-debugger-command
    (clim-debugger::com-print-backtrace :name "Print Backtrace" :menu t)
    ((file 'string :prompt "File Path" :default "~/SDB-backtrace.txt"))
  (with-open-file (f file :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((debug-info (clim-debugger::the-condition clim:*application-frame*))
          (*print-right-margin* *backtrace-right-margin*))
      (format f "SDB CONDITION INFORMATION:~%~%~2TLiteral Condition: ~S~%~2TPretty Condition:  ~A~&~2TCondition Message: ~A~&~2TCondition Type:    ~S~&~2TCondition Extra:   ~A~%~%SDB BACKTRACE:~%~%"
              (clim-debugger::the-condition debug-info)
              (clim-debugger::the-condition debug-info)
              (clim-debugger::condition-message debug-info)
              (clim-debugger::type-of-condition debug-info)
              (clim-debugger::condition-extra debug-info))
      (loop for frame in (clim-debugger::backtrace debug-info)
            for numstr = (format nil "[~D]" (clim-debugger::frame-no frame))
            for blankstr = (make-string (length numstr) :initial-element #\space)
            do (format f "~&~A ~A~&~A Locals:"
                       numstr (clim-debugger::frame-string frame) blankstr)
               (loop for local in (clim-debugger::frame-variables frame)
                     do (format f "~&~A   ~A -> ~S"
                                blankstr
                                (getf local :name)
                                (getf local :value)))))
    (finish-output f)))

(stumpwm:define-minor-mode swm-debugger-mode () ()
  (:scope :unscoped)
  (:interactive sdb-mode)
  (:lighter "SDB"))

(defvar *swm-debuger-mode-control-i-map* (stumpwm:make-sparse-keymap))

(define-swm-debugger-mode-command invoke-sdb-without-condition () ()
  (invoke-sdb (make-condition 'simple-error :format-control "Dummy error")))

(stumpwm:define-key *swm-debuger-mode-control-i-map* (stumpwm:kbd "d")
  "invoke-sdb-without-condition")


(stumpwm:define-key *swm-debugger-mode-root-map* (stumpwm:kbd "C-i")
  '*swm-debuger-mode-control-i-map*)

(defvar *holdover-debugger-hook* nil)

(defun install-dbg (&rest rest)
  (declare (ignore rest))
  (unless (eq *debugger-hook* #'debugger-hook-call-sdb)
    (setf *holdover-debugger-hook* *debugger-hook*))
  (setf *debugger-hook* #'debugger-hook-call-sdb))

(defun uninstall-dbg (&rest rest)
  (declare (ignore rest))
  (setf *debugger-hook* *holdover-debugger-hook*))

(stumpwm:add-hook *swm-debugger-mode-enable-hook* #'install-dbg)
(stumpwm:add-hook *swm-debugger-mode-disable-hook* #'uninstall-dbg)
