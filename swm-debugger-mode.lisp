
(in-package :swm-debugger-mode)

(defvar *debugger-width* 600
  "The width of the debugger")

(defvar *debugger-height* 480
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

(clim:define-gesture-name :show-location :keyboard (#\v :meta))
;; (clim:define-gesture-name :more :keyboard (#\m :meta))
;; (clim:define-gesture-name :exit :keyboard (#\q :meta))
;; (clim:define-gesture-name :eval :keyboard (#\e :meta))
;; (clim:define-gesture-name :toggle :keyboard (#\tab :meta))

(defvar *show-source-locations* '#1=(nil :current :all . #1#))

(clim-debugger::define-clim-debugger-command
    (com-show-locations :name "Toggle display of source locations"
                        :keystroke :show-location)
    ()
  (setf *show-source-locations* (cdr *show-source-locations*)))

(defvar *position-display* nil
  "A function to call instead of displaying the file position as is.
Called with the file, position and stream.")

(defvar *snippet-display* nil
  "A function to call instead of displaying the snippet as is.
Called with the file, position, snippet, and stream")

(defvar *snippet-lines* 5
  "The number of lines from the snippet to display.")

(defun n-lines (string n)
  (if (zerop n)
      ""
      (let ((lines (if (listp string)
                       string
                       (cl-ppcre:split (coerce '(#\newline) 'string) string))))
        (do* ((l lines (cdr l))
              (line (car l) (concatenate 'string line '(#\newline) (car l)))
              (i 1 (1+ i)))
             ((or (null l) (= i n))
              line)))))

(defun position->line-number (position file &optional (newline-chars '(#\newline)))
  (let ((r 1))
    (with-open-file (f file)
      (loop repeat position
            for ch = (read-char f)
            when (member ch newline-chars :test #'char=)
              do (incf r)))
    r))

(defun display-position-as-line (file position stream)
  "Display the position in the file as a line number"
  (format stream "Line ~D" (position->line-number position file)))

(defun display-snippet-lines-around (file position snippet stream)
  "Display the lines surrounding POSITION instead of SNIPPET."
  (declare (ignore snippet))
  (let* ((line (position->line-number position file))
         (pre (floor *snippet-lines* 2))
         (post (if (oddp *snippet-lines*)
                   (floor *snippet-lines* 2)
                   (ceiling *snippet-lines* 2)))
         (discard (- line pre 1))
         (tstr (format nil "~D" (+ line *snippet-lines*)))
         (total (length tstr))
         (fstr (concatenate 'string "~D~" (format nil "~D" total) "T: ")))
    (labels ((readl (stream)
               (read-line stream nil nil))
             (rewrite (f line-count offset)
               (loop for i from 1 to line-count
                     for line-in = (readl f)
                     do (cond (line-in
                               (write-string (format nil fstr (+ line offset i))
                                             stream)
                               (write-string line-in stream))
                              (t (write-string "~" stream)))
                        (terpri stream))))
      (clim:with-text-family (stream :fix)
        (with-open-file (f file)
          (dotimes (v discard)
            (readl f))
          (fresh-line stream)
          (rewrite f pre (1- (- pre)))
          (clim:with-text-face (stream :bold)
            (write-string (format nil fstr line) stream)
            (write-string (readl f) stream)
            (terpri stream))
          (rewrite f post 1)
          (fresh-line stream))))))

(clim:define-presentation-method clim:present :after
  (object (type clim-debugger::stack-frame) stream
          (view clim-debugger::maximized-stack-frame-view)
          &key acceptably for-context-type)
  ;; Define a presentation method for a stack frame that appends the source
  ;; location information when applicable. 
  (declare (ignore acceptably for-context-type))
  (when (and *show-source-locations*
             (or (eql (car *show-source-locations*) :all)
                 (and (eql (car *show-source-locations*) :current)
                      ;; Protect against being passed non-debugger pane streams.
                      (ignore-errors (= (clim-debugger::active-frame stream)
                                        (clim-debugger::frame-no object))))))
    (clim:with-text-face (stream :bold)
      (write-string "Location Information" stream))
    (fresh-line stream)
    (clim:indenting-output (stream ">")
      (let* ((source-info (swank:frame-source-location
                           (clim-debugger::frame-no object)))
             (type (unless (atom source-info) (car source-info))))
        (cond ((eql type :location)
               (let ((file (assoc :file (cdr source-info)))
                     (snippet (assoc :snippet (cdr source-info)))
                     (position (assoc :position (cdr source-info))))
                 (labels ((gencont (thing stream)
                            (lambda ()
                              (cond ((stringp thing)
                                     (write-string thing stream))
                                    ((null thing)
                                     (write-string "NOT PROVIDED" stream))
                                    (t (write thing :stream stream)))))
                          (write-with-newlines (stream cont)
                            (fresh-line stream)
                            (funcall cont))
                          (write-elided (stream cont)
                            (write-string "..." stream)
                            (fresh-line stream)
                            (funcall cont)
                            (fresh-line stream)
                            (write-string "..." stream))
                          (try-write (thing stream &optional (how :plain))
                            (let ((cont (gencont thing stream)))
                              (case how
                                ((:plain)
                                 (funcall cont))
                                ((:newline)
                                 (write-with-newlines stream cont))
                                ((:elided)
                                 (write-elided stream cont))
                                ((:elided-newline)
                                 (write-with-newlines
                                  stream
                                  (lambda ()
                                    (write-elided stream cont))))
                                (otherwise
                                 (funcall cont))))))
                   (clim:with-text-face (stream :bold)
                     (write-string "File: " stream))
                   (try-write (cadr file) stream)
                   (fresh-line stream)
                   (clim:with-text-face (stream :bold)
                     (write-string "Position: " stream))
                   (if *position-display*
                       (funcall *position-display*
                                (cadr file)
                                (cadr position)
                                stream)
                       (try-write (cadr position) stream))
                   (fresh-line stream)
                   (when (or *snippet-lines* *snippet-display*)
                     (clim:with-text-face (stream :bold)
                       (write-string "Snippet:" stream))
                     (clim:indenting-output (stream ">")
                       (if *snippet-display*
                           (funcall *snippet-display*
                                    (cadr file)
                                    (cadr position)
                                    (cadr snippet)
                                    stream)
                           (try-write (if (numberp *snippet-lines*)
                                          (n-lines (cadr snippet) *snippet-lines*)
                                          (cadr snippet))
                                      stream :elided-newline))))
                   (fresh-line stream))))
              ((eql type :error)
               (write-string "Error aquiring source location information" stream))
              (t (write-string "Unable to determine source location information"
                               stream)))))
    (fresh-line stream)))

(defmethod clime:find-frame-type ((frame swm-debugger))
  :override-redirect)

(defmethod clim:default-frame-top-level :before ((frame swm-debugger)
                                                 &key &allow-other-keys)
  ;; Resize the frame.
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
    (setf (clim-debugger::frame-current-layout debugger-frame)
          'clim-debugger::without-interactor)
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
                  (xlib:display-finish-output stumpwm:*display*)
                  ;; Without this we end up failing to grab the keyboard and
                  ;; keypresses go through to the previously focused window.
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
  (let ((block (gensym)))
    `(block ,block
       (unwind-protect
            (let ((*debugger-hook* #'debugger-hook-call-sdb))
              (sb-debug::enable-debugger)
              (restart-case
                  (handler-bind ((,condition-type #'invoke-sdb))
                    ,@body)
                (quit-sdb (&optional value)
                  :report "Return from WITH-SDB form"
                  (return-from ,block value))))
         (sb-debug::disable-debugger)))))

(clim-debugger::define-clim-debugger-command
    (clim-debugger::com-eval :name "SDB Eval in frame"
                             :keystroke :eval
                             :menu t)
    ()
  (let* ((dbg-pane (clim:find-pane-named clim:*application-frame*
                                         'clim-debugger::debugger-pane))
         (active-frame (clim-debugger::active-frame dbg-pane))
         (pkg (swank-backend:frame-package active-frame)))
    (multiple-value-bind (object type)
        (let ((clim:*command-dispatchers* '(#\,)))
          (clim:with-text-face (*standard-output* :bold)
            (format *standard-output* "Eval in frame ~D (~A)>"
                    active-frame (package-name pkg)))
          (clim:accept 'clim::command-or-form
                       :default-type 'empty-input
                       :prompt " "
                       :prompt-mode :raw))
      (flet ((ev (form)
               (let ((values (multiple-value-list
                              (swank:eval-string-in-frame (format nil "~S" object)
                                                          active-frame
                                                          pkg))))
                 (format *standard-output* "~&~{~A~^~%~}" values))))
        (cond ((clim:presentation-subtypep type 'empty-input)
               (ev '(values)))
              ((clim:presentation-subtypep type 'clim::command)
               (climi::ensure-complete-command object
                                               (clim:frame-command-table
                                                clim:*application-frame*)
                                               *standard-input*))
              (t (ev object)))))))

(clim-debugger::define-clim-debugger-command
    (clim-debugger::com-invoke-restart :name "Invoke restart")
    ((restart 'clim-debugger::restart :gesture :select))
  (setf (clim-debugger::returned-restart clim::*application-frame*) restart)
  (invoke-restart-interactively restart)
  (clim:frame-exit clim::*application-frame*))

(macrolet
    ((define ()
       (flet ((define-one (number)
                (let* ((char (digit-char number))
                       (name (alexandria:symbolicate "INVOKE-RESTART-" char)))
                  `(clim-debugger::define-clim-debugger-command
                       (,name :keystroke (,char :control)) ()
                     (alexandria:when-let*
                         ((pane (clim:find-pane-named
                                 clim:*application-frame*
                                 'clim-debugger::debugger-pane))
                          (restart
                           (nth ,number
                                (clim-debugger::restarts
                                 (clim-debugger::condition-info pane)))))
                       (clim-debugger::com-invoke-restart restart))))))
         `(progn ,@(loop :for i :to 9 :collect (define-one i))))))
  (define))

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
  (sb-debug::enable-debugger)
  (unless (eq *debugger-hook* #'debugger-hook-call-sdb)
    (setf *holdover-debugger-hook* *debugger-hook*))
  (setf *debugger-hook* #'debugger-hook-call-sdb))

(defun uninstall-dbg (&rest rest)
  (declare (ignore rest))
  (sb-debug::disable-debugger)
  (setf *debugger-hook* *holdover-debugger-hook*))

(stumpwm:add-hook *swm-debugger-mode-enable-hook* #'install-dbg)
(stumpwm:add-hook *swm-debugger-mode-disable-hook* #'uninstall-dbg)
