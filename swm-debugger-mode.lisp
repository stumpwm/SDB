
(in-package :swm-debugger-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Prevent SDB from being reloaded while it is running, e.g. via loadrc. 
  (defvar *sdb-loaded-p* nil)
  (when (and *sdb-loaded-p*
             (stumpwm:minor-mode-enabled-p 'swm-debugger-mode))
    (restart-case
        (error "SDB is currently enabled, cannot reload file. Disable SDB and try again.")
      (disable-sdb ()
        :report "Disable SDB and continue"
        (stumpwm:disable-minor-mode 'swm-debugger-mode))
      (ignore ()
        :report "«DANGER» Ignore SDB being loaded and re-evaluate the file anyway"
        nil))))

(defvar *debugger-width* 600
  "The width of the debugger")

(defvar *debugger-height* 480
  "The height of the debugger")

(defvar *debugger-coordinates* (cons 10 10)
  "the coordinates the debugger should be located at")

(defvar *default-eval-package* (find-package :stumpwm)
  "The default package to eval in if swank cannot find a package")

(defvar *show-source-locations* '#1=(nil :current :all . #1#))

(defvar *backtrace-right-margin* 100
  "The right margin the pretty printer should respect when printing backtraces.")

(defvar *position-display* nil
  "A function to call instead of displaying the file position as is.
Called with the file, position and stream.")

(defvar *snippet-display* nil
  "A function to call instead of displaying the snippet as is.
Called with the file, position, snippet, and stream")

(defvar *snippet-lines* 5
  "The number of lines from the snippet to display.")

(defvar *evaluated* nil
  "A list of evaluated strings and the results of their evaluation. Stored in
reverse order of evaluation. Every element has the shape:

(FRAME-NUMBER PACKAGE STRING-EVALUATED EVALUATION-RESULTS CAPTURED-OUTPUT)

where EVALUATION-RESULTS is a cons of the resultant object and its printed
representation.")

(defvar *appropriate-debugger-hook* 'sb-ext:*invoke-debugger-hook*
  "The debugger hook used by SDB. This variable should be set before enabling SDB
mode and should not be changed while SDB mode is active.")

(defvar *inform-on-grab-status* t
  "Controls whether or not SDB informs the user of the result of grabbing the
keyboard.")

(defvar *font-size* :normal
  "The font size for SDB. May be any CLIM font size specifier, e.g. :normal,
:large, etc., or a integer indicating point size.")

(defclass sdb-common-mixin () ()
  (:documentation
   "A mixin class that provides common functionality to other SDB classes. Examples
include setting override redirect on a frames X11 window, or auto-resizing
frames, or grabbing the keyboard from StumpWM."))

(defclass swm-debugger (sdb-common-mixin clim-debugger::clim-debugger)
  ((frame-restart :initform nil :accessor restart-from-stack-frame)))

(defmethod clime:find-frame-type ((frame sdb-common-mixin))
  :override-redirect)

(defmethod clim:run-frame-top-level :around
    ((frame sdb-common-mixin) &key &allow-other-keys)
  (let ((clim:*default-text-style*
          (clim:make-text-style :sans-serif :roman *font-size*)))
    (call-next-method)))

(defmethod clim:default-frame-top-level :before ((frame sdb-common-mixin)
                                                 &key &allow-other-keys)
  ;; This should work without actually using defined commands and using a
  ;; function instead because clim commands are functions and executing one just
  ;; means applying the car of the list to the cdr of the list. 
  (clim:execute-frame-command frame
                              (list 'sdb-resize-frame
                                    (or *debugger-width*
                                        (stumpwm::head-width
                                         (stumpwm::current-head)))
                                    (or *debugger-height*
                                        (stumpwm::head-height
                                         (stumpwm::current-head)))
                                    frame)))

(defmethod clim:default-frame-top-level :around ((frame sdb-common-mixin)
                                                 &key &allow-other-keys)
  ;; We need to grab the keyboard in the frames top level, otherwise all
  ;; keypresses fall through to the focused window in StumpWM. 
  (let* ((tls (clim:frame-top-level-sheet frame))
         (mirror (clim:sheet-direct-mirror tls))
         (window (clim-clx::window mirror))
         (display (xlib:window-display window))
         (grab (xlib:grab-keyboard window)))
    (declare (ignorable grab))
    (when *inform-on-grab-status*
      (stumpwm:message "SDB grab status: ~S" grab))
    (unwind-protect (call-next-method)
      (xlib:ungrab-keyboard display))))

(defun sdb-resize-frame (width height &optional (frame clim:*application-frame*))
  (let ((toplevel (clim:frame-top-level-sheet frame)))
    (when toplevel
      (clim:move-and-resize-sheet toplevel
                                  (car *debugger-coordinates*)
                                  (cdr *debugger-coordinates*)
                                  width
                                  height))))

(clim:define-gesture-name :show-location   :keyboard (#\l :meta))
(clim:define-gesture-name :print-backtrace :keyboard (#\b :meta))

(clim-debugger::define-clim-debugger-command
    (com-show-locations :name "Toggle display of source locations"
                        :keystroke :show-location)
    ()
  (setf *show-source-locations* (cdr *show-source-locations*)))

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

(defmethod clim:redisplay-frame-pane :after ((frame swm-debugger)
                                             (stream clim-debugger::debugger-pane)
                                             &key force-p)
  (declare (ignore force-p))
  (terpri stream)
  (clim:with-text-face (stream :roman)
    (clim:with-text-face (stream :bold)
      (write-string "Evaluation History (most recent first):" stream))
    (fresh-line stream)
    (loop for (frame pkg in out printed) in *evaluated*
          do (write-string "In stack frame " stream)
             (clim:with-text-face (stream :bold)
               (format stream "~D" frame))
             (write-string " and package " stream)
             (clim:with-text-face (stream :bold)
               (format stream "~A:~&" pkg))
             (clim:with-text-family (stream :fix)
               (clim:with-text-face (stream :italic)
                 (write-string in stream))
               (fresh-line stream)
               (write-string printed stream)
               (fresh-line stream)
               (write-string "=> " stream)
               (if out
                   (clim:indenting-output (stream "=> " :move-cursor t)
                     (loop for (obj . string) in out
                           ;; TODO: Make OBJ inspectable
                           do (write-string string stream)
                              (fresh-line stream)))
                   (format stream "; No Values~%")))))
  (terpri stream))

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
                 ;; TODO: This could probably be simplified.
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

(defun call-with-appropriate-debugger-hook
    (continuation hook-as-symbol hook-function)
  "Call CONTINUATION with the appropriate debugger hook let-bound to
HOOK-FUNCTION. HOOK-AS-SYMBOL should be either '*DEBUGGER-HOOK*,
'SB-EXT:*INVOKE-DEBUGGER-HOOK*, or :ALL."
  (case hook-as-symbol
    ((:all)
     (let ((*debugger-hook* hook-function)
           (sb-ext:*invoke-debugger-hook*
             (lambda (c old)
               (if *debugger-hook*
                   nil
                   (funcall hook-function c old)))))
       (funcall continuation)))
    ((sb-ext:*invoke-debugger-hook*)
     (let ((sb-ext:*invoke-debugger-hook* hook-function))
       (funcall continuation)))
    ((*debugger-hook*)
     (let ((*debugger-hook* hook-function))
       (funcall continuation)))
    (otherwise
     (error "Unknown appropriate debugger hook ~A, expected one of ~A ~A or ~A"
            hook-as-symbol '*debugger-hook* 'sb-ext:*invoke-debugger-hook* :all))))

(defmacro with-appropriate-debugger-hook
    ((hook &optional (function nil fpp)) &body body)
  "Evaluate BODY with HOOK let-bound to FUNCTION. If FUNCTION is not provided, the
function DEBUGGER-HOOK-CALL-SDB is used."
  (let ((fn (gensym "CONT")))
    `(flet ((,fn () ,@body))
       (declare (dynamic-extent #',fn))
       (call-with-appropriate-debugger-hook #',fn
                                            ,hook
                                            ,@(if fpp
                                                  (list function)
                                                  `(#'debugger-hook-call-sdb))))))

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
                (with-appropriate-debugger-hook
                    (*appropriate-debugger-hook*
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
                             (when *inform-on-grab-status*
                               (stumpwm:message "Regrab status: ~A" grab)))))))
                  ;; Without this we end up failing to grab the keyboard and
                  ;; keypresses go through to the previously focused window.
                  (xlib:display-finish-output stumpwm:*display*)
                  (clim:run-frame-top-Level debugger-frame))
             (when (> pointer-grabs 0)
               (stumpwm:grab-pointer (stumpwm:current-screen)))
             (let ((restart (clim-debugger::returned-restart debugger-frame))
                   (frame (restart-from-stack-frame debugger-frame)))
               ;; If we invoked a restart, display a message of which restart we
               ;; invoked. Otherwise, we can safely exit to the toplevel. If no
               ;; toplevel restart is found (it should be, cause it is established
               ;; early on) then abort.
               (cond ((and restart frame)
                      (stumpwm:message
                       "Both restart and stack frame provided, aborting")
                      (abort))
                     (restart
                      ;; If RESTART is true, then we have already invoked it.
                      (stumpwm:message "Invoked restart ~A" restart))
                     (t
                      (let ((top-level (find-restart 'stumpwm::top-level)))
                        (if top-level
                            (invoke-restart top-level)
                            (abort)))))))))))))

(defun debugger-hook-call-sdb (c e)
  "Call SWM-DEBUGGER with the condition and encapsulation."
  (funcall (symbol-function 'swm-debugger) c e))

(defun invoke-sdb (c)
  "Explicitly invoke SDB"
  (let ((*evaluated* nil))
    (swm-debugger c (symbol-function 'swm-debugger))))

(defmacro with-sdb (condition-type &body body)
  "Invoke SDB for specific condition types signalled within BODY."
  (let ((block (gensym))
        (dbg (gensym)))
    `(block ,block
       (let ((,dbg sb-ext:*invoke-debugger-hook*))
         (unwind-protect
              (with-appropriate-debugger-hook (:all)
                (when (eql ,dbg 'sb-debug::debugger-disabled-hook)
                  (sb-debug::enable-debugger))
                (restart-case
                    (handler-bind ((,condition-type #'invoke-sdb))
                      ,@body)
                  (quit-sdb (&optional value)
                    :report "Return from WITH-SDB form"
                    (return-from ,block value))))
           (when (eql ,dbg 'sb-debug::debugger-disabled-hook)
             (sb-debug::disable-debugger)))))))

(defun invoke-with-stack-frame-information (frame continuation)
  (assert (typep frame 'clim-debugger::clim-debugger))
  (let* ((debug-pane (clim:find-pane-named frame 'clim-debugger::debugger-pane))
         (active-frame (clim-debugger::active-frame debug-pane))
         (package (or (swank-backend:frame-package active-frame)
                      (find-package *default-eval-package*))))
    (funcall continuation debug-pane active-frame package)))

(defmacro with-stack-frame-information ((pane stack-frame package) frame
                                        &body body)
  (let ((fn (gensym "CONTINUATION")))
    `(flet ((,fn (,pane ,stack-frame ,package) ,@body))
       (declare (dynamic-extent #',fn))
       (invoke-with-stack-frame-information ,frame #',fn))))

(defun read-user-input (pane prompt &key (prompt-in-pane t))
  "Read input from the user. PROMPT should be a string or a function that accepts
the stream to write to. If PROMPT-IN-PANE is T then *STANDARD-INPUT* is rebound
to the pane."
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\,))
            (*standard-output* (if prompt-in-pane pane *standard-output*)))
        (clim:with-text-face (*standard-output* :bold)
          (cond ((functionp prompt)
                 (funcall prompt *standard-output*))
                ((stringp prompt)
                 (write-string prompt *standard-output*))
                (t (format *standard-output* "~A" prompt))))
        (clim:accept 'clim::command-or-form
                     :default-type 'empty-input
                     :prompt " "
                     :prompt-mode :raw))
    (values object type)))

(defun eval-in-stack-frame (object type package pane
                            &key (save-evaluation-results t))
  "Evaluate an object in a stack frame. "
  (let ((active-frame (clim-debugger::active-frame pane)))
    (labels ((printer (values)
               (swank::with-bindings swank::*swank-pprint-bindings*
                 (apply #'values
                        (loop for v in values
                              collect (cons v (princ-to-string v))))))
             (ev (form)
               (let* ((to-ev (format nil "~S" form))
                      (*standard-output* (make-string-output-stream))
                      (values (multiple-value-list
                               (swank::eval-in-frame-aux active-frame
                                                         to-ev
                                                         package
                                                         #'printer))))
                 (when save-evaluation-results
                   (push (list active-frame (package-name package) to-ev values
                               (get-output-stream-string *standard-output*))
                         *evaluated*))
                 values)))
      (cond ((clim:presentation-subtypep type 'empty-input)
             (ev '(values)))
            ((clim:presentation-subtypep type 'clim::command)
             (climi::ensure-complete-command object
                                             (clim:frame-command-table
                                              clim:*application-frame*)
                                             *standard-input*))
            (t (ev object))))))

(defun read-and-eval-in-stack-frame ()
  "Read and evaluate input from the user within the context of a stack frame."
  (with-stack-frame-information (pane frame package) clim:*application-frame*
    (flet ((prompter (stream)
             (format stream "~%Eval in frame ~D (~A)> "
                     frame (package-name package))
             (force-output stream)))
      (declare (dynamic-extent #'prompter))
      (multiple-value-bind (object type)
          (read-user-input pane #'prompter :prompt-in-pane nil)
        (eval-in-stack-frame object type package pane)))))

(clim-debugger::define-clim-debugger-command
    (clim-debugger::com-eval :name "SDB Eval in frame"
                             :keystroke :eval)
    ()
  (cond ((eq (clim:frame-current-layout clim:*application-frame*)
             'clim-debugger::with-interactor)
         (read-and-eval-in-stack-frame)
         (setf (clim:frame-current-layout clim:*application-frame*)
               'clim-debugger::without-interactor))
        (t
         (climi::schedule-command clim:*application-frame*
                                  '(clim-debugger::com-eval)
                                  0)
         (setf (clim:frame-current-layout clim:*application-frame*)
               'clim-debugger::with-interactor))))

(clim-debugger::define-clim-debugger-command
    (clim-debugger::com-invoke-restart :name "Invoke restart")
    ((restart 'clim-debugger::restart :gesture :select))
  (let ((frame clim:*application-frame*))
    (if (eq (clim:frame-current-layout frame) 'clim-debugger::with-interactor)
        (let ((pane (clim:find-pane-named frame 'clim-debugger::interactor)))
          (setf (clim-debugger::returned-restart frame) restart)
          (loop repeat 2 do (terpri pane) finally (force-output pane))
          (invoke-restart-interactively (find-restart restart)))
        (progn
          (climi::schedule-command clim:*application-frame*
                                   '(clim-debugger::com-invoke-restart restart)
                                   0)
          (setf (clim:frame-current-layout frame)
                'clim-debugger::with-interactor)))))

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
    (clim-debugger::com-print-backtrace :name "Print Backtrace"
                                        :menu t
                                        :keystroke :print-backtrace)
    ()
  (cond ((eq (clim:frame-current-layout clim:*application-frame*)
             'clim-debugger::with-interactor)
         (unwind-protect
              (multiple-value-bind (file type)
                  (let ((clim:*command-dispatchers* '(#\,)))
                    (clim:with-text-face (*standard-output* :bold)
                      (terpri *standard-output*)
                      (write-string "Save backtrace file as: " *standard-output*))
                    (write-string "(~/SDB-backtrace.txt)" *standard-output*)
                    (clim:accept 'string :prompt " "
                                         :default "~/SDB-backtrace.txt"
                                         :prompt-mode :raw))
                (declare (ignorable type))
                (with-open-file (f file :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                  (let ((debug-info
                          (clim-debugger::the-condition clim:*application-frame*))
                        (*print-right-margin* *backtrace-right-margin*))
                    (format f "SDB CONDITION INFORMATION:~%~%~2TLiteral Condition: ~S~%~2TPretty Condition:  ~A~&~2TCondition Message: ~A~&~2TCondition Type:    ~S~&~2TCondition Extra:   ~A~%~%SDB BACKTRACE:~%~%"
                            (clim-debugger::the-condition debug-info)
                            (clim-debugger::the-condition debug-info)
                            (clim-debugger::condition-message debug-info)
                            (clim-debugger::type-of-condition debug-info)
                            (clim-debugger::condition-extra debug-info))
                    (loop for frame in (clim-debugger::backtrace debug-info)
                          for numstr = (format nil "[~D]"
                                               (clim-debugger::frame-no frame))
                          for blankstr = (make-string (length numstr)
                                                      :initial-element #\space)
                          do (format f "~&~A ~A~&~A Locals:"
                                     numstr (clim-debugger::frame-string frame) blankstr)
                             (loop for local in (clim-debugger::frame-variables frame)
                                   do (format f "~&~A   ~A -> ~S"
                                              blankstr
                                              (getf local :name)
                                              (getf local :value)))))
                  (finish-output f)))
           (setf (clim:frame-current-layout clim:*application-frame*)
                 'clim-debugger::without-interactor)))
        (t
         (climi::schedule-command clim:*application-frame*
                                  '(clim-debugger::com-print-backtrace)
                                  0)
         (setf (clim:frame-current-layout clim:*application-frame*)
               'clim-debugger::with-interactor))))

(stumpwm:define-minor-mode swm-debugger-mode () ()
  (:scope :unscoped)
  (:interactive sdb-mode)
  (:lighter "SDB"))

(defvar *swm-debugger-mode-control-i-map* (stumpwm:make-sparse-keymap)
  "A keymap hung on <PREFIX C-I>")

(define-swm-debugger-mode-command invoke-sdb-without-condition () ()
  (invoke-sdb (make-condition 'simple-error :format-control "Dummy Error")))

(stumpwm:define-key *swm-debugger-mode-control-i-map* (stumpwm:kbd "d")
  "invoke-sdb-without-condition")

(stumpwm:define-key *swm-debugger-mode-root-map* (stumpwm:kbd "C-i")
  '*swm-debugger-mode-control-i-map*)

;; Allow for hot reloading of this file without adding many versions of the same
;; function. 
(macrolet ((uninstaller (symbol hook)
             `(when (and (fboundp ,symbol)
                         (member (symbol-function ,symbol) ,hook))
                (stumpwm:remove-hook ,hook (symbol-function ,symbol)))))
  (uninstaller 'install-dbg *swm-debugger-mode-enable-hook*)
  (uninstaller 'uninstall-dbg *swm-debugger-mode-disable-hook*))


(let ((debug-hook nil)
      (sb-ext-debug-hook nil)
      (installed nil))
  (defun install-dbg (&rest rest)
    (declare (ignore rest))
    (if installed
        (stumpwm:message "Cannot install debugger: already installed")
        (flet ((fn (c old)
                 (if *debugger-hook*
                     nil
                     (debugger-hook-call-sdb c old))))
          (psetf installed t
                 debug-hook *debugger-hook*
                 sb-ext-debug-hook sb-ext:*invoke-debugger-hook*)
          (if (eq *appropriate-debugger-hook* :all)
              (setf sb-ext:*invoke-debugger-hook* #'fn
                    *debugger-hook* #'debugger-hook-call-sdb)
              (setf (symbol-value *appropriate-debugger-hook*)
                    #'debugger-hook-call-sdb)))))
  (defun uninstall-dbg (&rest rest)
    (declare (ignore rest))
    (if installed
        (psetf installed nil
               *debugger-hook* debug-hook
               sb-ext:*invoke-debugger-hook* sb-ext-debug-hook
               debug-hook nil
               sb-ext-debug-hook nil)
        (stumpwm:message "Cannot uninstall debugger: not installed"))))

(when (not (member 'install-dbg *swm-debugger-mode-enable-hook*))
  (stumpwm:add-hook *swm-debugger-mode-enable-hook* 'install-dbg))
(unless (member 'uninstall-dbg *swm-debugger-mode-disable-hook*)
  (stumpwm:add-hook *swm-debugger-mode-disable-hook* 'uninstall-dbg))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Mark that we have compiled and loaded SDB at least once.
  (setf *sdb-loaded-p* t))
