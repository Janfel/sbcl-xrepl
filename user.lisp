;;;; user.lisp

(in-package :sbcl-xrepl)

(defun default-prompt ()
  (format nil "~a> " (prompt-package-name)))

(defun default-prompt-cont ()
  (format nil "~a> " (make-string (length (prompt-package-name))
                                  :initial-element #\Space)))

(defun default-print-results (results)
  (dolist (result results)
    (format t "~&~{~s~^ ~}" result)))

(defvar *prompt*            #'default-prompt)
(defvar *prompt-cont*       #'default-prompt-cont)
(defvar *print-results-fun* #'default-print-results)
(defvar *history-file* (uiop:xdg-data-home #P"common-lisp/sbcl-history")
  "The file where sbcl-xrepl stores history data.")
(defvar *special-prefix* ":" "The prefix string for special commands.")
(defvar *special-commands*
  '((:!   special-shell-command :string)
    (:c   cl:class-of           :eval)
    (:cd  special-cd            :string)
    (:d   cl:describe           :lisp)
    (:dis cl:disassemble        :lisp "Disassemble the compiled code associated with OBJECT.")
    (:h   special-help          :lisp)
    (:i   cl:inspect            :lisp "Interactively give information about OBJECT.")
    (:ld  cl:load               :file "Load the file given by FILESPEC into the Lisp environment.")
    (:mx  cl:macroexpand        :lisp)
    (:mx1 cl:macroexpand-1      :lisp)
    (:pwd special-pwd           :lisp)
    (:t   cl:type-of            :eval)
    (:q   sb-ext:exit           :lisp "Terminate the REPL with optional exit code CODE."))
  "A list of SPEC objects specifying the accepted special commands.
Each SPEC object is a list (ID FUNC PARSING &OPTIONAL DOC) where:
ID is a keyword naming the special command.
FUNC is a symbol naming the function that is invoked.
DOC is a one-line string describing what ID does.
DOC defaults to (CL:DOCUMENTATION FUNC 'FUNCTION).
PARSING is a symbol specifying how the rest of the command line is interpreted
and passed to FUNC. PARSING can be one of the following:
  :LISP   As a list of unevaluated expressions.
  :EVAL   As a list of evaluated expressions.
  :STRING As string.
  :PATH   As path object.
  :FILE   As path object pointing to an existing file.")

(defun special-command-argument-forms (argstring parsing)
  "Parse ARGSTRING according to PARSING. See SBCL-XREPL:*SPECIAL-COMMANDS*."
  (case parsing
    (:lisp    (mapcar (lambda (x) `(quote ,x)) (rl-read-forms argstring)))
    (:eval    (rl-read-forms argstring))
    (:string  (list argstring))
    (:path   `((uiop:parse-native-namestring (uiop:native-namestring ,argstring))))
    (:file   `((truename (uiop:parse-native-namestring (uiop:native-namestring ,argstring)))))
    (t       `((error "Undefined parsing mode: ~a~%" ,parsing)))))

(defun special-help (&optional command)
  "Print a help message or a description of special command COMMAND."
  (if command
      (let ((spec (assoc (uiop:find-symbol* command :keyword nil) *special-commands* :test #'eq)))
        (if spec
            (destructuring-bind (keyword func parsing &rest nil) spec
              (format t "~
The special command ~s is bound to ~s.
The commandline is parsed according to ~s.
Documentation of ~s:~%~a"
                      keyword func parsing func (documentation func 'function)))
            (format t "The special command ~s is not bound.~%" command)))
      (progn
        (write-line "Builtin special commands:") (terpri)
        (loop for (keyword func parsing docstring) in *special-commands*
              do (format t "~s~8T~@[~a ~](~s~@[ ~s~])~%"
                         keyword (or docstring (documentation func 'function)) func parsing)
              finally (terpri))))
  (values))

(defun special-pwd ()
  "Print the current working directory."
  (format t "~a~%" (namestring cl:*default-pathname-defaults*))
  (values))

(defun special-cd (directory)
  "Move to DIRECTORY and print it."
  (when (or (null directory) (string= directory "") (string= directory "~"))
    (setq directory (user-homedir-pathname)))
  (setq cl:*default-pathname-defaults* (truename directory))
  (special-pwd))

(defun special-shell-command (shell-command)
  "Run SHELL-COMMAND with /bin/sh."
  (sb-ext:run-program #P"/bin/sh" (list "-c" shell-command) :input t :output t))
