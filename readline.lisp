;;;; readline.lisp

(in-package :sbcl-xrepl)

(setq rl:*basic-quote-characters* "\"|")
(setq rl:*basic-word-break-characters*
      (coerce '(#\Space #\Tab #\Newline #\( #\) #\' #\` #\,) 'string))
(setq rl:*readline-name* "SBCL")

(defvar *history-modified* nil)

;; Implement defuns like `form-quoted-p' to complete only variables/functions if appropriate.
;; Implement completion for special commands.
(defun completion-fun (prefix start-pos end-pos)
  (declare (ignorable start-pos end-pos))
  (setq rl:*completion-append-character* #\Space)
  (let ((parts (uiop:split-string (string-upcase prefix) :separator ":"))
        (completions nil))
    (case (length parts)
      ;; PREFIX is empty.
      (0 nil)
      ;; PREFIX is not namespaced.
      (1
       (dolist (pkg (list-all-packages))
         (dolist (name (cons (package-name pkg) (package-nicknames pkg)))
           (when (uiop:string-prefix-p (first parts) name)
             (push (concatenate 'string name ":") completions))))
       (do-symbols (s)
         (let ((name (symbol-name s)))
           (when (uiop:string-prefix-p (first parts) name)
             (push name completions)))))
      ;; PREFIX refers to exported symbol.
      (2
       (if (or (string= (first parts) "")
               (string-equal (first parts) "keyword"))
           (do-symbols (s :keyword)
             (let ((name (symbol-name s)))
               (when (uiop:string-prefix-p (second parts) name)
                 (push (concatenate 'string ":" name) completions))))
           (let ((pkg (find-package (intern (first parts)))))
             (when pkg
               (do-external-symbols (s pkg)
                 (let ((name (symbol-name s)))
                   (when (uiop:string-prefix-p (second parts) name)
                     (push (concatenate 'string (first parts) ":" name) completions))))))))
      ;; PREFIX refers to unexported symbol.
      (3
       (cond ((string/= (second parts) "") nil)
             ((string= (first parts) "")
              (do-symbols (s :keyword)
                (let ((name (symbol-name s)))
                  (when (uiop:string-prefix-p (second parts) name)
                    (push (concatenate 'string "::" name)
                          completions)))))
             (t
              (let ((pkg (find-package (make-symbol (first parts)))))
                (when pkg
                  (do-symbols (s pkg)
                    (let ((name (symbol-name s)))
                      (when (uiop:string-prefix-p (third parts) name)
                        (push (concatenate 'string (first parts) "::" name) completions)))))))))
      ;; PREFIX is an invalid symbol.
      (t nil))
    (let ((com-prefix (reduce (lambda (&optional s1 s2)
                                (cond ((null s1) "")
                                      ((null s2) s1)
                                      (t (string-common-prefix s1 s2))))
                              completions)))
      (when (uiop:string-suffix-p com-prefix ":")
        ;; Don’t append a space when completing a namespace.
        (setq rl:*completion-append-character* #\Null))
      (cons (string-downcase com-prefix)
            (mapcar #'string-downcase completions)))))

(defun rl-read-forms (&optional first-line)
  (setq *history-modified* t)
  (flet ((read-one-line (prompt)
           (or (rl:readline :prompt prompt :add-history t :novelty-check #'novelty-check)
               (sb-ext:exit))))
    (let ((forms nil)
          (eof-marker   '#:EOF)
          (error-marker '#:ERROR)
          (result nil)
          (index 0)
          (line (or first-line (read-one-line (get-prompt)))))
      (when (and (not first-line) (uiop:string-prefix-p *special-prefix* line))
        (setq line (replace line ":" :end1 (length *special-prefix*)))
        (multiple-value-setq (result index) (read-from-string line))
        (let* ((command-spec (assoc result *special-commands* :test #'eq))
               (func (and command-spec (symbol-function (second command-spec))))
               (parsing (third command-spec)))
          (return-from rl-read-forms
            (if command-spec
                `((funcall ,func ,@(special-command-argument-forms (subseq line index) parsing)))
                `((progn (warn "Undefined special command: ~a~%" command) (values)))))))
      (loop
        (handler-case
            (multiple-value-setq (result index)
              (read-from-string line nil eof-marker :start index))
          (end-of-file () (setq result error-marker)))
        (cond
          ((eq result eof-marker) (return))
          ((eq result error-marker)
           ;; We reached the end of the line, but we are inside an incomplete object.
           (setq line (uiop:strcat line #\Newline (read-one-line (get-prompt-cont)))))
          (t (push result forms))))
      (nreverse forms))))

(defun use-readline-p ()
  (interactive-stream-p *standard-input*))

#+nil
(defun write-history ()
  (rl:write-history (namestring *history-file*)))

#+nil
(defun write-history ()
  (uiop:with-temporary-file (:pathname tempfile)
    (rl:write-history (namestring tempfile))
    (let ((hist-line "") (file-line ""))
      (with-open-file (history tempfile)
        (with-open-file (hist-file *history-file* :if-does-not-exist :create)
          (setq hist-line (read-line history nil))
          (setq file-line (read-line hist-file nil))
          (loop while (and hist-line file-line (string/= hist-line file-line))
                do (setq hist-line (read-line history nil)))
          (loop while (and hist-line file-line (string= hist-line file-line))
                do (setq hist-line (read-line history nil))
                   (setq file-line (read-line hist-file nil))))
        (when hist-line
          (with-open-file (hist-file *history-file*
                                     :direction :output
                                     :if-exists :append)
            (loop while hist-line
                  do (write-line hist-line hist-file)
                     (setq hist-line (read-line history nil)))))))))

(defun write-history ()
  (uiop:with-temporary-file (:pathname repl-history-file-path)
    (rl:write-history (namestring repl-history-file-path))
    (with-open-file (repl-history-file repl-history-file-path)
      (let ((repl-line  (read-line repl-history-file  nil)))
        (with-open-file (saved-history-file *history-file* :if-does-not-exist :create)
          (loop for saved-line = (read-line saved-history-file nil)
                while (and  repl-line saved-line)
                if (string= repl-line saved-line)
                do (setf repl-line (read-line repl-history-file nil))))
        (when repl-line
          ;; The file pointer of saved-history-file is now at EOF.
          ;; We append all history entries we have not found in *history*.
          (with-open-file (saved-history-file *history-file* :direction :output :if-exists :append)
            (loop do (write-line repl-line saved-history-file)
                  while (setf repl-line (read-line repl-history-file nil)))))))))

(defun read-history ()
  (when *history-modified* (write-history))
  (rl:clear-history)
  (rl:read-history (namestring *history-file*))
  (setq *history-modified* nil))

(rl:register-function :complete #'completion-fun)
(pushnew #'write-history sb-ext:*exit-hooks*)
