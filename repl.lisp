;;;; repl.lisp

(in-package :sbcl-xrepl)

(defvar *noprint* nil
  "boolean: T if don't print prompt and output")


(defun get-prompt ()
  (cond (*noprint* nil)
        ((functionp *prompt*) (funcall *prompt*))
        (t *prompt*)))

(defun get-prompt-cont ()
  (cond (*noprint* nil)
        ((functionp *prompt-cont*) (funcall *prompt-cont*))
        ((eq t *prompt-cont*) (get-prompt))
        (t *prompt-cont*)))


(defun read-forms ()
  ;; KLUDGE: *READ-SUPPRESS* makes the REPL useless, and cannot be
  ;; recovered from -- flip it here.
  (when *read-suppress*
    (warn "Setting *READ-SUPPRESS* to NIL to restore toplevel usability.")
    (setf *read-suppress* nil))
  (if (use-readline-p)
      (rl-read-forms)
      (list (sb-impl::repl-read-form-fun *standard-input* *standard-output*))))

(defun rep-one ()
  (sb-impl::scrub-control-stack)
  (sb-thread::get-foreground)
  (unless *noprint*
    (sb-impl::flush-standard-output-streams)
    (fresh-line))
  (let* ((forms (read-forms))
         (results (mapcar (lambda (form)
                            (multiple-value-list (sb-impl::interactive-eval form)))
                          forms)))
    (unless *noprint* (funcall *print-results-fun* results))))

(defun repl ()
  (read-history)
  (loop
    (unwind-protect (rep-one)
      (sb-impl::disable-stepping))))

(defun make-repl-fun ()
  (copy-lambda (*prompt*
                *prompt-cont*
                *print-results-fun*
                *history-file*
                *history-modified*)
               (noprint)
    (let ((*noprint* noprint)) (repl))))


;; (setq sb-int:*repl-prompt-fun*    #'repl-prompt-fun)
;; (setq sb-int:*repl-read-form-fun* #'repl-read-form-fun)

(setq sb-impl::*repl-fun-generator* #'make-repl-fun)
