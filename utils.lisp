;;;; utils.lisp

(in-package :sbcl-xrepl)

(defun prompt-package-name ()
  (car (sort (cons (package-name cl:*package*)
                   (copy-seq (package-nicknames cl:*package*)))
             #'< :key #'length)))

(defun novelty-check (this-line last-line)
  (let ((trim-chars '(#\Space #\Tab #\Newline)))
    (string/= (string-trim trim-chars this-line)
              (string-trim trim-chars last-line))))

(defun string-common-prefix (s1 s2)
  (let ((len (min (length s1) (length s2))))
    (loop
      (when (string= s1 s2 :end1 len :end2 len)
        (return (subseq s1 0 len)))
      (setq len (1- len)))))

(defmacro copy-lambda ((&rest vars) arglist &body forms)
  ;; Are the GENSYMs really necessary?
  ;; I just copied this from sb-aclrepl.
  (let ((gvars (mapcar (lambda (var) (gensym (symbol-name var))) vars)))
    `(let (,@(mapcar #'list gvars vars))
       (lambda ,arglist
         (let (,@(mapcar #'list vars gvars))
           (unwind-protect (progn ,@forms)
             ,@(mapcar (lambda (gvar var) `(setq ,gvar ,var)) gvars vars)))))))
