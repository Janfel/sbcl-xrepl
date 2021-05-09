(defpackage :sbcl-xrepl
  (:use #:common-lisp)
  (:export
   #:default-prompt
   #:default-prompt-cont
   #:default-print-results
   #:*prompt*
   #:*prompt-cont*
   #:*print-results-fun*
   #:*history-file*
   #:*special-prefix*
   #:*special-commands*))
