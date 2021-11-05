;;; Locally cached FASLs
(let ((cache-dir (merge-pathnames ".common-lisp/" *default-pathname-defaults*)))
  (asdf:initialize-output-translations
    `(:output-translations (t (,cache-dir :implementation))
      :disable-cache
      :ignore-inherited-configuration)))


;;; Quicklisp (installed in the current directory)
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp" *default-pathname-defaults*)))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (progn
        (load (merge-pathnames "quicklisp.lisp" *load-truename*))
        (funcall (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART"))
                 :path (merge-pathnames ".quicklisp/" *default-pathname-defaults*)))))


;;; Nicer prompt
(defvar *last-package* nil)
(defvar *cached-prompt* nil)
(defun package-prompt (stream)
  (when (not (eq *last-package* *package*))
    (setf *cached-prompt*
          (format nil "~%[SBCL] ~A "
                  (or (first (package-nicknames *package*))
                      (package-name *package*))))
    (setf *last-package* *package*))
  (terpri)
  (princ *cached-prompt* stream))

(setf sb-int:*repl-prompt-fun* #'package-prompt)


;;; Scratch Marker
(defun sharp-semicolon-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (loop :while (read-line stream nil nil))
  (values))
(set-dispatch-macro-character #\# #\; #'sharp-semicolon-reader)
