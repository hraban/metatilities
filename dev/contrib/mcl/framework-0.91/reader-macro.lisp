;;;-*- Mode: Lisp; Package: MACH -*-
;;;
;;;
;;; File:	trap-reader.lisp
;;; Author:	Takehiko Abe <keke@gol.com>
;;; Date:	2004-01-30

;;; defines #~ reader macro that works like #_ but for macho functions.
;;; It reads from MCL's interface files.
;;;
;;; Limitations:
;;;
;;; It only works for deftrap-inline form
;;; It only searches entry-points in ApplicationServices.framework.
;;;

(in-package :mach)

;;;
;;; #~ reader macro
;;;

;; from ccl:deftrap-inline
(defmacro deftrap-macho (name args &optional return)
  (when (and return (> (ccl::record-field-length return) 4))
    (error "Sorry. ~a is too big" return))
  (mapc #'(lambda (arg)
            (find-arg-mactype (cadr arg)))
        args)
  (let ((sym (intern (string-upcase name) ccl::*traps-package*)))
    (loop for argspec in args
          do 
          (when (memq (car argspec) ccl::*variable-names-macro-arglists-shouldnt-use*)
            (setf (car argspec) (ccl::bad-variable-name-replacement (car argspec)))))
    (let ((env-var (gensym)))
      `(defmacro ,sym (&environment ,env-var ,@(mapcar 'car args))
         (expand-macho-trap ',sym (list ,@(mapcar 'car args)) ',args ',return nil ,name
                            nil ,env-var)))))


;; from ccl::find-interface-entry
(defun find-interface-entry (symbol file stream &optional (errorp t))
  (let* ((search-symbol (find-symbol (format nil "_~A" symbol)))
         (new-symbol-p (not search-symbol)))
    (unless search-symbol
      (setq search-symbol (intern (format nil "_~A" symbol))))
    (unwind-protect
      (multiple-value-bind (pos start-pos) (and stream (ccl::search-index-stream stream search-symbol))
        (declare (ignore start-pos))
        (unless pos
          (when errorp
            (cerror "Ignore the error."
                    "Can't find ~s in ~s.~%Consider (~s)~:[~; or look in \"ccl:library;interfaces.lisp\"~]"
                    symbol file 'reindex-interfaces (eq file ccl::*traps-index-file*)))
          (return-from find-interface-entry nil))
        (let (path position)
          (let ((*package* ccl::*traps-package*)
                (*read-base* 10.))
            (setq path (ccl::traps-file-pathname stream)
                  position (read stream)))
          (with-open-file (s path :element-type 'base-character)
            (file-position s position)
            (let ((form (let ((*package* ccl::*traps-package*)
                              (*read-base* 10.))
                          (ignore-errors (read s)))))
              (if (and form
                       (listp form)
                       (memq (car form) '(deftrap defctbtrap deftrap-inline))
                       ;; FIXIT
                       (let ((name (cadr form)))
                         (and name
                              (string-equal search-symbol
                                            (if (listp name)
                                              (car name)
                                              name)))))
                (unless (eq (car form) 'deftrap-inline)
                  (error "Unspported interface definition found. ~%~
                          #~~XXX reader macro does not support ~A form." (car form)))
                (error "Unexpected interface files entry:~%      ~s~%~
                        Consider (~s)"
                       form 'reindex-interfaces))
              (let ((*loading-file-source-file* path))
                (let* ((name (ccl::remove-leading-underbar (cadr form)))
                       (new-form `(deftrap-macho ,name ,(third form) ,(fourth form))))
                  (eval new-form)))))))
      (when new-symbol-p
        (unintern search-symbol)))))

(defun load-mach-trap (symbol)
  (let* ((file ccl::*traps-index-file*)
         (stream (ccl::open-index-file file 'ccl::*traps-index-stream*)))
    (find-interface-entry symbol file stream)))


(set-dispatch-macro-character #\# #\~
                              #'(lambda (stream char n)
                                  (declare (ignore char n))
                                  (load-mach-trap (read stream t nil t))))

;; from ccl:require-trap
(defmacro require-trap (trap-name &rest rest)
  (setq trap-name (require-type trap-name 'symbol))
  (load-mach-trap trap-name)
  `(,trap-name ,@rest))



;;;
;;; redefine ed-current-sexp to support meta-. for #~.
;;;

(let ((*warn-if-redefine* nil)
      #+ignore
      (*warn-if-redefine-kernel* nil))

(defmethod ed-current-sexp ((w fred-mixin) &optional pos dont-skip &aux
                            (buf (fred-buffer w)))
  (multiple-value-bind (b e) (ccl::frec-get-sel (slot-value w 'frec))
    (declare (fixnum b e))
    (if (or (neq b e)
            (setq e t b (buffer-current-sexp-start buf pos)))
      (let* ((pkg (window-package w))
             (*package* (if (null pkg) *package* (ccl::pkg-arg pkg)))
             (size (buffer-size buf)))
        (when (and (or (eql #\# (buffer-char buf b))
                       (and (eql #\' (buffer-char buf b))
                            (< (1+ b) size)
                            (eql #\# (buffer-char buf (1+ b)))
                            (incf b)))
                   (< (1+ b) size)
                   ;; keke
                   (let ((char (buffer-char buf (1+ b))))
                     (or (ccl::%str-member char "_$")
                         (eql char #\~))))
          (multiple-value-bind (symbol end-pos)
                               (ccl::sexp-ignore-warnings-and-errors
                                 (ccl::buffer-read buf b e))
            (if symbol
              (return-from ed-current-sexp (values symbol end-pos))
              (incf b))))
        (when (not dont-skip) (setq b (ccl::buffer-skip-fwd-reader-macros buf b)))
        ; skip from edit-definition, get-doc,
        ; dont from read and and macroexpand 
        (ccl::sexp-ignore-warnings-and-errors
         (ccl::buffer-read buf b e)))
      (values nil nil))))
)


