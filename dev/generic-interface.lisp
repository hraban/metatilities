(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; System variables
;;; ---------------------------------------------------------------------------

(declaim (special *development-mode* *use-native-debugger*))

(defvar *development-mode* t)

(defvar *use-native-debugger* nil)


;;; ---------------------------------------------------------------------------
;;; progress bars
;;; ---------------------------------------------------------------------------

(defgeneric make-progress-bar (interface min max title &key)
  )

;;; ---------------------------------------------------------------------------

(defgeneric progress-bar-value (interface bar)
  )

;;; ---------------------------------------------------------------------------

(defgeneric (setf progress-bar-value) (value interface bar)
  )

;;; ---------------------------------------------------------------------------

(defgeneric close-progress-bar (interface bar) 
  )

;;; ---------------------------------------------------------------------------

(defvar *dummy-progress-variable*)
(defvar *progress-bar-count* 0)

;;; ---------------------------------------------------------------------------

(defmacro with-progress-bar ((min max &rest args
                                  &key (title "Progress")
                                  (verbose? t)
                                  (desired-interface nil interface-supplied?)
                                  (determinate-p t) &allow-other-keys)
                             &body body)
  (remf args :title)
  (remf args :desired-interface)
  (remf args :verbose?)
  (with-variables (bar window interface)
    `(let ((,interface (if ,interface-supplied?
                         ,desired-interface
                         *default-interface*)))
       (if (and ,verbose? (is-interface-available-p ,interface))
         (macrolet ((progress ()
                      ;; avoid compiler warnings
                      `(progress-bar-value ,',interface ,',bar)))
           (multiple-value-bind (,window ,bar) 
                                (make-progress-bar
                                 ,interface ,min ,max ,title
                                 :determinate-p ,determinate-p 
                                 ,@args)
             (declare (ignorable ,bar))
             (unwind-protect
               (progn (incf *progress-bar-count*)
                      (setf (progress-bar-value ,interface ,bar) ,min)
                      ,@body)
               ;; cleanup
               (close-progress-bar ,interface ,window)
               (decf *progress-bar-count*))))
         ;; just execute body
         (macrolet ((progress ()
                      '*dummy-progress-variable*))
           (let ((*dummy-progress-variable* 0))               
             (progn ,@body)))))))

;;; ---------------------------------------------------------------------------

(defmacro with-process-message ((&rest args &key (title "Please wait...") 
                                       &allow-other-keys)
                                &body body)
  (remf args :title)
  `(with-progress-bar (0 0 :title ,title :determinate-p nil ,@args)
     ,@body))


;;; ---------------------------------------------------------------------------
;;; Error handling
;;; ---------------------------------------------------------------------------

(defmacro handle-errors (standard-message &body body)
  `(catch 'recover
     (if *use-native-debugger*
       (progn . ,body)
       ;; Otherwise set up our nice error handlers...
       (handler-bind ((error #'(lambda (condition)
                                 (gui-error condition "ERROR: " ,standard-message)
                                 (throw 'recover NIL)))
                      #+MCL
                      (warning #'(lambda (condition) (gui-error condition "WARNING: ")
                                   (muffle-warning condition))))
         ,@body))))

;;; ---------------------------------------------------------------------------

(defgeneric gui-error (condition &optional prefix standard-message)
  )

;;; ---------------------------------------------------------------------------

(defgeneric gui-warn (string &rest args)
  )


;;; ---------------------------------------------------------------------------
;;; beeping
;;; ---------------------------------------------------------------------------

(defmethod interface-beep* (interface &rest args)
  (declare (ignore interface args)))

;;; ---------------------------------------------------------------------------

(defmethod interface-beep (&rest args)
  (apply #'interface-beep* *default-interface* args))
         

;;; ---------------------------------------------------------------------------
;;; y-or-n-question
;;; ---------------------------------------------------------------------------

(defgeneric y-or-n-question* (interface message &rest args)
  (:documentation "Asks a yes or no question spiffily."))


;;; ---------------------------------------------------------------------------
;;; no interface interface implementations
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Progress bars

(defmethod make-progress-bar (interface min max title &key &allow-other-keys)
  (declare (ignore interface min max title))
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod progress-bar-value (interface bar)
  (declare (ignore interface bar))
  (values 0))

;;; ---------------------------------------------------------------------------

(defmethod (setf progress-bar-value) (value interface bar)
  (declare (ignore interface bar))
  (values value))

;;; ---------------------------------------------------------------------------

(defmethod close-progress-bar (interface bar)
  (declare (ignore interface bar))
  (values))

;;; ---------------------------------------------------------------------------

#+Test
(with-progress-bar (0 20)
  (loop repeat 20 do (incf (progress)) (sleep .1)))

;;; ---------------------------------------------------------------------------
;;; Errors and warnings

#-(or DIGITOOL OPENMCL)
(defmethod report-condition ((condition condition) stream)
  (write-string 
   (cond ((typep condition 'error) "Error")
	 ((typep condition 'warning) "Warning")
	 (t "Unknown Condition"))
   stream)
  (print-object condition stream))

(defmethod gui-error* (interface condition &optional (prefix "") (standard-message nil)) 
  (gui-warn* 
   interface
   (if *development-mode*
     (with-output-to-string (stream)
       (let ((*print-array* nil)
             (*print-length* 10)
             (*print-level* 4))
         (#+(or DIGITOOL OPENMCL) ccl::report-condition 
	  #-(or DIGITOOL OPENMCL) report-condition condition stream)))
     (format nil "~A~A" prefix 
             (if standard-message 
               standard-message
               condition)))))

;;; ---------------------------------------------------------------------------

(defmethod gui-error (condition &optional (prefix "") (standard-message nil))
  (gui-error* *default-interface* condition prefix standard-message))

;;; ---------------------------------------------------------------------------

(defmethod gui-warn* (interface string &rest args)
  (declare (ignore interface))
  (apply #'warn string args))

;;; ---------------------------------------------------------------------------

(defmethod gui-warn (string &rest args)
  (apply #'gui-warn* *default-interface* string args))


;;; ---------------------------------------------------------------------------
;;; Color
;;; ---------------------------------------------------------------------------

(defmethod make-color** (interface red green blue)
  (declare (ignore interface red green blue))
  (values 0))

;;; ---------------------------------------------------------------------------

(defmethod make-color* (red green blue)
  ;;; Make-color with sensible arguments
  "given red, green, and blue, returns an encoded rgb value"
  (make-color** (default-interface) red green blue))

;;; ---------------------------------------------------------------------------

(defmethod make-gray* (interface level)
  (make-color** interface level level level))

;;; ---------------------------------------------------------------------------

(defmethod make-gray (level)
  ;;; These use a 0-255 scale for component levels
  (make-gray* (default-interface) level))

;;; ---------------------------------------------------------------------------

(defmethod make-scaled-color* (interface red green blue scale)
  (make-color** interface
                (round (* red scale))
                (round (* green scale))
                (round (* blue scale))))

;;; ---------------------------------------------------------------------------

(defmethod make-scaled-color (red green blue scale)
  (make-scaled-color* (default-interface) red green blue scale))

  
;;; ---------------------------------------------------------------------------
;;; y-or-n-dialog
;;; ---------------------------------------------------------------------------

(defmethod y-or-n-question* (interface message &rest args)
  (declare (ignore interface args))
  (y-or-n-p message))

;;; ---------------------------------------------------------------------------

(defun y-or-n-question (message &rest args)
  (apply #'y-or-n-question* *default-interface* message args))


;;; ---------------------------------------------------------------------------
;;; choose-file-question
;;; ---------------------------------------------------------------------------

(defmethod choose-file-question* (interface &rest args)
  (declare (ignore interface args))
  (print "I would love to choose a file for you, but I'm not sure how?"))

;;; ---------------------------------------------------------------------------

(defun choose-file-question (&rest args)
  (apply #'choose-file-question* *default-interface* args))


;;; ---------------------------------------------------------------------------
;;; choose-new-file-question
;;; ---------------------------------------------------------------------------

(defmethod choose-new-file-question* (interface &rest args)
  (declare (ignore interface args))
  (print "I would love to choose a new file name for you, but I'm not sure how?"))

;;; ---------------------------------------------------------------------------

(defun choose-new-file-question (&rest args)
  (apply #'choose-new-file-question* *default-interface* args))


;;; ---------------------------------------------------------------------------
;;; choose-directory-question 
;;; ---------------------------------------------------------------------------

(defmethod choose-directory-question* (interface &rest args)
  (declare (ignore interface args))
  (print "I would love to choose a directory name for you, but I'm not sure how?"))

;;; ---------------------------------------------------------------------------

(defun choose-directory-question (&rest args)
  (apply #'choose-directory-question* *default-interface* args))


;;; ---------------------------------------------------------------------------
;;; choose-item-question
;;; ---------------------------------------------------------------------------

(defmethod choose-item-question* (interface list &rest args &key &allow-other-keys)
  (declare (ignore interface list args))
  (print "I would love to choose an item for you, but I'm not sure how?"))

;;; ---------------------------------------------------------------------------

(defun choose-item-question (list &rest args &key &allow-other-keys)
  (apply #'choose-item-question* *default-interface* list args))

;;; ---------------------------------------------------------------------------
;;; choose-item-from-pup
;;; ---------------------------------------------------------------------------

;; defaults to choose-item-question:
(defmethod choose-item-from-pup* (interface the-list &rest args &key &allow-other-keys)
  (apply #'choose-item-question* interface the-list args))

;;; ---------------------------------------------------------------------------

(defun choose-item-from-pup (the-list &rest args &key &allow-other-keys)
  "Present an interface to allow a choice from a list. Can throw :cancel." 
  (apply #'choose-item-from-pup* *default-interface* the-list args))

;;; ---------------------------------------------------------------------------

(defun choose-item-from-pup-no-singletons (the-list-or-atom &rest args 
                                                            &key &allow-other-keys)
  "Like choose-item-from-pup, but just returns the datum if it 
is an atom or a singleton list."
  (cond ((atom the-list-or-atom) (values the-list-or-atom))
        ((= (length the-list-or-atom) 1) (values (first the-list-or-atom)))
        (t (apply #'choose-item-from-pup the-list-or-atom args))))

;;; ---------------------------------------------------------------------------
;;; make-ui-point
;;; ---------------------------------------------------------------------------

(defmethod make-ui-point* (interface x y)
  (declare (ignore interface x y))
  (values))

;;; ---------------------------------------------------------------------------

(defun make-ui-point (x y)
  (make-ui-point* *default-interface* x y))


;;; ---------------------------------------------------------------------------
;;; process-parameters
;;; ---------------------------------------------------------------------------

(defmethod process-parameters* (interface &rest args)
  (declare (ignore interface args))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod process-parameters (&rest args)
  (apply #'process-parameters* *default-interface* args)
  (values))


;;; ---------------------------------------------------------------------------
;;; put-item-on-clipboard
;;; ---------------------------------------------------------------------------

(defmethod put-item-on-clipboard* (interface thing)
  (declare (ignore interface thing))
  (error "I don't know anything about clipboards."))

;;; ---------------------------------------------------------------------------

(defmethod put-item-on-clipboard (thing)
  (put-item-on-clipboard* *default-interface* thing)
  thing)

;;; ---------------------------------------------------------------------------
;;; inspect-thing
;;; ---------------------------------------------------------------------------

(defmethod inspect-thing* (interface thing &rest args)
  (declare (ignore interface args))
  (error "I don't know how toinspect ~S" thing))

;;; ---------------------------------------------------------------------------

(defmethod inspect-thing (thing &rest args)
  (apply #'inspect-thing* *default-interface* thing args)
  (values thing))

;;; ---------------------------------------------------------------------------

(defun inspect-things (&rest things)
  (let ((result nil))
    (mapc (lambda (thing)
            (setf result (inspect-thing thing)))
          things)
    (values result)))

;;; ---------------------------------------------------------------------------

(defmethod sound-note* (interface pitch velocity &rest args)
  (declare (ignore interface pitch velocity args))
  (interface-beep))

;;; ---------------------------------------------------------------------------

(defmethod sound-note (pitch velocity &rest args)
  (apply #'sound-note* *default-interface* pitch velocity args))

;;; ---------------------------------------------------------------------------

(defmethod stop-notes* (interface)
  (declare (ignore interface))
  (error "I don't know how to stop music."))

(defun stop-notes ()
  (stop-notes* *default-interface*))         

;;; ---------------------------------------------------------------------------

(defmethod select-instrument* (interface instrument &rest args)
  (declare (ignore interface instrument args))
  (error "I don't know how to select instruments."))

;;; ---------------------------------------------------------------------------
  
(defmethod select-instrument (instrument &rest args)
  (apply #'select-instrument* *default-interface* instrument args))


;;; ---------------------------------------------------------------------------
;;; query-user-for-string
;;; ---------------------------------------------------------------------------

(defun query-user-for-string (prompt &rest args &key &allow-other-keys)
  (apply #'prompt-for 'string prompt args))

;;; ---------------------------------------------------------------------------

(defun query-user-for-integer (prompt &optional minimum maximum)
  (catch :cancel
    (loop do
          (let ((number (parse-integer (query-user-for-string prompt)
                                       :junk-allowed t)))
            (cond ((null number)
                   )
                  ((and minimum (< number minimum))
                   )
                  ((and maximum (> number maximum))
                   )
                  (t
                   (return-from query-user-for-integer number)))))))

;;; ---------------------------------------------------------------------------
;;; prompt-for
;;; ---------------------------------------------------------------------------

(defmethod prompt-for* (interface type message &rest args)
  (declare (ignore interface message args))
  (warn "I don't know how to prompt for ~A" type))

;;; ---------------------------------------------------------------------------

(defmethod prompt-for* (interface (type (eql 'string)) message &rest args)
  (declare (ignore interface))
  (apply #'format *query-io* message args)
  (finish-output *query-io*)
  (read-line *query-io* nil :eof))

;;; ---------------------------------------------------------------------------

(defmethod prompt-for* (interface (type (eql 'fixnum)) message &rest args)
  (declare (ignore interface))
  (apply #'query-user-for-integer message args))

;;; ---------------------------------------------------------------------------

(defmethod prompt-for (type message &rest args)
  (apply #'prompt-for* *default-interface* type message args))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
