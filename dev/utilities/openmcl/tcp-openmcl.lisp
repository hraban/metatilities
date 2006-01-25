(in-package metatilities)

;;; ---------------------------------------------------------------------------
;;; TCP Stream Implementation for MCL
;;;
;;;    Some of this code may be useful elsewhere (read-until-no-hang)
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------

#|
;;; ---------------------------------------------------------------------------

(defun close-socket (socket)
  (common-lisp::close socket))

(defun socket-read (socket &optional (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)
  (let ((s (read-until-no-hang socket #\Linefeed EOF-ERROR-P EOF-VALUE RECURSIVE-P)))
    (when (and s (not (string-equal s "")))
      (read-from-string s))))

;; what is the difference between this and the above socket-read?
(defun socket-read-line (socket &optional (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)
  (read-from-string
   (read-until-no-hang socket #\Linefeed EOF-ERROR-P EOF-VALUE RECURSIVE-P)))

(defun read-until (stream &optional (until-char #\Linefeed) (EOF-ERROR-P T)
                          EOF-VALUE RECURSIVE-P)
  (declare (ignore recursive-p))
  (let ((x (read-char stream EOF-ERROR-P EOF-VALUE)))
    (if (char-equal until-char x) 
      "" 
      (concatenate 'string (string x) 
                   (read-until stream until-char EOF-ERROR-P EOF-VALUE)))))

(defun read-no-hang (stream &optional (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)
  (let ((str (read-until-no-hang stream #\Linefeed EOF-ERROR-P EOF-VALUE RECURSIVE-P)))
    (when (string-not-equal "" str)
      (format t "ERROR: read-no-hang should return empty string, instead, returned ~A~%" str))))

(defun read-until-no-hang (stream &optional (until-char #\Linefeed) (EOF-ERROR-P T)
                                  EOF-VALUE RECURSIVE-P)
  (declare (ignore recursive-p))
  (let ((x (read-char-no-hang stream EOF-ERROR-P EOF-VALUE)))
    (if (or (null x) (char-equal until-char x)) 
      "" 
      (concatenate 'string (string x) 
                   (read-until-no-hang stream until-char EOF-ERROR-P EOF-VALUE)))))

(defun flush-input-socket (socket &optional (see? t))
  (let (b)
    (loop while (setq b (read socket nil nil)) do
	  (format see? "~A " b))))

;;; ---------------------------------------------------------------------------

(defmethod open-socket-stream (host port &rest args)
  (apply #'ccl::open-tcp-stream host port args))

;;; ---------------------------------------------------------------------------

(defmethod close-socket-stream (socket-stream)
  (close-socket socket-stream))

;;; ---------------------------------------------------------------------------

(defmethod start-server (name listen-port connection-handler &rest handler-args)
  (process-run-function 
   (format nil "~A-server" name)
   (lambda ()                     
     (loop do
           (let ((stream (open-socket-stream 
                          nil listen-port 
                          :element-type 'base-char)))
             (setf (ccl::stream-read-timeout stream) nil)
             (process-wait "Listen" (lambda (s) (listen s)) stream)
             (make-thread 
              name 
              (lambda ()
                (apply connection-handler stream handler-args))))))))
|#

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************