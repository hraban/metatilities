(in-package #:metatilities)

(defun make-thread (name function &rest args)
  "Creates a new thread named `name' that executes `function' and takes arguments `args,'
which can start executing immediately.  `name' can be pretty much anything, but strings or
symbols are more informative. Also, `args' is only used for the MCL implementation right now."
   #+lispworks
   (if args 
     (apply #'mp:process-run-function name '() function args)
     (mp:process-run-function name '() function))                
   #+allegro
   (if args
     (mp:process-run-function name function args)
     (mp:process-run-function name function))
   #+MCL
   (if args
     (apply #'ccl:process-run-function name function args)
     (ccl:process-run-function name function))
   #+Clim
   (clim-sys:make-process function :name name))

(defun destroy-thread (thread)
  "Terminates the thread, which is an object as returned by `make-thread.'"
  #+Clim      (clim-sys:destroy-process thread)
  #+DIGITOOL  (ccl:process-kill thread)
  #+OPENMCL   (ccl:process-reset thread t)
  #+LISPWORKS (mp:process-kill thread)
  #+allegro   (mp:process-kill thread))

(defun thread-name (thread)
  #+lispworks (mp::process-name thread)
  #+allegro (mp::process-name thread)
  #+MCL (ccl::process-name thread))

(defun find-thread (name)
  "Returns the thread named name or nil if no such frame exists."
  (dolist (thread (all-threads))
    (when (string-equal name (thread-name thread))
      (return-from find-thread thread)))
  (values nil))

(defun current-thread ()
  "Returns the currently running thread, which will be the same kind of object
returned by `make-thread.'"
  #+Clim     (clim-sys:current-process)
  #+allegro  SYS:*CURRENT-PROCESS*
  #+LISPWORKS mp:*current-process*
  #+MCL      ccl:*current-process*)

(defun all-threads ()
  "Returns a sequence of all the threads."
   #+Clim     (clim-sys:all-processes)
   #+allegro  SYS:*ALL-PROCESSES*
   #+LISPWORKS mp::*processes*
   #+DIGITOOL  *all-processes*
   #+OPENMCL   (all-processes))

(defun thread-wait (reason-string wait-predicate &rest wait-args)
  "This thread is suspended until `wait-predicate' returns true.
`Reason-string' is primarily for debugging/observation purposes."
  ;; Call it once so that the trivial errors happen in the user's process
  ;; (funcall wait-predicate)
  #+allegro  (apply #'mp:process-wait reason-string wait-predicate wait-args)
  #+LISPWORKS  (apply #'mp:process-wait reason-string wait-predicate wait-args)
  #+MCL      (apply #'ccl:process-wait reason-string wait-predicate wait-args)
  #+Clim     
  (progn
    (when wait-args (error "Clim MP does not allow args to process-wait.")
          (clim-sys:process-wait reason-string wait-predicate))))

(defun thread-halt (reason thread)
  "Stops a thread in it's tracks by giving it an arrest reason."
  #+lispworks (push reason (mp:process-arrest-reasons thread))
   #+allegro (mp:process-add-arrest-reason thread reason)
   #+DIGITOOL 
   (progn
     (assert (symbolp reason) nil "Reason must be #'eq testable")
     (ccl::process-enable-arrest-reason thread reason)))

(defun thread-halted-p (thread)
  "Returns non-nill if the thread has been halted."
  #+lispworks (not (null (mp::process-arrest-reasons thread)))
   #+allegro (not (null (mp::process-arrest-reasons thread)))
   #+DIGITOOL (not (null (ccl::process-arrest-reasons thread))))

(defun thread-resume (reason thread)
  "Possibly resumes a thread by taking away an arrest reason."
  #+lispworks (pop (mp::process-arrest-reasons thread))
  #+allegro (mp:process-revoke-arrest-reason thread reason)
  #+DIGITOOL
  (progn 
    (assert (symbolp reason) nil "Reason must be #'eq testable")
    (ccl::process-disable-arrest-reason thread reason)))

(defun thread-resume-absolute (thread)
  "Resumes a thread by taking away all arrest reasons."
  #+lispworks (setf (mp::process-arrest-reasons thread) NIL)
  #+allegro (loop for reason-string in (mp:process-arrest-reasons thread) do
		  (mp:process-revoke-arrest-reason thread reason-string))
  #+DIGITOOL (loop for reason-string in (ccl:process-arrest-reasons thread) do
	           (ccl::process-disable-arrest-reason thread reason-string)))

(defun thread-wait-with-timeout (reason-string seconds wait-predicate)
  "This thread is suspended until `wait-predicate' returns true or `seconds'
pass.  `Reason-string' is primarily for debugging/observation purposes."
  #+Explorer (ticl:process-wait-with-timeout reason-string (* 60 seconds) wait-predicate)
   #+allegro (mp:process-wait-with-timeout reason-string seconds wait-predicate)
   #+Clim     (clim-sys:process-wait-with-timeout reason-string seconds wait-predicate)
   #+LISPWORKS (mp:process-wait-with-timeout reason-string seconds wait-predicate)
   #+MCL      (ccl:process-wait-with-timeout reason-string (round (* 60 seconds)) wait-predicate))

(defun thread-yield ()
  "Allows other processes to run.  As soon as others have had a chance, this
thread continues."
   #+allegro (multiprocessing:process-allow-schedule)
   #+Clim     (clim-sys:process-yield)
   #+LISPWORKS (mp::process-allow-scheduling)
   #+MCL      (ccl:process-allow-schedule))

(defun thread-interrupt (thread function)
  "Interrupts `thread,' throwing out of its computation, and forces it to call
`function.'"
   #+Clim     (clim-sys:process-interrupt thread function)
   #+allegro (multiprocessing::process-preset thread function)
   #+LISPWORKS (mp::process-interrupt thread function)
   #+MCL      (ccl:process-preset thread function))

(defun thread-alive-p (thread)
  "Checks to see if a process is still living."
  (declare (ignore thread))
  (warn "Thread alive-p unimplemented !"))

;;; ============================================================================

(defmacro change-thread (place me you)
  "For two threads that want to `co-routine.' The `place' will be set to either
`me' or `you,' which are typically symbols but can be any marker.  When one
thread calls this, it sets the `place' to be `you,' indicating that it is now
the other thread's turn to run, and waits for `place' to be eq to `me.' Consider
the following example:
   
 (defun test-change-thread ()
   (let ((turn 'a)
         (stream *terminal-io*))
     (let ((a (make-thread \"a\" #'(lambda ()
                                     (dotimes (i 10)
                                       (print 'aaaa stream)
                                       (change-thread turn 'a 'b)))))
           (b (make-thread \"b\" #'(lambda ()
                                     (thread-wait 'first-turn #'(lambda () (eq turn 'b)))
                                     (dotimes (i 10)
                                       (print 'bbbb stream)
                                       (change-thread turn 'b 'a))))))
      (thread-yield)
      (sleep 5)
      (destroy-thread a)
      (destroy-thread b))))

`place' has to be a simple, setfable place, such as a variable or a slot in a
structure or an object.  The `place' form should have no side effects or do any
pointer chasing.  In other words, the macro doesn't use `get-setf-method.'"
  `(progn (setf ,place ,you)
	  (thread-wait ,(format nil "Wait ~s eq ~s" place me)
		       #'(lambda () (eq ,place ,me)))))

#+test
(defun test-change-thread ()
  (let ((turn 'a)
        (stream #+Lispworks *standard-output*))
    (let ((a (make-thread "a" #'(lambda ()
                                 (dotimes (i 10)
                                   (print 'aaaa stream)
                                   (change-thread turn 'a 'b)))))
          (b (make-thread "b" #'(lambda ()
                                 (thread-wait 'first-turn #'(lambda () (eq turn 'b)))
                                 (dotimes (i 10)
                                   (print 'bbbb stream)
                                   (change-thread turn 'b 'a))))))
      (thread-yield)
      (sleep 5)
      (destroy-thread a)
      (destroy-thread b))))

;;; ============================================================================
;;; Experiments in Thread I/O

(defvar *foreground-streams* :unbound)

(defun store-foreground-streams ()
  "A function that stores streams so that they can be used by
`with-foreground-io.'"
  (setf *foreground-streams*
        (list *terminal-io* *debug-io* *error-output* *query-io*
	      *standard-output* *standard-input* *trace-output*)))

(defmacro with-foreground-io (&body body)
  "Binds the CL I/O streams so that I/O goes through the foreground window.  It
does not bind *terminal-io*, since CLtL2 says explicitly that ``no program
should ever change the value of *terminal-io*.  This macro binds the other I/O
streams: *debug-io* *error-output* *query-io* *standard-output* *standard-input*
and *trace-output*.  Note that this will only work if the function
`store-foreground streams' has already been run in the foreground, that is, it
was run before starting up a thread."
  #+(or Lispworks allegro Lucid MCL) 
  `(destructuring-bind
     (*debug-io* *error-output* *query-io* *standard-output*
                 *standard-input* *trace-output*)
     (cdr *foreground-streams*)
     . ,body))

(defun make-output-safe-thread (name function)
  "Similar to make thread, but uses `with-foreground-io' to redirect standard
output streams to reasonable places. See `with-foreground-io' for details."
  #+DEBUG
  (print-output-streams "~a: foreground:" name)  
  (let ((thread (make-thread
		 name
		 #'(lambda ()
		     #+DEBUG
		     (print-output-streams "~a: before with-foreground-io" name)
		     (with-foreground-io
		      #+DEBUG
		      (print-output-streams "~a: after with-foreground-io" name)
		      (funcall function))))))
    (values thread)))

(defun with-timeout-fn (timeout body-fn timeout-fn)
  (block timeout
    #+MCL
    (let* ((result nil)
           (process (process-run-function "body-fn" 
                                          (lambda () 
                                            (setf result (funcall body-fn))))))
      (process-wait-with-timeout "timeout-thread" (* 60 timeout)
                                 (lambda () (not (process-active-p process))))
      (if (process-active-p process) 
        (progn (process-kill process)
               (funcall timeout-fn))
        result))
    #+LISPWORKS
    (let* ((process (current-process))
           (timer (make-timer #'(lambda () (interrupt-process process
                                                              #'(lambda ()
                                                                  (return-from timeout
                                                                    (funcall timeoutf))))))))
      (schedule-timer-relative timer timeout)
      (unwind-protect (funcall bodyf)
        (unschedule-timer timer)))
  ))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate and evaluate TIMEOUT-FORMS."
  #-allegro
  (with-gensyms (bodyf timeoutf)
    `(flet ((,bodyf () ,@body)
            (,timeoutf () ,@timeout-forms))
       (with-timeout-fn ,seconds #',bodyf #',timeoutf)))
  #+allegro
  `(mp::with-timeout (,seconds ,@timeout-forms) ,@body))

#+test
(defun print-output-streams (&optional format-string &rest format-args)
  (dolist (s '(*debug-io* *error-output* *query-io* *standard-output* *standard-input*
			  *trace-output*))
    (let ((stream (eval s)))
      (when (output-stream-p stream)
        (fresh-line stream)
        (when format-string
          (apply #'format stream format-string format-args))
        (write-char #\space stream)
        (princ s stream)
	(write-string " is here" stream)
        (terpri stream)))))

#+test
(defun test-io ()
  (print-output-streams "foreground")
  (make-thread "io test" #'(lambda () (print-output-streams "background")))
  (sleep 1)
  (store-foreground-streams)
  (make-thread "io test" #'(lambda () (with-foreground-io (print-output-streams "fore/background"))))
  (values))

;;; ***************************************************************************
;;; EOF
