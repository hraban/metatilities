;;-*- Mode: Lisp; Package: ccl -*-
;******************************************************************
;*                                                                *
;*    S P E E C H   R E C O G N I T I O N   M A N A G E R         *
;*                                                                *
;******************************************************************
;* Author       : Alexander Repenning                             *
;* Copyright    : 2001 (c) AgentSheets Inc.                       *
;*                http://www.agentsheets.com                      *
;* Filename     : Speech-Recognition-Manager.lisp                 *
;* Last Update  : 11/10/01                                        *
;* Version                                                        *
;*    1.0       : 09/03/01                                        *
;*    1.1       : 09/10/01 speech boost, command window           *
;*    1.2       : 11/10/01 Command Set Window feedback and click  *
;* HW/SW        : G4, OS 9, MCL 4.3.1                             *
;* Abstract     : Define a list of strings as voice commands      *
;*                                                                *
;******************************************************************

#| Status:

This first version may be not the finest in Lisp programming; it's a 
close interpretation of the "Weapons Example" sample code of 
ftp://ftp.apple.com/developer/Development_Kits/Speech_Recog_Mgr.sit.hqx

Less versatile than "speech-recognition.lisp" but works more reliable
(not sure why) and is really trivial to set up. See example at the end 
of this file. Don't forget OS 9 speech recognition is not great but 
if you play with the microphone setup and tweak your command set you 
will get a good speech interface.

MCL 4.3.1 problems: the "SpeechRecognition.lisp" file inside the interfaces
on the MCL 4.3.1 CD does not work. Replace it with the one from the MCL 4.3 CD
and (reindex-interfaces)

|#


(in-package :ccl)

(export '(define-speech-commands add-speech-command
           define-recognition-handler
           start-speech-recognition stop-speech-recognition
           speech-recognition-running-p has-speech-recognition-p
           command-set-window unmark-string))

;; not sure why this is needed: maybe due to MCL 4.3.1 problem
(add-to-shared-library-search-path "SpeechRecognitionLib" :check)

;_______________________________
;  Globals                      |
;______________________________/

(defvar *Speech-Commands* nil "list of currently defined speech recognition commands")

(defvar *Recognition-System* nil)

(defvar *Language-Model* nil)

(defvar *Recognizer* nil)

(defvar *Recognition-Handler* #'print)

(defconstant *Speech-RefCon* :|coma|)

;_______________________________
;  Trap Error Handling          |
;______________________________/

(eval-when (load compile eval)
(defmacro ERR? (Form) "
  Error handling trap call wrapper"
  (let ((Error (gensym))
        (Trap (and (listp Form) 
                   (symbolp (first Form))
                   (equal (symbol-package (first Form)) (find-package :traps))
                   (symbol-name (first Form)))))
    `(let ((,Error ,Form))
       (case ,Error
         (#.#$noErr 0)
        (t (error "~A calling trap: ~A" ,Error ,Trap)))))))

;_______________________________
;  Low-Level                    |
;______________________________/

(defun HAS-SPEECH-RECOGNITION-P ()
  (rlet ((&mySRVersion :pointer))
    (and (zerop (#_Gestalt #$gestaltSpeechRecognitionVersion &mySRVersion))
         (>= (%get-long &mySRVersion) #x0150))))


(defun SPEECH-RECOGNITION-RUNNING-P () "
  in: Is-Running {boolean}.
  Return non nil if the speech recognition is currently running"
  (if *Recognition-System* t nil))


(defun AUTOMATIC-GAIN-P ()
  (rlet ((&inRefNum :long)
         (&autoGain :word))
    (err? (#_SPBOpenDevice (%null-ptr) #$siReadPermission &inRefNum))
    (err? (#_SPBGetDeviceInfo (%get-long &inRefNum) #$siAGCOnOff &autoGain))
    (not (zerop (%get-word &autoGain)))))

;(automatic-gain-p)

;_______________________________
;  STRING LIST Window           |
;______________________________/

(defclass STRING-LIST-WINDOW (windoid)
  ((strings :accessor strings :initform nil :initarg :strings))
  (:documentation "A floating window that maintains a changeable list of strings"))


(defmethod INITIALIZE-INSTANCE ((Self string-list-window) &rest Init)
  (apply #'call-next-method Self :close-box-p nil Init)
  (set-window-title Self "Speak")
  (set-view-position Self #@(100 100))
  (set-view-font Self '("Geneva" 9 :plain)))


(defmethod (setf STRINGS) :after (Strings (Self string-list-window))
  (cond
   (Strings
    ;; if there are strings make window visible and 
    ;; keep it at a minimal size to show all strings as well as the title
    (set-view-size Self (make-point 
                         (max (+ (reduce #'max Strings 
                                         :key #'(lambda (String) (string-width String '("Geneva" 9 :plain))))
                                 4)
                              100)
                         (+ (* 15 (length Strings)) 2)))
    (invalidate-view Self))
   (t
    ;; no need to show an empty window
    (window-hide Self))))


(defmethod VIEW-DRAW-CONTENTS ((Self string-list-window))
  (with-focused-view Self
    (let ((Y -3))
      (rlet ((Box :rect :topleft #@(0 0) :botright (make-point (point-h (view-size Self)) 20)))
        (dolist (String (strings Self))
          (incf Y 15)
          ;; erase a box and draw a string one at a time to avoid flicker
          (#_EraseRect Box)
          (#_OffsetRect Box 0 15)
          (#_MoveTo 2 Y)
          (with-pstrs ((&string String))
            (#_DrawString &string)))))))


(defvar *Command-Set-Window* nil "window displaying the current Speech Recognition Commands")

(defun COMMAND-SET-WINDOW () 
  (or *Command-Set-Window* 
      (setq *Command-Set-Window* (make-instance 'string-list-window :window-show nil))))


(defmethod MARK-STRING ((Self string-list-window) String)
  (let ((Index (position String (strings Self) :test #'string-equal)))
    (when Index 
      (with-focused-view Self
        (with-pen-saved
          (#_penMode #$srcXor)
          (rlet ((Box :rect :topleft (make-point 0 (* Index 15))
                      :botright (make-point (point-h (view-size Self)) (+ (* Index 15) 16))))
            (#_paintRect Box)))))))


(defmethod UNMARK-STRING ((Self string-list-window) String)
  (mark-string Self String))


(defmethod VIEW-CLICK-EVENT-HANDLER ((Self string-list-window) Where)
  ;; on mouseup manually call the speech recognition hander
  (let ((String (nth (truncate (point-v Where) 15) (strings Self))))
    (mark-string (command-set-window) String)
    (loop (unless (mouse-down-p) (return)))
    (funcall *Recognition-Handler* String)))

;_______________________________
;  Performance Booster          |
;______________________________/
#| 
Especially on a G3 other applications or MCL may not give 
Speech Recognition and Recognition Feedback sufficient chance to work reliably and quickly.

|#

(defvar *Use-Speech-Performance-Boost-p* t "set to true to make speech recognition use boost mode: additional idle and wait event cycles")

(defvar *Stop-Speech-Performance-Boost-p* nil "set true to stop ongoing speech boost process")

(defun START-SPEECH-PERFORMANCE-BOOSTER ()
  (setq *Stop-Speech-Performance-Boost-p* nil)
  (process-run-function
   "Speech Performance Boost"
   #'(lambda ()
       (loop
         (when *Stop-Speech-Performance-Boost-p* (return))
         (#_SRIdle)
         (event-dispatch)))))


(defun STOP-SPEECH-PERFORMANCE-BOOSTER ()
  (setq *Stop-Speech-Performance-Boost-p* t))

;_______________________________
;  Start/Stop/Define            |
;______________________________/

(defvar *Command-Id* 0)


(defun UPDATE-COMMAND-SET-WINDOW ()
  (setf (strings (command-set-window)) (sort (mapcar #'rest *Speech-Commands*) #'string-lessp))
  (when *Speech-Commands*
    (window-show (command-set-window))))


(defun DEFINE-SPEECH-COMMANDS (&rest Strings) "
  in: &rest Strings {string}.
  Define a list of strings to be used as speech commands.
  Commands need to be defined before starting speech recognition"
  (setq *Speech-Commands* nil)
  (setq *Command-Id* 0)
  (dolist (String Strings)
    (push (cons (incf *Command-Id*) String) *Speech-Commands*))
  (update-command-set-window))


(defun ADD-SPEECH-COMMAND (String) "
  in: String {string}.
  Add a speech command to speech recognition.
  Speech commands can be added to a running system."
  ;; Don't add the same string twice
  (unless (member String *Speech-Commands* :key #'rest :test #'string-equal)
    (incf *Command-Id*)
    (push (cons *Command-Id* String) *Speech-Commands*)
    (when *Recognition-System* ;; recognition system is active
      ;; command needs to be added to current language model
      (with-cstrs ((&string String))
        (err? (#_SRAddText *Language-Model* &string (length String) *Command-Id*))))
    (update-command-set-window)))


(defun DEFINE-RECOGNITION-HANDLER (Handler) "
  in: Handler {lambda ({string})}.
  The <Handler> is called with the recognized string when 
  the speech recognition is done."
  (setf *Recognition-Handler* Handler))


(defun BUILD-LANGUAGE-MODEL ()
  (rlet ((&languageModel :pointer (%null-ptr)))
    (with-pstrs ((Name "<command>"))
      (err? (#_SRNewLanguageModel *Recognition-System* &languageModel Name (length "<command>")))
      (setq *Language-Model* (%get-ptr &languageModel))))
  (rlet ((&refCon :ostype *Speech-RefCon*))
    (err? (#_SRSetProperty *Language-Model* #$kSRRefCon &refCon 4)))
  ;; add all the command definitions to the language model
  (dolist (Command *Speech-Commands*)
    (with-cstrs ((&string (rest Command)))
      (err? (#_SRAddText *Language-Model* &string (length (rest Command)) (first Command))))))


(defun START-SPEECH-RECOGNITION ()
  (rlet ((&gRecSystem :pointer (%null-ptr)))
    (err? (#_SROpenRecognitionSystem &gRecSystem #$kSRDefaultRecognitionSystemID))
    (setq *Recognition-System* (%get-ptr &gRecSystem)))
  (rlet ((&myModes :short #$kSRHasFeedbackHasListenModes))
    (err? (#_SRSetProperty *Recognition-System* #$kSRFeedbackAndListeningModes &myModes 2)))
  (build-language-model)
  (rlet ((&gRecognizer :pointer (%null-ptr)))
    (err? (#_SRNewRecognizer *Recognition-System* &gRecognizer #$kSRDefaultSpeechSource))
    (setf *Recognizer* (%get-ptr &gRecognizer)))
  (rlet ((&enable :boolean t))
    (err? (#_SRSetProperty *Recognizer* #$kSRBlockModally &enable 1))) ;; block other speech systems
  (rlet ((&speed :short 100))
    (err? (#_SRSetProperty *Recognizer* #$kSRSpeedVsAccuracyParam &speed 2)))
  (err? (#_SRSetLanguageModel *Recognizer* *Language-Model*))
  (err? (#_SRStartListening *Recognizer*))
  (update-command-set-window)
  (when *Use-Speech-Performance-Boost-p* (start-speech-performance-booster)))


(defun STOP-SPEECH-RECOGNITION ()
  (stop-speech-performance-booster)
  (when *Language-Model* 
    (err? (#_SRReleaseObject *Language-Model*))
    (setq *Language-Model* nil))
  (when *Recognizer*
    (err? (#_SRStopListening *Recognizer*))
    (err? (#_SRReleaseObject *Recognizer*))
    (setq *Recognizer* nil))
  (when *Recognition-System*
    (err? (#_SRCloseRecognitionSystem *Recognition-System*))
    (setq *Recognition-System* nil))
  (window-hide (command-set-window)))
    
;_______________________________
;  Recognition Handler          |
;______________________________/

(defun HANDLE-RECOGNITION-RESULT (Result)
  (rlet ((&myResultLM :pointer)
         (&myLen :long)
         (&myRefCon :long)
         (&mySubElement :pointer))
    ;; get the language model
    (err? (#_SRGetProperty Result #$kSRLanguageModelFormat &myResultLM &myLen))
    (let ((myResultLM (%get-ptr &myResultLM)))
      ;; get the refCon which is a number refering to the command string
      (%put-long &myLen 4)              ;; size of long
      (err? (#_SRGetProperty myResultLM #$kSRRefCon &myRefCon &myLen))
      ;; make sure the language model is meant for us
      (when (equal (%get-ostype &myRefCon) *Speech-RefCon*)
        (err? (#_SRGetIndexedItem myResultLM &mySubElement 0))
        (let ((mySubElement (%get-ptr &mySubElement)))
          (%put-long &myLen 4)
          (err? (#_SRGetProperty mySubElement #$kSRRefCon &myRefCon &myLen))
          (let ((String (rest (first (member (%get-long &myRefCon) *Speech-Commands* :key #'first)))))
            (mark-string (command-set-window) String)
            (funcall *Recognition-Handler* String))
          (err? (#_SRReleaseObject mySubElement))))
      (err? (#_SRReleaseObject myResultLM))
      (err? (#_SRReleaseObject Result)))))
      

(defmethod SPEECH-DONE-APPLEEVENT-HANDLER ((App Application) theAEevt reply refCon)
  (declare (ignore App reply refCon))
  (rlet ((&actualType :ostype)
         (&actualSize :signed-long)
         (&recStatus :word)
         (&recResult :pointer))
    ;; get speech status
    (err? (#_AEGetParamPtr theAEevt #$keySRSpeechStatus 
           #$typeInteger &actualType &recStatus 2 &actualSize))
    (when (zerop (%get-word &recStatus))
      ;;; get result
      (err? (#_AEGetParamPtr theAEevt #$keySRSpeechResult
             #$typeSRSpeechResult &actualType &recResult 4 &actualSize))
      (handle-recognition-result (%get-ptr &recResult)))))


(install-appleevent-handler #$kAESpeechSuite #$kAESpeechDone #'speech-done-appleevent-handler)



#| Examples:

(has-speech-recognition-p)

(define-speech-commands "Bowie Knife" "Forty-Four Pistol" "Machine Gun" "Double Barrel Shotgun"
  "Flame Thrower" "Rocket launcher"
  "Left" "Right" "Lift" "Sink")


(define-recognition-handler
  #'(lambda (String)
      ;;(format t "~%I just recognized ~S" String)
      (sleep 0.2)
      (unmark-string (command-set-window) String)))

(start-speech-recognition)

(add-speech-command "Cell Phone")

(add-speech-command "Laser")

(stop-speech-recognition)


|#