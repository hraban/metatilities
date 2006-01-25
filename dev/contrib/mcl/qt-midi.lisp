;;-*- Mode: Lisp; Package: :ccl -*-
;*********************************************************************
;*                                                                   *
;*    Q U I C K T I M E    M I D I    M A N A G E R                  *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexandr@agentsheets.com)     *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2001, AgentSheets Inc.                    *
   ;* Filename  : QuickTime-MIDI-Manager.lisp                        *
   ;* Updated   : 02/08/03                                           *
   ;* Version   :                                                    *
   ;*    1.0    : 12/07/96                                           *
   ;*    1.1    : 01/22/97 works with multiple channels              *
   ;*    1.1.1  : 05/24/97 works on 68k and PPC                      *
   ;*    1.2    : 06/02/97 instrument-number->instrument-name        *
   ;*    1.3    : 02/02/98 AS 2.0                                    *
   ;*    1.4    : 03/20/98 in-package :ccl, QUICKTIME-AVAILABLE-P    *
   ;*    1.4.1  : 03/21/98 instrument# =  0 -> NO instrument         *
   ;*    1.4.2  : 11/18/99 return 'instrument not found'             *
   ;*    2.0    : 06/13/01 Carbonized, Mac OS X native               *
   ;*    2.0.1  : 02/08/03 :reserved gone                            *
   ;* Systems   : PowerPC G4, MCL 5.0                                *
   ;* Abstract  : Play notes on Apple MIDI Synthesizer               *
   ;* Requires  : QuickTime V >= 2.5 with Musical Instruments        *
   ;*                                                                *
   ;******************************************************************

(in-package :ccl)

(export '(USER-PICK-INSTRUMENT QUICKTIME-MUSIC-AVAILABLE-P 
          PLAY-NOTE STOP-THE-MUSIC INSTRUMENT-NUMBER->INSTRUMENT-NAME))

;______________________________
; Error Handling              |
;_____________________________/

(defmacro QTMA-ERROR (Form) "
  in: Form {t}.
  Check for QuickTime Music Architecture error result codes"
  (let ((Error (gensym))
        (Trap (and (listp Form) 
                   (symbolp (first Form))
                   (equal (symbol-package (first Form)) (find-package :traps))
                   (symbol-name (first Form)))))
    `(let ((,Error ,Form))
       (case ,Error
         (#.#$noErr 0)
         (-128 0)
        (t (error "QuickTime Music Architecture Error: ~A calling trap: ~A" ,Error ,Trap))))))


(defun QUICKTIME-MUSIC-AVAILABLE-P () "
  out: True {Boolean}
  Return true if quicktime music is available"
  ;; this should be more specific since
  ;; presence of quicktime does not imply presence of 
  ;; quicktime music
  (rlet ((Result :pointer))
    (eql (#_Gestalt #$gestaltQuickTime Result) #$noErr)))

;______________________________
; Instrument Table             |
;_____________________________/

(defvar *Instruments* (make-hash-table) 
  "hash table <Instrument-Number> => list: <Allocator> <Channel>")

;______________________________
; Internal Functions           |
;_____________________________/

(defvar *Note-Allocator* nil "the current QuickTime Music note allocator")

(defun OPEN-DEFAULT-NOTE-ALLOCATOR () "
  out: Note-Allocator {NoteAllocator}.
  Return a note allocator." 
  (let ((Note-Allocator (#_OpenDefaultComponent :|nota| 0)))
    (when (%null-ptr-p Note-Allocator) (error "QT Music: Could not create note allocator"))
    Note-Allocator))


(defun DEFAULT-NOTE-ALLOCATOR () "
  out: Note-Allocator {NoteAllocator}.
  Return a note allocator. Open one if necessary."
  (or
   *Note-Allocator*
   (setq *Note-Allocator* (open-default-note-allocator))))


(defun MAKE-INSTRUMENT-ALLOCATOR-AND-CHANNEL (Instrument-Number) "
  in:  Instrument-Number {fixnum}.
  out: Allocator {Component}, Channel {NoteChannel}, Request {NoteRequest}.
  Create, initialize and return a Note Allcator, and a Note Channel."
  (let ((Note-Allocator (open-default-note-allocator))
        (Note-Channel (#_NewPtr 4))
        (Note-Request (make-record
                       :NoteRequest 
                       :info.flags 0
                       :info.polyphony 3      ;; three simultaneous tones
                       :info.typicalPolyphony #b0010000
                       :tone.synthesizertype :|ss  |)))
      (let ((&NR.Tone (rref Note-Request :NoteRequest.tone)))
        (QTMA-error (#_NAStuffToneDescription Note-Allocator Instrument-Number &NR.Tone))
        (rlet ((&Note-Request :pointer) 
               (&Note-Channel :pointer))
          (%put-long Note-Channel 0)
          (%put-ptr &Note-Channel Note-Channel)
          (%put-ptr &Note-Request Note-Request)
          (QTMA-error (#_NANewNoteChannel Note-Allocator Note-Request &Note-Channel))
          (setq Note-Channel (%get-ptr &Note-Channel))
          (setq Note-Request (%get-ptr &Note-Request))
          (values
           Note-Allocator
           Note-Channel
           Note-Request)))))


(defun INSTRUMENT-NUMBER->INSTRUMENT-NAME (Instrument-Number) "
  in:  Instrument-Number {fixnum}.
  out: Instrument-Name {string}.
  Return the name of the instrument."
  (if (= Instrument-Number 0)
    "instrument undefined"
    (if (quicktime-music-available-p)
      (let ((Note-Allocator (open-default-note-allocator)))
        (rlet ((Note-Request 
                :NoteRequest 
                :info.flags 0
                :info.polyphony 2      ;; two simultaneous tones
                :info.typicalPolyphony #b0010000))
          (let ((&NR.Tone (rref Note-Request :NoteRequest.tone)))
            (QTMA-error (#_NAStuffToneDescription Note-Allocator Instrument-Number &NR.Tone))
            (prog1
              (let ((Name (rref Note-Request :NoteRequest.tone.instrumentName)))
                (if (string-equal Name "")
                  "instrument not found"
                  (copy-seq (rref Note-Request :NoteRequest.tone.instrumentName))))
              (QTMA-error (#_CloseComponent Note-Allocator))))))
      "Quicktime not installed")))
  

(defun USER-PICK-INSTRUMENT (&optional (Default-Instrument-Number 1)) "
  in: &optional Default-Instrument-Number {fixnum} default 1 = piano.
  out: Instrument-Number {fixnum},
       Instrument-Name {string}.
  Make the user select an instrument.
  Return the instrument number or nil if no instrument 
  was selected."
  (when (= Default-Instrument-Number 0) (setq Default-Instrument-Number 1))
  (let ((Note-Allocator (open-default-note-allocator)))
    (rlet ((Note-Request 
            :NoteRequest 
            :info.flags 0
            :info.polyphony 2      ;; two simultaneous tones
            :info.typicalPolyphony #b0010000))
      (let ((&NR.Tone (rref Note-Request :NoteRequest.tone)))
        (QTMA-error (#_NAStuffToneDescription Note-Allocator Default-Instrument-Number &NR.Tone))
        (with-pstrs ((Prompt "Pick An Instrument:"))
          (QTMA-error (#_NAPickInstrument Note-Allocator (%null-ptr) Prompt &NR.Tone #$kPickUserInsts 0 0 0)))
        (multiple-value-prog1
          (values 
           (rref Note-Request :NoteRequest.tone.instrumentnumber)
           (copy-seq (rref Note-Request :NoteRequest.tone.instrumentName)))
          (QTMA-error (#_CloseComponent Note-Allocator)))))))

;______________________________
; External Functions           |
;_____________________________/

(defun PLAY-NOTE (Pitch Velocity &optional (Instrument-Number (user-pick-instrument))) "
  in:  Pitch {fixnum}, Velocity {fixnum}, &optional Instrument-Number {fixnum} default (user-pick-instrument).
  Play a note with <Pitch> at <Velocity> on <Instrument-Number>."
  (when (= 0 Instrument-Number) (return-from play-note))
  (let ((Instrument (gethash Instrument-Number *Instruments*)))
    (cond
     (Instrument                      ;;; found instrument in cache!
      (QTMA-error (#_NAPlayNote (first Instrument) (second Instrument) Pitch Velocity)))
     (t
      (multiple-value-bind (Allocator Channel Request)
                           (make-instrument-allocator-and-channel Instrument-Number)
        (setf (gethash Instrument-Number *Instruments*) (list Allocator Channel Request))
        (QTMA-error (#_NASetNoteChannelVolume Allocator Channel 100000))
        (QTMA-error (#_NAPlayNote Allocator Channel Pitch Velocity)))))))


(defun STOP-THE-MUSIC () "
  Close ALL the currently open Components and NoteChannels.
  This function should be called before the application is quit."
  (maphash
   #'(lambda (Instrument-Number Instrument)
       (declare (ignore Instrument-Number))
       (#_NADisposeNoteChannel (first Instrument) (second Instrument))
       (#_CloseComponent (first Instrument)))
   *Instruments*)
  (clrhash *Instruments*)
  (setq *Note-Allocator* nil))


(defun PLAY-MELODY (Melody &optional (Instrument-Number 1) (Time 0.5))
  (let ((Note-Events nil))
    (dolist (Note Melody)
      (let ((Pitch (if (listp Note) (first Note) Note)))
        (let* ((Now (get-internal-real-time))
               (Stop-Time (+ Now (* (if (listp Note) (* Time 4 (second Note)) Time) internal-time-units-per-second))))
          (play-note Pitch 127 Instrument-Number)
          ;; check if some old sounds are still playing and need to be stopped
          (setq Note-Events
                (mapcan
                 #'(lambda (Event)
                     (cond
                      ;; time to stop
                      ((> Now (first Event))
                       (play-note (rest Event) 0 Instrument-Number)
                       nil)
                      (t (list Event))))
                 Note-Events))
          ;; add the sound just played to note events
          (push (cons (+ (get-internal-real-time) 4000) Pitch) Note-Events)
          ;; wait until it's time for next note
          (loop (when (> (get-internal-real-time) Stop-Time) (return))))))))

#|
    ;; stop remaining notes
    (dolist (Event Note-Events)
      (play-note (rest Event) 0 Instrument-Number))))

|#
 

#| Examples:

(user-pick-instrument)

;; Lullaby by Brahms For Lorenzo and Viviana
;; fixed up by David Cope
(loop
  (play-melody '((35 1/2) (35 1/2) (36 1/4) 60 60 60) 16385 0.2)
  (play-melody '(64 64 (67 1)
                 64 64 (67 1)
                 64 67 (72 1/2)(71 1/2)(69 1/2)(69 1/2)(67 1/2)
                 62 64 (65 1/2) (62 1/2) 62 64 (65 1/2) (62 1/2)
                 62 65 71 69 (67 1/2)(71 1/2)(72 1)
                 60 60 (72 1) 69 65 (67 1) 64 60 (65 1/2)(67 1/2)
                 (69 1/2)(67 1) 60 60 (72 1) 69 65 (67 1) 64 60
                 (65 1/2)(64 1/2)(62 1/2)(60 2))
               1
               0.2))


(play-note 35 127 16385)

(play-note 60 127 113)
(play-note 60 0 113)


(play-note 60 90 1)


(let ((I 49))
  (play-note 60 127 I)
  (sleep 1)
  (play-note 60 0 I))

(dotimes (i 40)
  (play-note (+ 20 i) 127 1)
  (sleep 0.1))

(play-note 60 127 124)
(play-note 60 0 124)

;;; Echos

(play-note 60 127 103)
(play-note 58 127 103)
(play-note 40 127 103)
(play-note 30 127 103)

(play-note 60 0 103)
(play-note 58 0 103)
(play-note 40 0 103)
(play-note 30 0 103)


;;; drum kit

(loop
  (let ((Note (+ 30 (random 40))))
    ;;start
    (play-note Note (+ 64 (random 64)) 16385)   
    (sleep (random 0.2))
    ;;stop
    ;;(play-note Note 0)
    (sleep (random 0.1))))

(dotimes (i 127) 
  (sleep 0.5)
  (play-note i 100 16385))

(loop
  (play-note 70 60 101)
  (sleep (random 1.0))
  (play-note 70 0 101))


|#