;;;-*- Mode: Lisp; Package: BUNDLE-SYS -*-
;;;
;;; 2004-01-30
;;; Takehiko Abe <keke@gol.com>
;;;
;;; See:
;;; <http://developer.apple.com/samplecode/Sample_Code/Runtime_Architecture/CFM_MachO_CFM.htm>

(in-package :bundle-sys)

(export '(create-macho-callback))

(defun create-macho-callback (cfm-callback)
  (cfm-fp-to-macho-fp
   (pref (pref cfm-callback :RoutineDescriptor.RoutineRecords)
         :RoutineRecord.ProcDescriptor)))

;; direct translation of MachOFunctionPointerForCFMFunctionPointer
;; don't know what it does.
(defun cfm-fp-to-macho-fp (cfmfp)
  (let* ((template (list #x3D800000 #x618C0000 #x800C0000 
                         #x804C0004 #x7C0903A6 #x4E800420))
         (template-size #.(* 6 4))
         (mfp (#_newptr template-size)))
    (%put-long mfp
               (logior (elt template 0)
                       (ash (%ptr-to-int cfmfp) -16)))
    (%put-long mfp
               (logior (elt template 1)
                       (logand (%ptr-to-int cfmfp)
                               #xFFFF))
               4)
    (loop for i from 2 to 5
          do
          (%put-long mfp (elt template i) (* i 4)))
    (#_makedataexecutable mfp template-size)
    mfp))


#|

(defccallable free-callback (:ptr info :ptr data :long size :void)
  (declare (ignore info size))
  (print :yipee)
  (#_DisposePtr data))

(defvar mach-o-free-callback
  (cfm-fp-to-macho-fp
   (ccl:pref (ccl:pref free-callback :RoutineDescriptor.RoutineRecords)
             :RoutineRecord.ProcDescriptor)))

(setf provider (#~CGDataProviderCreateWithData
                (%null-ptr) 
                (#_NewPtr 100)
                100
                mach-o-free-callback
                ))

(#_CFRelease provider) --> yipee


|#