;;; -*- package: ccl -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM      I M A G E      D I A L O G   I T E M              *
;*                                                                   *
;*********************************************************************
   ;* Author    : Alexander Repenning (alexandr@agentsheets.com)     *
   ;*             http://www.agentsheets.com                         *
   ;* Copyright : (c) 1996-2004, AgentSheets Inc.                    *
   ;* Updated   :  07/06/04                                          *
   ;* Filename  : Image-Dialog-Item.lisp                             *
   ;* Version   :                                                    *
   ;*   1.0     :  05/11/01                                          *
   ;*   2.0     :  12/12/02 OS X                                     *
   ;*   2.1     :  05/02/03 Garbage Collection handling              *
   ;*   2.1.1   :  06/24/03 File Type error checking                 *
   ;*   2.1.2   :  03/23/04 remove red background                    *
   ;*   2.1.3   :  07/06/04 :pixel-depth :initarg to overwrite       *
   ;* HW/SW     : G4,  MCL 5, OS X 10.3.4                            *
   ;* Abstract  : QuickTime based interface for Image Dialog         *
   ;*    Items that can  be loaded from most types of files          *
   ;*    including: GIF, JPEG, PNG, PICT, JFIF, PDF, MacPaint,       *
   ;*               Flash 5, Photoshop, QuickTime Image File, BMP,   *
   ;*               SGI, Targa, TIFF, and FlashPix                   *
   ;******************************************************************

;; All File Formats: http://www.apple.com/quicktime/products/qt/specifications.html

(in-package :ccl)

(export '(image-dialog-item))

;********************************
;   IMAGE-DIALOG-ITEM           *
;********************************

(defclass IMAGE-DIALOG-ITEM (dialog-item)
  ((gworld :accessor gworld)
   (filename :accessor filename :initform nil :initarg :filename)
   (pixel-depth :accessor pixel-depth :initform nil :initarg :pixel-depth :documentation "Use to overwrite image file depth as default"))
  (:documentation ""))


(defmethod INITIALIZE-INSTANCE ((Self image-dialog-item) &rest Initargs)
  (declare (ignore Initargs))
  (call-next-method)
  (when (filename Self) 
    (setf (gworld Self) (load-image-file Self (filename Self))))
  ;; if garbage collected free the GWorld
  (terminate-when-unreachable Self))


(defmethod VIEW-DRAW-CONTENTS ((Self image-dialog-item))
  (when (gworld Self)
    (with-focused-view Self
      (rlet ((&rect :rect :topleft #@(0 0) :bottomRight (view-size Self)))
        (#_LockPixels (#_GetGWorldPixMap (gworld Self)))
        (#_CopyBits
         (#_GetPortBitMapForCopyBits (gworld Self))
         (#_GetPortBitMapForCopyBits (#_GetWindowPort (wptr Self)))
         &rect
         &rect
         #$ditherCopy 
         (%null-ptr))
        (#_UnlockPixels (#_GetGWorldPixMap (gworld Self)))))))


(defmethod LOAD-IMAGE-FILE ((Self image-dialog-item) Pathname)
  (rlet ((&gi :handle)
         (Fsspec :fsspec))
    ;; Set File Spec
    (with-pstrs ((Name (mac-namestring (probe-file Pathname))))
      (unless (zerop (#_fsmakefsspec 0 0 Name Fsspec))
        (error "Invalid file reference ~A" Pathname))
      ;; Read Image dimensions and create GWorld
      (let ((Err (#_GetGraphicsImporterForFile Fsspec &gi)))
        (unless (zerop Err) (error "Cannot import image file. GetGraphicsImporterForFile Err=~A" Err))))
    (let ((gi (%get-ptr &gi)))
      (rlet ((&ImageDescriptor :pointer))
        (#_GraphicsImportGetImageDescription gi &ImageDescriptor)
        (let ((ImageDescriptor (%get-ptr &ImageDescriptor)))
          ;; if there is not user defined size use the original size of the image
          (unless (view-size Self) 
            (set-view-size Self (make-point (rref ImageDescriptor :imagedescription.width)
                                            (rref ImageDescriptor :imagedescription.height))))
          ;; create the GWorld
          (rlet ((&GWorld :pointer)
                 (&rect :rect :topleft #@(0 0) :botright (view-size Self)))
            (unless (zerop (#_NewGWorld 
                            &GWorld
                            (or (pixel-depth Self) (rref ImageDescriptor :imagedescription.depth))
                            &rect
                            (%null-ptr)
                            (%null-ptr)
                            0))
              (error "Cannot create GWorld"))
            (let ((GWorld (%get-ptr &GWorld)))
              (dispose-record ImageDescriptor)
              (#_CloseComponent gi)
              ;; write file content into GWorld
              (#_SetGWorld GWorld (%null-ptr))
              (#_LockPixels (#_GetGWorldPixMap GWorld))
              (#_GetGraphicsImporterForFile Fsspec &gi)         ;; kind redundant but needed
              (let ((gi (%get-ptr &gi)))
                ;; set bounds
                (rlet ((rect :rect :topleft #@(0 0) :botright (view-size Self)))
                  (#_GraphicsImportSetBoundsRect gi rect))
                (unless (zerop (#_GraphicsImportDraw gi)) (error "QuickTime error: _GraphicsImportDraw"))
                (#_UnlockPixels (#_GetGWorldPixMap GWorld))
                (#_CloseComponent gi))
              ;; return the GWorld
              GWorld)))))))


(defmethod TERMINATE ((Self image-dialog-item))
  ;;(format t "disposed GWorld: ~A" Self)
  (when (gworld Self)
    (#_DisposeGWorld (gworld Self))))



#| Examples:

(defparameter *Window* (make-instance 'window :color-p t))

(defparameter *File* (choose-file-dialog))

;; size defined by user:
(defparameter *IDI* (make-instance 'image-dialog-item :filename *File* :view-size #@(200 100)))

(add-subviews *Window* *IDI*)

;; original size from image used:
(defparameter *IDI2* (make-instance 'image-dialog-item :filename *File* :view-position #@(50 50)))

(add-subviews *Window* *IDI2*)



;; garbage collection

(discard-all-subviews *Window*)

(window-close *Window*)

(setq *Window* nil)

(setq *IDI* nil)

(setq *IDI2* nil)

(trace terminate)


(gc)



|#


  