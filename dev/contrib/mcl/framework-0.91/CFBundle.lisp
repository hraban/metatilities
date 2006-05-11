;;;-*- Mode: Lisp; Package: BUNDLE-SYS -*-


;;;------------------------
;;; 2004-01-30
;;; Takehiko Abe <keke@gol.com>
;;; This file is based on Gary Byer's original CFBundle.lisp.
;;;


(in-package #:BUNDLE-SYS)

(export '(load-framework-bundle-from-cfurl
          lookup-function-in-framework
          lookup-symbol-in-framework
          system-frameworks-url
          private-frameworks-url))

;;; See the sample code at:
;;; <http://developer.apple.com/samplecode/Sample_Code/Runtime_Architecture/CallMachOFramework.htm>
;;; This is The Official Way to open and use Mach-O libraries from CFM
;;; Carbon applications.  -gb 2/21/02
;;; (Revised slightly, since MCL's interface files now define
;;; more constants and entrypoints than they once did) -gb 11/12/02


;;; Define some shared-library entrypoints for symbols that don't seem
;;; to be in the interface files.  There may be a better way to do this;
;;; if I ever knew it, I've forgotten.

;; no longer used
(defvar *__CFStringMakeConstantString-slep*
  (ccl::get-slep "__CFStringMakeConstantString"))

;; not longer used
(defun CFSTR (string)
  (with-cstrs ((cstr string))
    (ccl::ff-call-slep *__CFStringMakeConstantString-slep*
                       :address cstr 
                       :address)))

(defun %create-system-frameworks-url ()
  (rlet ((fsref :fsref))
    (let* ((err (#_FSFindFolder #$kOnAppropriateDisk #$kFrameworksFolderType #$true fsref)))
      (declare (type (signed-byte 16) err))
      (if (eql #$noErr err)
        (let* ((url (#_CFURLCreateFromFSRef (%null-ptr) fsref)))
          (if (%null-ptr-p url)
            (error "Failed to create URL")
            url))
        (error "Couldn't find system Frameworks folder")))))

(ccl::defloadvar *system-frameworks-url* nil)

(defun system-frameworks-url ()
  (or *system-frameworks-url*
      (setq *system-frameworks-url* (%create-system-frameworks-url))))


(ccl::defloadvar *private-frameworks-url* nil)

(defun %create-private-frameworks-url ()
  "don't forget to cfrelease."
  (let ((bndle (#_CFbundlegetmainbundle)))
    (if (%null-ptr-p bndle)
      (error "cant get main bundle [CFBundleGetMainBundle]")
      (#_CFBundlecopyprivateframeworksurl bndle))))

(defun private-frameworks-url ()
  (or *private-frameworks-url*
      (setq *private-frameworks-url* (%create-private-frameworks-url))))


;; FIXIT
(defun cf-get-string (cfstring &optional (encoding 0) (error-p nil))
  "return string. Inefficient. FIXIT."
  (let ((ptr (#_CFSTringGetCStringPtr cfstring encoding)))
    (if (%null-ptr-p ptr)
      (let ((length (#_CFStringGetlength cfstring)))
        (declare (fixnum length))
        ;; I think we can assume buffer length is always enough.
        ;; CFStringGetCstring can fail due to encoding mismatch.
        (setq length (1+ (#_CFStringGetMaximumSizeForEncoding length encoding)))
        (%stack-block ((buffer length))
          (if (#_CFSTringGetCstring cfstring buffer length encoding)
            (%get-cstring buffer)
            (when error-p
              (error "CFSTringGetCString failed. encoding = ~d."
                     encoding)))))
      (%get-cstring ptr))))

;; FIXIT
(defun cfurl-namestring (cfurl &optional (style :hfs))
  "optional style :hfs or :posix. Defaults to :hfs"
  (let* ((cfstr (#_CFURLcopyfilesystempath cfurl 
                 (ecase style
                   ((:hfs) #$kcfurlhfspathstyle)
                   ((:posix) #$kCFURLPOSIXPathStyle)))))
    (unless (%null-ptr-p cfstr)
      (unwind-protect
        (cf-get-string cfstr #$kCFStringEncodingUTF8)
        (#_cfrelease cfstr)))))


(defun load-framework-bundle-from-cfurl (bundle-url)
  (let* ((bundle (#_CFBundleCreate (%null-ptr) bundle-url)))
    (if (%null-ptr-p bundle)
      (error "Can't create bundle for ~s" (cfurl-namestring bundle-url))
      (if (eql #$false (#_CFBundleLoadExecutable bundle))
        (error "Couldn't load bundle library for ~s"
               (cfurl-namestring bundle-url))
        bundle))))


#|
2003-08-11 23:39:41 keke note

The load-framework-bundle below does not call CFBundleLoadExecutable.

Mike Kluev report that CFBundleGetFunctionPointerForName works without
CFBundleLoadExecutable. And indeed it does.

* whoops, but I only use the sysmtem framworks that must have been
  already loaded before I call CFBundleCreate...

  (#_CFBundleIsExecutableLoaded (system-framework-bundle)) returns
   T even before I call CFBundleGetFunctionPointerForName 
   [lookup-function-in-framework].

  So I cannot say I really confirmed it. but at least the doc says:
  "Calling this function will cause the bundle's code to be loaded
   if necessary." about CFBundleGetFunctionPointerForName.

(defun load-framework-bundle (framework-name)
  (let* ((bundle-url 
          (#_CFURLCreateCopyAppendingPathComponent
           (%null-ptr)
           (sysmte-frameworks-url)    ; file:///System/Library/Frameworks/
           (CFSTR framework-name)
           #$false)))
    (if (%null-ptr-p bundle-url)
      (error "Can't create URL for ~s in system frameworks folder" 
             framework-name)
      (let* ((bundle (#_CFBundleCreate (%null-ptr) bundle-url)))
        (if (%null-ptr-p bundle)
          (error "Can't create bundle for ~s" framework-name)
          bundle)))))

|#

(defun lookup-function-in-framework (symbol-name bundle)
  (with-cfstrs ((symbol-name-cfstr symbol-name))
    (let* ((addr (#_CFBundleGetFunctionPointerForName bundle symbol-name-cfstr)))
      (if (%null-ptr-p addr)
        (error "Couldn't resolve address of foreign function ~s" symbol-name)
        ;; This may be a little confusing: MCL uses fixnums (whose low 2 bits are
        ;; zero) to represent function addresses (whose low 2 bits are zero ...)
        ;; Shove the pointer in a buffer; fetch a signed 32-bit integer, shift it
        ;; right 2 bits ... voila.
        (rlet ((buf :long))
          (setf (%get-ptr buf) addr)
          (ash (%get-signed-long buf) -2))))))


;; keke 2003-09-05
(defun lookup-symbol-in-framework (symbol-name bundle)
  (with-cfstrs ((symbol-name-cfstr symbol-name))
    (let ((addr (#_CFBundleGetDataPointerForName bundle symbol-name-cfstr)))
      (if (%null-ptr-p addr)
        (error "Couldn't resolve address of a symbol ~s" symbol-name)
        addr))))

(provide "CF-BUNDLE")

#|
;;; Lookup the foreign function "gethostname" and call it.
;;; Note that (a) we have to use CCL::PPC-FF-CALL to call the address
;;; (b) that address may become invalid when a saved application is
;;; resumed.
;;; One would want to cache these addresses (and invalidate the cache on
;;; application startup); that's left as an exercise.
;;; The Darwin implementation of gethostname doesn't seem to want to tell
;;; you how big the buffer needs to be.

(defun gethostname ()
  (let* ((gethostname-function-ptr (lookup-function-in-framework
                                    "gethostname" (system-framework-bundle ))))
    (%stack-block ((buf 512))
      (if (eql 0 (ccl::ppc-ff-call
                  gethostname-function-ptr
                  :address buf
                  :unsigned-fullword 512
                  :signed-fullword))
        (%get-cstring buf)))))

|#