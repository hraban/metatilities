Framework Access 0.91 ReadMe 2004-02-06

Takehiko Abe <keke@gol.com>

2004-02-05 0.9 --> 0.91
           optimized framework.lisp
2004-01-30 first public release


This modle provides define-entry-point for a macho framework library.
It is built on Gary Byer's CFBundle.lisp.

*****

To use it, load the following files in the order:

"package.lisp"
"CFBundle.lisp"
"framework.lisp"
["reader-macro.lisp"]

** "reader-macro.lisp" defines #~ reader macro so that
   we can write (#~CGContextFlush ...) the same way as
   (#_CGContextFlush ...). It is not required for mach:define-entry-point
   to work.

--------------------------------------------------------------------------
;;;
;;; doc
;;;

mach:define-entry-point (name framework-name &optional framework-type)
                           args  &optional return

Arguments:

  name [string] -- name of the entry-point
  framework-name [string] -- name of the framework
  framework-type [keyword] -- :system [default] :full-path or :ccl
  
     * :system specifies that the framework is located under
      /System/Library/Frameworks/. This is the default.

     * :full-path specifies that the framework-name is full pathname to
       the framework.

     * :ccl means that the framework is located under CCL folder.
       [that is, the same folder as MCL app. == (mac-namestring "ccl:")]

  args -- list of argument specs.
        argument specs == (parameter mactype)

  return-type [mactype keyword] -- return type

--------------------------------------------------------------------------
;;;
;;; Examples:
;;;

;;
;; Example 1
;;

(mach:define-entry-point ("SetWindowAlpha" "Carbon.framework")
  ((inWindow :windowref)
   (inAlpha :single-float))
  :osstatus)

(setwindowalpha (wptr (front-window)) 0.5s0)
(setwindowalpha (wptr (front-window)) 1.0s0)


;;
;; Example 2
;;
;; requires "reader-macro.lisp"
;;

(defparameter *w* (make-instance 'window
                    :view-size #@(180 170)))

(defparameter *context*
  (rlet ((context :pointer))
    (if (zerop (#~createcgcontextforport (#_getwindowport (wptr *w*))
                context))
      (%get-ptr context)
      (error "error createcgcontextforport"))))

(progn
  (#~cgcontexttranslatectm *context* 40s0 10s0)
  (#~cgcontextbeginpath *context*)
  (#~cgcontextmovetopoint *context* 0s0 0s0)
  (dotimes (i 5)
    (#~cgcontextaddlinetopoint *context* 100.0s0 0.0s0)
    (#~cgcontexttranslatectm *context* 100s0 0s0)
    (#~cgcontextrotatectm *context* 1.2566371s0))
  (#~cgcontextclosepath *context*)
  (#~cgcontextstrokepath *context*)
  (#~cgcontextflush *context*))


(window-close *w*)
(#~cgcontextrelease *context*)


;;
;; Example 3: CGContextGetPathCurrentPoint
;;
;; CGContextGetPathCurrentPoint is defined in CGContext.h as:
;;
;;     CGPoint CGContextGetPathCurrentPoint(CGContextRef c);
;;
;; Since it returns CGPoint, we can not say:
;;
;; (mach:define-entry-point ("CGContextGetPathCurrentPoint" 
;;                           "ApplicationServices.framework")
;;   ((ctx :pointer))
;;   :CGPoint)
;;
;; Insetead, we need to allocate the memory for CGPoint
;; and path the pointer to it as the first argument to the function.
;; see MachO Runtime Architure pdf page 57.


(defparameter *w* (make-instance 'window))

(defparameter *context*
  (rlet ((context :pointer))
    (if (zerop (#~createcgcontextforport (#_getwindowport (wptr *w*))
                context))
      (%get-ptr context)
      (error "error createcgcontextforport"))))

;; move current point to #@(100 100)
(#~cgcontextmovetopoint *context* 100s0 100s0)

;; this signals error
(rlet ((point :cgpoint))
  (#~CGContextGetPathCurrentPoint *context*)
  (values (pref point :cgpoint.x)
          (pref point :cgpoint.y)))

;; we need:
(mach:define-entry-point ("CGContextGetPathCurrentPoint"
                          "ApplicationServices.framework")
  ((cgpoint :pointer)
   (ctx :pointer))
  nil)

(rlet ((point :cgpoint))
  (CGContextGetPathCurrentPoint point *context*)
  (values (pref point :cgpoint.x)
          (pref point :cgpoint.y)))

(window-close *w*)
(#~cgcontextrelease *context*)


;;
;; Example 4: CGContextAddRect
;;
;; In CGContext.h:
;;
;; void CGContextAddRect(CGContextRef c, CGRect rect);
;;
;; CGRect is a C structure

(defparameter *w* (make-instance 'window))

(defparameter *context*
  (rlet ((context :pointer))
    (if (zerop (#~createcgcontextforport (#_getwindowport (wptr *w*))
                context))
      (%get-ptr context)
      (error "error createcgcontextforport"))))

;; this won't work
(rlet ((rect :cgrect 
             :origin.x 10.0s0
             :origin.y 10.0s0
             :size.width 100s0
             :size.height 100s0))
  (#~cgcontextaddrect *context* rect))

;; We need to expand a CGRect and pass its fields individually.
;; [CGRect is defined in CGGeometry.h]
(mach:define-entry-point ("CGContextAddRect" "ApplicationServices.framework")
  ((context :pointer)
   (orgin-x :single-float)
   (orgin-y :single-float)
   (width :single-float)
   (height :single-float))
  nil)

(cgcontextaddrect *context*
                  10s0
                  10s0
                  100s0
                  100s0)

(#~cgcontextfillpath *context*)
(#~cgcontextflush *context*)


(window-close *w*)
(#~cgcontextrelease *context*)


;; Example 5: Use of :ccl framework-type
;;
;; Copy test.framework folder to CCL directory.
;; So that (probe-file "ccl:test.framework;") returns non-nil.
;;
;; test.framework provides function foo which is defined as:
;;
;; extern int foo (int a, int b)
;; {
;;     return a + b;
;; }
;;
;; 
;; The test.framework is created with XCode.
;; See <http://developer.apple.com/documentation/MacOSX/Conceptual/BPFrameworks/index.html>
;; 


(mach:define-entry-point ("foo" "test.framework" :ccl)
  ((a :long)
   (b :long))
  :long)

(foo 23 23) --> 46




;; @dev-note

(let ((con *context*))
  (#~cgcontextbeginpath con)
  (#~cgcontextmovetopoint con 0s0 100s0)
  (let ((p 0))
    (dotimes (i 14)
      (#~cgcontextaddcurvetopoint con
       (incf p 20s0)
       200s0
       (incf p 20s0)
       0s0
       (incf p 20s0)
       100s0))
    (dotimes (i 14)
      (#~cgcontextaddcurvetopoint con
       (decf p 20s0)
       -100s0
       (decf p 20s0)
       300s0
       (decf p 20s0)
       100s0))
      )
  (#~cgcontextfillpath con)
  (#~cgcontextflush con)
  (#~cgcontextclosepath con))





