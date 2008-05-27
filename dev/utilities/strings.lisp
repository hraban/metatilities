(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; Every good set of utilities needs STRING utilities!!!
;;; ---------------------------------------------------------------------------


;; this should have a lot more &key args.  Add 'em when
;;  you need 'em.
(defun string-before (string char &key (from-end nil) (start 0))
  "Returns the part of string before the first appearance of CHAR.
   (If FROM-END is T, we count from the end.)  If CHAR does not
   appear in STRING, then we return the whole string."
  (subseq string start
          (position char string :from-end from-end)))


(defun string-after (string char &key (from-end nil) (end nil))
  "Returns the part of string before the first appearance of CHAR.
   (If FROM-END is T, we count from the end.)  If CHAR does not
   appear in STRING, then we return the whole string."
  (subseq string
          (1+ (position char string :from-end from-end))
          end))


;;; ----------------------------------------------------------------------------
;;;
;;;   A few functions cribbed from Wiblur.  Feel free to improve.
;;;

(defun substring (string start &optional end downcasep)
  (declare (type string string) (type fixnum start) (optimize (speed 3) (safety 0)))
  (let* ((end (or end (length string)))
         (rv (make-string (- end start))))
    (declare (type fixnum end))
    (do ((r-index start (1+ r-index))
         (w-index 0 (1+ w-index)))
        ((>= r-index end) rv)
      (declare (type fixnum r-index w-index))
      (setf (char rv w-index) 
            (let ((c (char string r-index)))
              (if downcasep
                (char-downcase c)
                c))))))

(defun (setf substring) (new-value string &optional (start 0) end downcasep)
  (declare (type string string new-value))
  (let* ((end (or end (length string)))
         (end (min (+ start (length new-value)) end)))
    (declare (type fixnum end))
    (do ((w-index start (1+ w-index))
         (r-index 0 (1+ r-index)))
        ((>= w-index end) new-value)
      (declare (type fixnum r-index w-index))
      (setf (char string w-index) 
            (let ((c (char new-value r-index)))
              (if downcasep
                (char-downcase c)
                c))))))

;; Nor is this cribbed from Wilbur --ach
;;?? Gary King 2003-06-22: could probably use #'char-equal and simplify this a little
(defun string-contains-p (string string-to-find
                                 &key (start 0) 
                                 (end (length string))
                                 (case-sensitive? nil))
  "Returns t if string contains the string-to-find. Case insensitive unless specified, and you can also set a start and an end to search over in the string." 
  (if (> (length string-to-find) (length string))
    (values nil)
    (if case-sensitive?
      (search string-to-find string :test #'string= :start2 start :end2 end)
      (search (string-downcase string-to-find) (string-downcase string) :test #'string=
              :start2 start :end2 end))))

(defun collect-to-char (char string &key (start 0) end downcasep)
  (declare (type string string) (type fixnum start) (optimize (speed 3) (safety 0)))
  (let ((end-index (position char string :start start :end end :test #'char=)))
    (when end-index 
      (values (substring string start end-index downcasep) end-index))))

(defun collect-to-not (char string &key (start 0) end downcasep)
  (declare (type string string) (type fixnum start) (optimize (speed 3) (safety 0)))
  (let ((end-index (position char string :start start :end end :test-not #'char=)))
    (when end-index 
      (values (substring string start end-index downcasep) end-index))))

;;; ---------------------------------------------------------------------------

(defun string->symbol (string &optional (package nil))
  "Returns the string as a bare symbol, replacing any whitespace with dashes. ~
   This is the \(near\) inverse of symbol->string. Return nil for empty strings"
 (let* ((new-string (substitute-if #\- 'whitespacep string))  
         (new-thing (read-from-string new-string nil :eof)))
    (cond ((eq new-thing :EOF) (values nil))
          
          ((stringp new-thing)
           (string->symbol new-thing package))
          
          (package (form-symbol-in-package package new-thing))
          
          (t new-thing))))

;;; ---------------------------------------------------------------------------

(defun symbol->string (string-or-symbol &optional (replace-dash? t))
  "Returns a symbol as a string, replacing any dashes with spaces. The inverse ~
   of string->symbol."
  (when (symbolp string-or-symbol) 
    (setf string-or-symbol (symbol-name string-or-symbol)))
  (string-capitalize (or (and replace-dash? (substitute #\space #\- string-or-symbol))
                         string-or-symbol)))

;;; ---------------------------------------------------------------------------

(defun tokenize-string (string &key (start 0) (end (length string)) 
                               (delimiter #\Space) (include-empties? nil))
  "Splits a string at delimiter and returns a list of the parts."  
  (declare (type string string))
  (let ((current start)
        (token nil) (result nil))
    (flet ((save-token (the-token)
             (push the-token result)))
      (loop while (< current end)
            with new = nil do
            (setf (values token new)
                  (collect-to-char delimiter string :start current :end end))
            (cond (new 
                   (setf current (+ new 1)))
                  (t
                   (save-token (substring string current))
                   (setf current end)))
            when (and token (or include-empties?
                                (not (string= token "")))) do (save-token token)))
    (nreverse result)))

;;; ---------------------------------------------------------------------------

(defun list->formatted-string (list &optional (separator ", ") (end-with "."))
  "This is a convenience function for when the intracacies of format are just too much. It takes a list and returns a string formatted with `delimiter' between each sucessive element and ending with `ends-with`. look in CLtL2, section 22.3.7.2 for the full deal."
  (apply #'format nil
         (concatenate 'string "~#[~;~A" end-with "~:;~@{~#[~;~]~A~^"  separator "~}~]")
         list))
            
#+WIP
(defun format-list (list &key (separator ", ") (end-with ".") (final-separator " and")
                         (empty-list "none"))
  (apply #'format nil
         (concatenate 'string "~#["
                      empty-list "~;~S~;~S"
                      final-separator "~S~:;~@{~#[~;"
                      final-separator "~]~S~^"
                      separator "~}~]"
                      end-with) 
         list))
            


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
