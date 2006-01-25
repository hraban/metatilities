;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pop-up menu functions

(in-package :ccl)

(export '(select-item-from-pup pup-arrow-draw menu-select-action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus-u.Lisp
;;
;; Copyright © 1991 Northwestern University Institute for the Learning Sciences
;; All Rights Reserved
;;
;; author: Michael S. Engber
;;
;; utilities for menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pup-arrow-draw (rect &key (width 10) (right-indent 4))
  (#_MoveTo (- (pref rect :Rect.right) width right-indent)
   (floor (- (+ (pref rect :Rect.top) (pref rect :Rect.bottom)) (ceiling width 2)) 2))
  (loop
    (#_Line width 0)
    (decf width)
    (#_Move (- width) 1)
    (decf width)
    (when (minusp width) (return))))

;; total hacks, but HiWord and LoWord appear not to be in Carbon,
;;  although the documentation says they're supposed to be.
(defun %hi-word (int)
  #+carbon-compat (ash int -16)
  #-carbon-compat (#_HiWord int))

(defun %lo-word (int)
  #+carbon-compat (mask-field (byte 16 0) int)
  #-carbon-compat (#_LoWord int))

(defun select-item-from-pup (item-list                        
                             &key
                             (where (%stack-block ((p 4))
                                      (#_GetMouse p)
                                      (#_LocalToGlobal p)
                                      (%get-point p)))
                             (default-item 0)
                             (item-checked-p nil)
                             (item-disabled-p nil)
                             (other-p nil)
                             (item-to-string-fn 'princ-to-string)
                             (hier-p-fn nil)
                             (hier-items-fn nil)
                             (hier-parent-fn nil)
                             (hier-select-p t))
  (let ((sub-id-cnt 0)
        (m-alist nil))
    (with-pstrs ((danQuayle " "))
      (labels ((new-id (sub-menu-p)
                       (let ((id (if sub-menu-p
                                   (if (< sub-id-cnt 235)
                                     (incf sub-id-cnt)
                                     (error "no available sub-menu-id's (range 1-235)"))
                                   (#_UniqueID "MENU"))))
                         (if (%null-ptr-p (#_GetMenuHandle id)) id (new-id sub-menu-p))))
               (make-menu (item-list sub-menu-p)
                          (let ((menu-id (new-id sub-menu-p)))
                            (with-macptrs ((menu_h (#_NewMenu menu-id danQuayle)))
                              (let ((i 1))
                                (dolist (item item-list)
                                  (let ((hier-p (and hier-p-fn (funcall hier-p-fn item))))
                                    (with-pstrs ((title_p (funcall item-to-string-fn (if hier-p (funcall hier-parent-fn item) item))))
                                      (#_AppendMenu menu_h danQuayle)
                                      (#_SetMenuItemText menu_h i title_p)
                                      (when (and (not hier-p)
                                                 item-checked-p
                                                 (funcall item-checked-p item)) 
                                        #+carbon-compat
                                        (#_CheckMenuItem menu_h i t)
                                        #-carbon-compat
                                        (#_CheckItem menu_h i t))
                                      (when (and (not hier-p)
                                                 item-disabled-p
                                                 (funcall item-disabled-p item)) 
                                        #+carbon-compat
                                        (#_DisableMenuItem menu_h i)
                                        #-carbon-compat
                                        (#_DisableItem menu_h i))
                                      (when hier-p
                                        (#_SetItemCmd  menu_h i (code-char #$hMenuCmd))
                                        (#_SetItemMark menu_h i (code-char (make-menu (funcall hier-items-fn item) t))))))
                                  (#_InsertMenu menu_h #$hierMenu)
                                  (incf i))
                                (setf m-alist (acons menu-id item-list m-alist))
                                menu-id)))))
        (declare (dynamic-extent #'new-id #'make-menu))
        
        (unwind-protect
          (with-macptrs ((menu_h (#_GetMenuHandle (make-menu item-list nil))))
            
            (when other-p (with-pstrs ((dashes "(-")
                                       (title "OtherÉ"))
                            (#_AppendMenu menu_h dashes)
                            (#_AppendMenu menu_h title)))
            
            ;;FlushEvents is needed in case we're not called from event processing
            ;; (e.g. some one tries us out by typing into the listener)
            (unless (and (boundp '*current-event*) *current-event*)
              (#_FlushEvents #$mDownMask 0))
            
            #+DEBUG
            (if (and (boundp '*current-event*)  *current-event*)
              (spy (rref *current-event* eventrecord.what)))
            
            (let ((sel-code (with-cursor *arrow-cursor*
                              (#_PopUpMenuSelect menu_h (point-v where) (point-h where) (1+ default-item)))))
              (when (and (zerop sel-code) hier-select-p) (setf sel-code (#_MenuChoice)))
              (let* ((sel-menu-id   (%hi-word sel-code))
                     (sel-item-no   (%lo-word sel-code))
                     (sel-item-list (rest (assoc sel-menu-id m-alist))))
                (cond ((zerop sel-item-no) (values nil nil))
                      ((<= sel-item-no (length sel-item-list))
                       (let ((sel-item (elt sel-item-list (1- sel-item-no))))
                         (values (if (and hier-p-fn (funcall hier-p-fn sel-item))
                                   (funcall hier-parent-fn sel-item)
                                   sel-item)
                                 (1- sel-item-no))))
                      ((= sel-item-no (+ 2 (length sel-item-list))) (values :other nil))
                      (t (values nil nil))))))
          
          (dolist (i m-alist)
            (#_DeleteMenu (first i))
            (#_DisposeMenu (#_GetMenuHandle (first i)))))))))

#+Old
;; Gary was here, 20020829
;; changed the interface to checked-items
(defun select-item-from-pup (item-list                        
                             &key
                             (where (%stack-block ((p 4))
                                      (#_GetMouse p)
                                      (#_LocalToGlobal p)
                                      (%get-point p)))
                             (default-item 0)
                             (checked-items nil)
                             (other-p nil)
                             (test 'eql)
                             (item-to-string-fn 'princ-to-string)
                             (hier-p-fn nil)
                             (hier-items-fn nil)
                             (hier-parent-fn nil)
                             (hier-select-p t))
  (let ((sub-id-cnt 0)
        (m-alist nil))
    (with-pstrs ((danQuayle " "))
      (labels ((new-id (sub-menu-p)
                       (let ((id (if sub-menu-p
                                   (if (< sub-id-cnt 235)
                                     (incf sub-id-cnt)
                                     (error "no available sub-menu-id's (range 1-235)"))
                                   (#_UniqueID "MENU"))))
                         (if (%null-ptr-p (#_GetMenuHandle id)) id (new-id sub-menu-p))))
               (make-menu (item-list sub-menu-p)
                          (let ((menu-id (new-id sub-menu-p)))
                            (with-macptrs ((menu_h (#_NewMenu menu-id danQuayle)))
                              (let ((i 1))
                                (dolist (item item-list)
                                  (let ((hier-p (and hier-p-fn (funcall hier-p-fn item))))
                                    (with-pstrs ((title_p (funcall item-to-string-fn (if hier-p (funcall hier-parent-fn item) item))))
                                      (#_AppendMenu menu_h danQuayle)
                                      (#_SetMenuItemText menu_h i title_p)
                                      (when (find item checked-items :test test) 
                                        #+carbon-compat
                                        (#_CheckMenuItem menu_h i t)
                                        #-carbon-compat
                                        (#_CheckItem menu_h i t))
                                      (when hier-p
                                        (#_SetItemCmd  menu_h i (code-char #$hMenuCmd))
                                        (#_SetItemMark menu_h i (code-char (make-menu (funcall hier-items-fn item) t))))))
                                  (#_InsertMenu menu_h #$hierMenu)
                                  (incf i))
                                (setf m-alist (acons menu-id item-list m-alist))
                                menu-id)))))
        (declare (dynamic-extent #'new-id #'make-menu))
        
        (unwind-protect
          (with-macptrs ((menu_h (#_GetMenuHandle (make-menu item-list nil))))
            
            (when other-p (with-pstrs ((dashes "(-")
                                       (title "OtherÉ"))
                            (#_AppendMenu menu_h dashes)
                            (#_AppendMenu menu_h title)))
            
            ;;FlushEvents is needed in case we're not called from event processing
            ;; (e.g. some one tries us out by typing into the listener)
            (unless (and (boundp '*current-event*) *current-event*)
              (#_FlushEvents #$mDownMask 0))
            
            #+DEBUG
            (if (and (boundp '*current-event*)  *current-event*)
              (spy (rref *current-event* eventrecord.what)))
            
            (let ((sel-code (with-cursor *arrow-cursor*
                              (#_PopUpMenuSelect menu_h (point-v where) (point-h where) (1+ default-item)))))
              (when (and (zerop sel-code) hier-select-p) (setf sel-code (#_MenuChoice)))
              (let* ((sel-menu-id   (%hi-word sel-code))
                     (sel-item-no   (%lo-word sel-code))
                     (sel-item-list (rest (assoc sel-menu-id m-alist))))
                (cond ((zerop sel-item-no) (values nil nil))
                      ((<= sel-item-no (length sel-item-list))
                       (let ((sel-item (elt sel-item-list (1- sel-item-no))))
                         (values (if (and hier-p-fn (funcall hier-p-fn sel-item))
                                   (funcall hier-parent-fn sel-item)
                                   sel-item)
                                 (1- sel-item-no))))
                      ((= sel-item-no (+ 2 (length sel-item-list))) (values :other nil))
                      (t (values nil nil))))))
          
          (dolist (i m-alist)
            (#_DeleteMenu (first i))
            (#_DisposeMenu (#_GetMenuHandle (first i)))))))))

;;; ---------------------------------------------------------------------------

(defun menu-select-action (choices &rest args)
  ;; Make sure the menu selection code does not think the mouse has been pressed...
  (let ((selection
         (apply #'select-item-from-pup
                choices
                :hier-p-fn 'listp
                :hier-items-fn 'rest
                :hier-parent-fn 'first
                :item-to-string-fn #'(lambda (i) (princ-to-string (svref i 0)))
                args)))
    (if (eq selection :other)
      nil
      (rest (coerce selection 'list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|


;simple example
(loop
  (when (mouse-down-p) 
    (without-interrupts 
     (return (select-item-from-pup '(1 2 3 4) :other-p t :default-item 1 :checked-items '(1 3))))))

(loop
  (when (mouse-down-p) 
    (without-interrupts 
     (return (select-item-from-pup '(1 2 3 4) 
                                   :other-p t
                                   :default-item 1
                                   :item-checked-p (lambda (i) (oddp i)))))))

;illustrates :item-to-string-fn
(loop
  (when (mouse-down-p) 
    (without-interrupts 
     (return (select-item-from-pup '((11111 :item1) (22222 :item2) (55555 :item3))
                                   :item-to-string-fn #'(lambda (i) (princ-to-string (second i))))))))


;;example of a hierarchical menu using a simple tree structure
;; it uses nested lists to represent sub-hierarchy the 1st element being the
;; parent and the rest being the children.
(loop
  (when (mouse-down-p) 
    (without-interrupts 
     (return (select-item-from-pup '(1 2 (3 31 32 33) 4)
                                   :hier-p-fn 'listp
                                   :hier-items-fn 'rest
                                   :hier-parent-fn 'first)))))

(loop
  (when (mouse-down-p) 
    (without-interrupts 
     (return (select-item-from-pup '(#(1 "One") #(2 "Two") (#(3 "Three") #(31 "Three One") #(32 "Three Two")) #(4 "Four"))
                                   :hier-p-fn 'listp
                                   :hier-items-fn 'rest
                                   :hier-parent-fn 'first
                                   :item-to-string-fn #'(lambda (i) (princ-to-string (svref i 1))))))))

|#