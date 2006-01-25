;;; ---------------------------------------------------------------------------
;;; selection and such
;;; ---------------------------------------------------------------------------

(defclass* basic-selection-manager (has-manager-mixin)
  "Root class for any class that manages selections."
  ((selected-items nil ir)
   (previous-selected-items nil ir)
   (selected-items-class nil ir))
  (:export-p t)
  (:export-slots selected-items)
  #+COPYING :copy-set-slots 
  (:default-initargs
    :selected-items-class 'list-container))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object basic-selection-manager) &key)
  (setf (slot-value object 'selected-items)
        (make-instance (selected-items-class object)))
  (setf (slot-value object 'previous-selected-items)
        (make-instance (selected-items-class object))))

;;; ---------------------------------------------------------------------------

(defclass* basic-selectable-thing (set-item-mixin)
  "Something that can be selected somehow."
  ((selected? nil r)
   (selected-func #'identity iar)
   (unselected-func #'identity iar)
   (selection-manager nil ir)
   (selectable? nil ir))
  #+COPYING (:copy-set-slots selection-manager)
  (:export-p t)
  #+COPYING (:copy-slots selected? selected-func unselected-func selectable?)
  (:export-slots selectable? selected? selected-func unselected-func))

;;; ---------------------------------------------------------------------------

(defgeneric begin-new-selection (selection-manager)
  (:documentation "")
  (:method ((manager basic-selection-manager))
           (empty! (previous-selected-items manager))
           (iterate-container
            (selected-items manager)
            (lambda (item) 
              (insert-item (previous-selected-items manager) item)))))

;;; ---------------------------------------------------------------------------

(defgeneric end-new-selection (selection-manager)
  (:documentation "")
  (:method ((manager basic-selection-manager))
           (values)))            

;;; ---------------------------------------------------------------------------

(defgeneric unselect-item (manager item)
  (:documentation "Called when the item in unselected")
  
  (:method ((manager t) (item t)) )
  
  (:method ((manager basic-selection-manager) (item basic-selectable-thing))
           (delete-item (selected-items manager) item)
           (nilf (slot-value item 'selected?))
           (funcall (unselected-func item) item)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object basic-has-manager-mixin) 
                                       &key &allow-other-keys)
  ;; just keeping things loose
  )

;;; ---------------------------------------------------------------------------

(defclass* selection-manager-mixin ()
  "A mixin for classes that need to manage selections."
  ((selection-manager nil r)
   (selection-manager-class nil ir))
  (:default-initargs
    :selection-manager-class 'basic-selection-manager)
  (:export-slots selection-manager selection-manager-class) 
  (:export-p t))

;;; ---------------------------------------------------------------------------

(defgeneric select-item (manager item)
  (:documentation "")
  
  (:method ((manager t) (item t)) )
  
  (:method ((manager selection-manager-mixin) (item t))
           (select-item (selection-manager manager) item)) 
  
  (:method ((manager basic-selection-manager) (item basic-selectable-thing))
           (insert-new-item (selected-items manager) item)
           (tf (slot-value item 'selected?))
           (funcall (selected-func item) item)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object selection-manager-mixin) &key)
  (setf (slot-value object 'selection-manager)
        (make-instance (selection-manager-class object) :owner object)))

;;; ---------------------------------------------------------------------------

(delegates-to (selection-manager-mixin basic-selection-manager selection-manager)
  (selected-items basic-selection-manager)
  (empty-selections! basic-selection-manager)
  (can-extend-selection-p basic-selection-manager)
  (can-add-to-selection-p basic-selection-manager basic-selectable-thing))

;;; ---------------------------------------------------------------------------

(defmethod insert-item :after ((manager selection-manager-mixin)
                               (item basic-selectable-thing))
  (setf (slot-value item 'selection-manager) (selection-manager manager)))

;;; ---------------------------------------------------------------------------

(defmethod delete-item :after ((manager selection-manager-mixin) 
                               (item basic-selectable-thing))
  (unselect-item (selection-manager manager) item)
  (nilf (slot-value item 'selection-manager)))

;;; ---------------------------------------------------------------------------

(defgeneric selected-items (selection-manager)
  (:documentation "Returns a list of the selected items in the manager. See 
also map-selected-items."))

;;; ---------------------------------------------------------------------------

(defgeneric map-selected-items (selection-manager function)
  (:documentation "Applies function to each selected item in the manager.")
  (:method ((manager basic-selection-manager) function)
           ;; Gary Warren King 2003-08-23: may be expensive if selected-items
           ;; must cons up a new list
           (iterate-container (selected-items manager) function)))

;;; ---------------------------------------------------------------------------

(defgeneric empty-selections! (selection-manager)
  (:documentation "")
  (:method ((manager basic-selection-manager))
           (unselect-all manager)))

;;; ---------------------------------------------------------------------------

(defgeneric can-extend-selection-p (selection-manager)
  (:documentation "Returns true if more items can be added to the selection manager."))

;;; ---------------------------------------------------------------------------

(defgeneric can-add-to-selection-p (selection-manager item)
  (:documentation "Returns true if the selected item can currently be added to
the selection manager."))

;;; ---------------------------------------------------------------------------

(defgeneric item-selected-p (selection-manager item)
  (:documentation "Returns true if the item is selected by the selection manager."))

;;; ---------------------------------------------------------------------------

(defgeneric anything-selected-p (selection-manager)
  (:documentation "Returns true if anything is currently selected."))

;;; ---------------------------------------------------------------------------

(defgeneric first-selected-item (selection-manager)
  (:documentation "Returns the first item in the selection manager, if any."))

;;; ---------------------------------------------------------------------------

;; This is probably going to cause problems as unselect-item deletes the 
;; item from the container.
(defgeneric unselect-all (manager)
  (:documentation "Unselects all items in the selection-manager.")
  
  (:method ((manager basic-selection-manager))
           (map-selected-items manager (lambda (item)
                                         (unselect-item manager item)))))

;;; ---------------------------------------------------------------------------

(defmethod item-selected-p ((manager basic-selection-manager)
                            (item basic-selectable-thing))
  (selected? item))

;;; ---------------------------------------------------------------------------

(defmethod anything-selected-p ((manager basic-selection-manager))
  (not (empty-p (selected-items manager))))

;;; ---------------------------------------------------------------------------

(defmethod first-selected-item ((manager basic-selection-manager))
  (first-item (selected-items manager)))

;;; ---------------------------------------------------------------------------

(defmethod can-extend-selection-p ((manager basic-selection-manager))
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod can-add-to-selection-p ((manager basic-selection-manager) 
                                   (item basic-selectable-thing))
  (values t))

;;; ---------------------------------------------------------------------------

(defmethod clean-up-view progn ((view selection-manager-mixin))
  (empty-selections! view))

;;; ---------------------------------------------------------------------------

(defmethod remove-view-from-window :after ((view selection-manager-mixin))
  (empty-selections! view))


