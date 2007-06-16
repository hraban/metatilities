(in-package #:metatilities)

(defgeneric choose-directory-question* (interface &rest args)
  (:documentation ""))

(defgeneric choose-file-question* (interface &rest args)
  (:documentation ""))

(defgeneric choose-item-from-pup* (interface the-list &rest args &key &allow-other-keys)
  (:documentation ""))

(defgeneric choose-item-question* (interface list &rest args &key title &allow-other-keys)
  (:documentation ""))

(defgeneric choose-new-file-question* (interface &rest args)
  (:documentation ""))

(defgeneric gui-error* (interface condition &optional prefix standard-message)
  (:documentation ""))

(defgeneric gui-warn* (interface string &rest args &key ok-text title size &allow-other-keys)
  (:documentation ""))

(defgeneric help-spec (view)
  (:documentation ""))

(defgeneric interface-beep* (interface &rest args)
  (:documentation ""))

(defgeneric make-color** (interface red green blue)
  (:documentation ""))

(defgeneric make-gray* (interface level)
  (:documentation ""))

(defgeneric make-scaled-color* (interface red green blue scale)
  (:documentation ""))

(defgeneric name (x)
  (:documentation ""))

(defgeneric process-parameters* (interface &rest args)
  (:documentation ""))

(defgeneric prompt-for* (interface type prompt &rest args &key &allow-other-keys)
  (:documentation ""))

(defgeneric put-item-on-clipboard* (interface thing)
  (:documentation ""))

(defgeneric select-instrument* (interface instrument &rest args)
  (:documentation ""))

(defgeneric sound-note* (interface pitch velocity &rest args)
  (:documentation ""))

(defgeneric stop-notes* (interface)
  (:documentation ""))

