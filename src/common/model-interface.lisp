(in-package :common)

(defclass model-interface ()
  ()
  (:documentation "Interface for objects that can store Cupertino data."))

(defgeneric model-project-type (obj)
  (:documentation "Get the project type of OBJ."))

(defgeneric (setf model-project-type) (new-value obj)
  (:documentation "Set the project type of OBJ to NEW-VALUE."))

(defgeneric model-project-path (obj)
  (:documentation "Get the project file path of OBJ."))

(defgeneric (setf model-project-path) (new-value obj)
  (:documentation "Set the project file path of OBJ to NEW-VALUE."))

(defgeneric model-sim (obj)
  (:documentation "Get the simulator device of OBJ."))

(defgeneric (setf model-sim) (new-value obj)
  (:documentation "Set the simulator device of OBJ to NEW-VALUE."))

(defgeneric model-device (obj)
  (:documentation "Get the device of OBJ."))

(defgeneric (setf model-device) (new-value obj)
  (:documentation "Set the device of OBJ to NEW-VALUE."))

(defgeneric model-scheme (obj)
  (:documentation "Get the scheme of OBJ."))

(defgeneric (setf model-scheme) (new-value obj)
  (:documentation "Set the scheme of OBJ to NEW-VALUE."))

(defgeneric model-test-scheme (obj)
  (:documentation "Get the test scheme of OBJ."))

(defgeneric (setf model-test-scheme) (new-value obj)
  (:documentation "Set the test scheme of OBJ to NEW-VALUE."))