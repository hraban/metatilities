(in-package #:metatilities)

;;; not perfect but not too bad.
;;; needs to strip out defaults from &optional and &key arguments


(defun gf-info (symbol)
  (let ((gf (symbol-function symbol)))
    (list 
     :declarations (ccl:generic-function-declarations gf)
     :lambda-list (ccl:generic-function-lambda-list gf)
     :method-class (ccl:generic-function-method-class gf)
     :method-combination (ccl:generic-function-method-combination  gf)
     :methods (ccl:generic-function-methods  gf) 
     :name (ccl:generic-function-name  gf))))

;;; ---------------------------------------------------------------------------

(defun write-gf-template (symbol stream)
  (let ((info (gf-info symbol)))
    (format stream "\(defgeneric ~(~A~) " (getf info :name))
    (format stream "~(~A~)" (getf info :lambda-list))
    (when (and (getf info :method-combination)
               (not (eq (ccl::method-combination-name (getf info :method-combination)) 
                        'standard)))
      (format stream "~%  \(:method-combination ~(~A~)\)" (getf info :method-combination)))
    (if (documentation symbol 'function)
      (format stream "~%  \(:documentation ~S\)\)" (documentation symbol 'function))
      (format stream "~%  \(:documentation \"\"\)\)~%")))
  (terpri stream)
  (values))

;;; ---------------------------------------------------------------------------

(defun find-symbol-function (symbol-name)
  (iterate-elements
   (list-all-packages)
   (lambda (package)
     (when (aand (find-symbol symbol-name package)
                 (ignore-errors (symbol-function it))
                 (typep it 'standard-generic-function))
       (return-from find-symbol-function (find-symbol symbol-name package))))))

;;; ---------------------------------------------------------------------------

(defun build-defgenerics-for-undocumented-methods (package file)
  (let* ((p (make-container 'package-container :packages package))
         (ms
          (sort
           (append
            (collect-elements 
             p
             :filter (lambda (symbol)
                       (and (fboundp symbol)
                            (typep (symbol-function symbol) 'standard-generic-function)
                            (some (lambda (m)
                                    (not (or (reader-method-p m)
                                             (writer-method-p m))))
                                  (mopu:generic-function-methods (symbol-function symbol)))
                            (not (documentation symbol 'function)))))
            (collect-elements 
             p
             :filter (lambda (base-symbol)
                       (let ((symbol (list 'setf base-symbol)))
                         (and (fboundp symbol)
                              (typep (symbol-function symbol) 'standard-generic-function)
                              (some (lambda (m)
                                      (not (or (reader-method-p m)
                                               (writer-method-p m))))
                                    (mopu:generic-function-methods (symbol-function symbol)))
                              (not (documentation symbol 'function)))))
             :transform (lambda (base-symbol)
                          (list 'setf base-symbol))))
           (lambda (a b)
             (cond ((and (consp a) (consp b))
                    (string-lessp (second a) (second b)))
                   ((and (consp a) (not (consp b)))
                    1)
                   ((and (not (consp a)) (consp b))
                    -1)
                   (t
                    (string-lessp a b)))))))
    
    (when ms
      (princ "Creating file...")
      (with-new-file (out file)
        (iterate-elements 
         ms
         (lambda (symbol)
           (write-gf-template symbol out)))))))
         
;;; ---------------------------------------------------------------------------

#+Test
(delete-file "user-home:darcs;lift-generics.lisp")

#+Test
(metatilities::build-defgenerics-for-undocumented-methods
 :cl-containers "user-home:darcs;cl-containers-generics.lisp") 

#+Test
(with-new-file (out "user-home:darcs;generics.lisp")
  (let ((package nil))
    (iterate-elements 
     (collect-elements 
      (make-iterator "
;;; moptilities

SUBCLASSP
COPY-TEMPLATE

;;; metatilities.generic-lisp

IS-INTERFACE-AVAILABLE-P
QUIT-LISP*
DEFCLASS*-SUPERCLASSES
   (SETF DEFCLASS*-SUPERCLASSES)
RESET
MACINTOSH-PATH->UNIX
INTERFACE-BEEP*
INTERFACE-BEEP
GUI-ERROR*
GUI-WARN*
MAKE-COLOR*
MAKE-GRAY*
MAKE-GRAY
MAKE-SCALED-COLOR*
MAKE-SCALED-COLOR
CHOOSE-FILE-QUESTION*
   CHOOSE-NEW-FILE-QUESTION*
   CHOOSE-DIRECTORY-QUESTION*
CHOOSE-ITEM-QUESTION*
CHOOSE-ITEM-FROM-PUP*
MAKE-UI-POINT*
PROCESS-PARAMETERS*
PROCESS-PARAMETERS
PUT-ITEM-ON-CLIPBOARD*
PUT-ITEM-ON-CLIPBOARD
INSPECT-THING*
INSPECT-THING
SOUND-NOTE*
SOUND-NOTE
STOP-NOTES*
SELECT-INSTRUMENT*
SELECT-INSTRUMENT
PROMPT-FOR*
PROMPT-FOR
SHELL-COMMAND*
SHELL-COMMAND
   INCLUDE-CLASS-DEPENDENCIES

;;; dynamic-classes

EXISTING-SUBCLASS

;;; containers

(SETF ITEM-AT)
MAKE-INITIAL-ELEMENT
ITERATE-CONTAINER
COLLECT-ITEMS
MAKE-NODE-FOR-CONTAINER
COLLECT-ELEMENTS
ITERATE-NODES
SIZE
TOTAL-SIZE
   MAKE-CONTAINER-FOR-CONTENTS
ITERATE-ELEMENTS
FIND-ELEMENT
DELETE-ITEM
DELETE-ELEMENT
PRINT-CONTAINER
CONTAINER->LIST
NTH-ELEMENT
COLLECT-NODES
SEARCH-FOR-ITEM
SEARCH-FOR-ELEMENT
SEARCH-FOR-NODE
SEARCH-FOR-MATCH
SEARCH-FOR-MATCHING-NODE
SEARCH-FOR-NODE*
BEST-ITEM
BEST-NODE
BEST-ELEMENT
ARGMAX
ARGMIN
REDUCE-CONTAINER
REDUCE-ELEMENTS
REDUCE-NODES
DELETE-ITEM-IF
FIRST-ELEMENT
(SETF FIRST-ELEMENT)
DELETE-LIST
INSERT-LIST
INSERT-SEQUENCE
INSERT-NEW-ITEM
SORT-ELEMENTS
EMPTY!
FIND-ITEM
SOME-ITEM-P
EVERY-ITEM-P
SOME-ELEMENT-P
EVERY-ELEMENT-P
INSERT-ITEM
APPEND-ITEM
APPEND-NEW-ITEM
ITERATE-KEYS
COLLECT-KEY-VALUE
REVERSE-FIND
FIND-VALUE
ITEM-AT!
ADD-DEFAULT-ITEM
ITEM-AT
ITERATE-ELEMENTS-STABLY
ITERATE-KEY-VALUE
COLLECT-KEYS
ITERATE-KEY-VALUE-STABLY
COLLECT-KEY-VALUE-STABLY
COLLECT-ELEMENTS-STABLY
CONTAINER-DIMENSION
DIMENSIONS
CONTAINER-DIFFERENCE
ADD-INITIAL-CONTENTS
ELEMENT-POSITION
REVERSE-CONTAINER
SOME-KEY-VALUE-P
EVERY-KEY-VALUE-P
SEARCH-FOR-KEY
REMOVE-ITEMS-IF
COUNT-ITEMS
COUNT-ELEMENTS
COUNT-ELEMENTS-IF
PRINT-CONTAINER-SUMMARY
ITEM-AT-1
ITEM-AT-1!
(SETF ITEM-AT-1)
DELETE-ITEM-AT
INITIALIZE-CONTAINER
SORT-KEYS
SORT-CONTAINER
ITEM-KEY
NEXT-ELEMENT
CURRENT-ELEMENT-P
MOVE-FORWARD
ITERATE-FORWARD
   MOVE-FORWARD-TO-NEXT-ELEMENT
ERROR-IF-QUEUE-EMPTY
DELETE-NODE
PUSH-ITEM
ITERATE-CHILDREN
HAS-CHILDREN-P
FIND-CHILD-NODE
INORDER-WALK
PREORDER-WALK
POSTORDER-WALK
INORDER-WALK-NODES
PREORDER-WALK-NODES
POSTORDER-WALK-NODES
WALK-TREE
WALK-TREE-NODES
ROTATE-LEFT
ROTATE-RIGHT
RB-DELETE-FIXUP
HEIGHT
UPDATE-ELEMENT
SET-DIRTY-FLAG
CLEAN-UP
INSERT-ITEM-AFTER
INSERT-ITEM-BEFORE
DELETE-ITEM-AFTER
DELETE-ITEM-BEFORE
REPLACE-ITEM
ITERATE-NODES-ABOUT-NODE
   INSERT-ITEM-ORDERED-ABOUT-NODE
INSERT-ITEM-ORDERED
LEFT-NODE-FOR-ITEM
RIGHT-NODE-FOR-ITEM
   LEFT-AND-RIGHT-NODES-FOR-ITEM
ITERATE-LEFT-NODES
ITERATE-RIGHT-NODES
ITERATE-LEFT
ITERATE-RIGHT
SORT-UPDATE-LEFT
SORT-UPDATE-RIGHT
UPDATE-ITEM
INCREMENT-END
CURRENT-ITEM
INSERT-ITEM-AT
L-CHILD
R-CHILD
HEAP-NODE-PARENT
L-CHILD-INDEX
R-CHILD-INDEX
NODE-PARENT-INDEX
EXCHANGE-HEAP-NODES
HEAPIFY
BIGGEST-ITEM
DELETE-BIGGEST-ITEM
MAKE-SET
GRAFT-NODES
FIND-SET
LINK-NODES
REPRESENTATIVE
REPRESENTATIVE-NODE
(SETF PACKAGES)
PRINT-CONTAINER-CONTENTS

;; cl-mathstats

MAKE-STATISTIC
CONVERT
STATISTICP
COMPOSITE-STATISTIC-P
SIMPLE-STATISTIC-P
FIND-SYSTEM-FOR-MAPPING*
UNIQUIFY-FILE-NAME
MAP-FORMS-IN-FILE
MAP-LINES-IN-FILE
SHORTEN-FILENAME-FOR-OS
REALLOCATE-INSTANCE
HELP-SPEC
VIEW-X/VIEW-Y->X/Y
VIEW-SCALE
CROSS-PRODUCT

;;; cl-graph

MAKE-VERTEX-FOR-GRAPH
TAG-ALL-EDGES
UNTAG-ALL-EDGES
UNTAG-EDGES
TAG-EDGES
REPLACE-VERTEX
ADD-EDGE-TO-VERTEX
SOURCE-EDGES
TARGET-EDGES
CHILD-VERTEXES
PARENT-VERTEXES
NEIGHBOR-VERTEXES
NUMBER-OF-NEIGHBORS
IN-CYCLE-P
ITERATE-VERTEXES
EDGES
VERTEX-COUNT
VERTEXES
SOURCE-EDGE-COUNT
TARGET-EDGE-COUNT
GRAPH-ROOTS
ROOTP
FIND-VERTEX-IF
FIND-EDGE-IF
FIND-EDGES-IF
FIND-VERTEXES-IF
FORCE-UNDIRECTED
TRAVERSE-ELEMENTS
TRAVERSE-ELEMENTS-HELPER
ANY-UNDIRECTED-CYCLE-P
COMPLETE-LINKS
SUBGRAPH-CONTAINING
EDGE-COUNT
TOPOLOGICAL-SORT
ASSIGN-LEVEL
DEPTH
   MAKE-VERTEX-EDGES-CONTAINER
OTHER-VERTEX
   FIND-EDGE-BETWEEN-VERTEXES-IF
VERTICES-SHARE-EDGE-P
   GRAPH-EDGE-MIXTURE-MATRIX
GRAPH-MIXING-MATRIX
UNIQUE-ELEMENTS
UNIQUE-NODES
INITIALIZE-VERTEX-DATA
BREADTH-FIRST-VISITOR
   BREADTH-FIRST-SEARCH-GRAPH
CONNECTED-COMPONENTS
   CONNECTED-COMPONENT-COUNT
   FIND-CONNECTED-COMPONENTS
MST-FIND-SET
MST-MAKE-SET
MST-TREE-UNION
MST-LINK
ADD-EDGES-TO-GRAPH
MAKE-GRAPH-FROM-VERTEXES
EDGE-LESSP-BY-WEIGHT
MINIMUM-SPANNING-TREE
CONNECTED-GRAPH-P
EDGE-LESSP-BY-DIRECTION
OUT-EDGE-FOR-VERTEX-P
DFS
DFS-VISIT
DFS-TREE-EDGE-P
DFS-BACK-EDGE-P
DFS-FORWARD-EDGE-P
DFS-CROSS-EDGE-P
DFS-EDGE-TYPE
   MAP-OVER-ALL-COMBINATIONS-OF-K-VERTEXES
   MAP-OVER-ALL-COMBINATIONS-OF-K-EDGES
GRAPH->DOT-PROPERTIES
VERTEX->DOT
EDGE->DOT
"
                     :treat-contents-as :lines)
      :filter (lambda (string)
                (and (plusp (size string))
                     (not (char-equal (aref string 0) #\;))
                     (not (char-equal (aref (string-left-trim '(#\ ) string) 0) #\())))
      :transform (lambda (string)
                   (string-left-trim '(#\ ) string)))
     (lambda (string)
       (let ((symbol (find-symbol-function string)))
         (unless (eq (symbol-package symbol) package)
           (format out "~&;;; ~A~%~%" (package-name (symbol-package symbol)))
           (setf package (symbol-package symbol)))
         (write-gf-template symbol out))))))
