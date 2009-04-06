;;; Copyright (c) 2007-2009, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :bk-tree)

(defclass bk-tree ()
  ((distance
    :initform 0
    :initarg :distance
    :type unsigned-byte
    :accessor distance-of
    :documentation "Metric distance between current node and its parent.")
   (value
    :initform nil
    :initarg :value
    :accessor value-of)
   (nodes
    :initform nil
    :initarg nodes
    :type list
    :accessor nodes-of
    :documentation "Nodes collected under this node.")))

(defmethod print-object ((self bk-tree) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":DISTANCE ~D :VALUE ~S :NODES ~S"
            (distance-of self)
            (value-of self)
            (mapcar #'value-of (nodes-of self)))))

(defclass search-result ()
  ((distance
    :initarg :distance
    :type unsigned-byte
    :accessor distance-of)
   (value
    :initarg :value
    :accessor value-of)))

(defmethod print-object ((self search-result) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":DISTANCE ~D :VALUE ~S"
            (distance-of self)
            (value-of self))))

(define-condition duplicate-value (error)
  ((value
    :initarg :value
    :accessor value-of))
  (:documentation "Signaled upon every duplicated entry insertion."))

(defun insert-value (value tree &key (metric #'levenshtein))
  "Inserts given VALUE into supplied TREE."
  (if (null (value-of tree))
      ;; If tree has no value yet, insert value here.
      (setf (value-of tree) value)
      ;; Otherwise, look for a suitable place.
      (let ((distance (funcall metric value (value-of tree))))
        ;; Check if we try to duplicate an existing entry.
        (if (zerop distance)
            (error 'duplicate-value :value value))
        ;; Find an appropriate place to insert the value.
        (let ((sub-tree
               (find distance (nodes-of tree) :test #'= :key #'distance-of)))
          (if sub-tree
              (insert-value value sub-tree :metric metric)
              (push (make-instance 'bk-tree :distance distance :value value)
                    (nodes-of tree)))))))

(defun search-value (value tree search-exhausted submit-result
                     &key (threshold 1) (metric #'levenshtein))
  "Searches given VALUE in the supplied TREE. Calls SUBMIT-RESULT function
during every found result satisfying supplied THRESHOLD. Finalizes search via
calling SEARCH-EXHAUSTED when THRESHOLD could not be satisfied anymore."
  (let ((distance (funcall metric value (value-of tree))))
    ;; Check if there is any sub-trees available to search.
    (unless (endp (nodes-of tree))
      (loop for (sub-distance . sub-tree) in
            ;; Sort available sub-trees collected under this
            ;; tree, according to absolute difference between
            ;; DISTANCE and SUB-DISTANCE.
            (sort (mapcar
                    (lambda (sub-tree)
                      ;; (SUB-DISTANCE . SUB-TREE) pairs.
                      (cons (abs (- distance (distance-of sub-tree)))
                            sub-tree))
                    (nodes-of tree))
                  #'<
                  :key #'car)
            ;; Scan sub-trees.
            while (<= sub-distance threshold)
            do (block sub-tree-search
                 (flet ((sub-search-exhausted ()
                          (return-from sub-tree-search)))
                   (search-value value
                                 sub-tree
                                 #'sub-search-exhausted
                                 submit-result
                                 :threshold threshold
                                 :metric metric)))))
    ;; After scanning sub-trees, if appropriate, submit this one.
    (if (<= distance threshold)
        ;; Submit current node.
        (funcall submit-result distance (value-of tree))
        ;; If threshold limit is not reached, then search is exhausted.
        (funcall search-exhausted))))

(defun collect-search-results (value tree &rest keys &key (limit 50) &allow-other-keys)
  "Convenient function to search supplied VALUE in the specified TREE. Function
returns a list of SEARCH-RESULT objects."
  (let ((count 0)
        results)
    (labels ((search-exhausted ()
               "Return from the outmost stack with collected results so far."
               (return-from collect-search-results (nreverse results)))
             (submit-result (distance value)
               "Submit a found result."
               (push
                (make-instance 'search-result :distance distance :value value)
                results)
               (incf count)
               (if (not (< count limit))
                   (search-exhausted))))
      (apply #'search-value
             value tree #'search-exhausted #'submit-result
             :allow-other-keys t keys)
      (search-exhausted))))

(defun print-tree (tree &key (stream *standard-output*) (depth 0))
  "Prints supplied BK-TREE in a human-readable format."
  ;; Print current value first.
  (format stream "~&")
  (loop repeat depth do (write-char #\space stream))
  (format stream "-> (~d) ~a" (distance-of tree) (value-of tree))
  ;; Iterate across sub-trees, according to their distances from root
  ;; node.
  (mapc
   (lambda (tree) (print-tree tree :stream stream :depth (+ 4 depth)))
   (sort (copy-list (nodes-of tree)) #'< :key #'distance-of)))

(defun maximum-depth (tree)
  "Returns maximum depth of the TREE."
  (labels ((scan (tree depth)
             (max depth
                  (loop for node in (nodes-of tree)
                        maximize (scan node (1+ depth))))))
    (scan tree 0)))

(defun average-children-count (tree)
  "Returns average children count per node of the supplied TREE."
  (let ((n-children 0)
        (n-node 0))
    (labels ((scan (tree)
               ;; Count children, unless this is a leaf node.
               (unless (null (nodes-of tree))
                 (incf n-node)
                 (incf n-children (length (nodes-of tree)))
                 (mapc #'scan (nodes-of tree)))))
      (scan tree))
    (if (zerop n-node) 0 (/ n-children n-node))))
