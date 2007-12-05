;;; Copyright (c) 2007, Volkan YAZICI <yazicivo@ttnet.net.tr>
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

(define-condition duplicate-value (error)
  ((value
    :initarg :value
    :accessor value-of))
  (:documentation "Signaled upon every duplicated entry insertion."))

(define-condition search-result ()
  ((distance
    :initarg :distance
    :accessor distance-of)
   (value
    :initarg :value
    :accessor value-of))
  (:documentation "Signaled upon every successful search result."))

(define-condition search-exhausted ()
  ()
  (:documentation "Signaled when it is guaranteed that no more results can be
  returned."))

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
              (insert-value value sub-tree)
              (push (make-instance 'bk-tree :distance distance :value value)
                    (nodes-of tree)))))))

(defun search-value (value tree &key (threshold 1) (metric #'levenshtein))
  "Searches given VALUE in the supplied TREE."
  (let ((distance (funcall metric value (value-of tree))))
    ;; Check if there is any sub-trees available to search.
    (handler-case
        (unless (endp (nodes-of tree))
          (let ((nodes
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
                       :key #'car)))
            ;; Scan sub-trees.
            (loop for (sub-distance . sub-tree) in nodes
                  while (<= sub-distance threshold)
                  do (search-value value
                                   sub-tree
                                   :threshold threshold
                                   :metric metric))))
      ;; Search is exhausted. Skip scanning other sub-trees.
      (search-exhausted () nil))
    ;; After scanning sub-trees, if appropriate, signal this one.
    (if (<= distance threshold)
        (restart-case
            (signal 'search-result :distance distance :value (value-of tree))
          ;; Do nothing, continue searching.
          (continue-searching () nil))
        ;; If threshold limit is not reached, then search is exhausted.
        (signal 'search-exhausted))))

(defun collect-search-results (value tree &rest keys &key (limit 50) &allow-other-keys)
  "Convenient function to collect signaled SEARCH-RESULTs."
  (let ((count 0)
        results)
    (handler-bind
        ((search-result
          (lambda (result)
            (push result results)
            (incf count)
            (if (< count limit)
                (invoke-restart 'continue-searching)))))
      (apply #'search-value value tree :allow-other-keys t keys))
    (nreverse results)))

(defun print-tree (tree &key (stream *standard-output*) (depth 0))
  "Prints supplied BK-TREE in a human-readable format."
  ;; Print current value first.
  (format stream "~&")
  (loop repeat depth do (write-char #\space stream))
  (format stream "-> ~a" (value-of tree))
  ;; Iterate across sub-trees, according to their distances from root
  ;; node.
  (mapc
   (lambda (tree) (print-tree tree :stream stream :depth (+ 4 depth)))
   (sort (copy-list (nodes-of tree)) #'< :key #'distance-of)))