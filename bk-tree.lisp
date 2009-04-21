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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient Helper Utilities
;;;

(defun print-tree (tree &key (stream *standard-output*) (depth 0))
  "Prints supplied `TREE' in a human-readable(?) format."
  ;; Print current value first.
  (format stream "~&")
  (loop repeat depth do (write-char #\space stream))
  (format stream "-> (~D) ~A" (distance-of tree) (value-of tree))
  ;; Iterate across sub-trees, according to their distances from root
  ;; node.
  (mapc
   (lambda (tree) (print-tree tree :stream stream :depth (+ 4 depth)))
   (sort (copy-list (nodes-of tree)) #'< :key #'distance-of)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tree Operations
;;;

(defun insert-value (value tree &key (metric #'levenshtein))
  "Inserts given `VALUE' into supplied `TREE'."
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

(defun hairy-search-value (value tree &key (threshold 1) (metric #'levenshtein)
                           limit ordered-results ordered-traversal)
  "Return a list of `SEARCH-RESULT' instances built from `TREE' and its children
whose value is no more distant from `VALUE' than `THRESHOLD', using `METRIC' to
measure the distance.

If `LIMIT' is non-NIL, given number of first found results will be
returned.

If `ORDERED-RESULTS' is non-NIL, returned results will be ordered according to
their distances from `VALUES'.

If `ORDERED-TRAVERSAL' is non-NIL, candidates in a level (e.g. children of a
validated node) will be traversed in sorted order according to the absolute
difference between the parent distance and child distance -- the (probably) more
similar is first.

`HAIRY-SEARCH-VALUE' is a feature rich and hence slower derivative of
`SEARCH-VALUE'. For simple query patterns, consider using `SEARCH-VALUE'."
  (when (and tree (value-of tree))
    (let ((results)
          (count 0))
      (labels ((%search (tree)
                 (unless (and limit (< limit count))
                   (let ((distance (funcall metric value (value-of tree))))
                     ;; Try to submit this result first.
                     (unless (< threshold distance)
                       (push (make-instance 'search-result
                                            :distance distance
                                            :value (value-of tree))
                             results)
                       (incf count))
                     ;; Scan children.
                     (if ordered-traversal
                         ;; Sort available sub-trees collected under
                         ;; this tree, according to absolute difference
                         ;; between DISTANCE and SUB-DISTANCE.
                         (loop for (sub-distance . sub-tree)
                                 in (sort
                                      ;; (SUB-DISTANCE . SUB-TREE) pairs.
                                     (mapcar
                                       (lambda (sub-tree)
                                         (cons (abs (- distance
                                                       (distance-of sub-tree)))
                                               sub-tree))
                                       (nodes-of tree))
                                      #'< :key #'car)
                               until (or (< threshold sub-distance)
                                         (and limit (< limit count)))
                               do (%search sub-tree))
                         ;; Or scan children without an ordering.
                         (loop for sub-tree in (nodes-of tree)
                               until (and limit (< limit count))
                               do (%search sub-tree)))))))
        (%search tree))
      (if ordered-results
          (sort results #'< :key #'distance-of)
          results))))

(defun search-value (value tree &key (threshold 1) (metric #'levenshtein)
                     ordered-results)
  "Return a list of `SEARCH-RESULT' instances built from `TREE' and its children
whose value is no more distant from `VALUE' than `THRESHOLD', using `METRIC' to
measure the distance.

If `ORDERED-RESULTS' is non-NIL, collected results will be sorted according to
their distances from `VALUE'."
  (let (results)
    (labels ((%search (tree)
               (let ((distance (funcall metric value (value-of tree))))
                 ;; Scan children.
                 (dolist (node (nodes-of tree))
                   (unless (< threshold (abs (- distance (distance-of node))))
                     (%search node)))
                 ;; Submit current node, if appropriate.
                 (when (< threshold distance)
                   (push
                    (make-instance
                     'search-result
                     :distance distance
                     :value (value-of tree))
                    results)))))
      (%search tree))
    (if ordered-results
        (sort results #'< :key #'distance-of)
        results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tree Statistics Routines
;;;

(defun maximum-depth (tree)
  "Returns maximum depth of the `TREE'."
  (labels ((%scan (tree depth)
             (max depth
                  (loop for node in (nodes-of tree)
                        maximize (%scan node (1+ depth))))))
    (%scan tree 0)))

(defun average-children-count (tree)
  "Returns average children count per node of the supplied `TREE'."
  (let ((n-children 0)
        (n-nodes 0))
    (labels ((%scan (tree)
               ;; Count children, unless this is a leaf node.
               (unless (null (nodes-of tree))
                 (incf n-nodes)
                 (dolist (node (nodes-of tree))
                   (incf n-children)
                   (%scan node)))))
      (%scan tree))
    (if (zerop n-nodes) 0 (/ n-children n-nodes))))
