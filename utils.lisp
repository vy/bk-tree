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
;;; Levenshtein Metric Related Definitions
;;;

(deftype levenshtein-cost ()
  "Available penalty costs."
  '(integer 0 7))

(deftype levenshtein-max-distance ()
  "Maximum distance a comparison can result."
  `(integer 0 ,(- most-positive-fixnum 7)))

(defun levenshtein (src dst &key (insert-cost 1) (delete-cost 1) (substitute-cost 1))
  "An O(mn) implementation of the Levenshtein distance metric."
  (declare (type simple-string src dst)
           (type levenshtein-cost insert-cost delete-cost substitute-cost))
  (declare (optimize (speed 3)))
  (let ((ls (1+ (length src)))
        (ld (1+ (length dst))))
    ;; Validate supplied strings.
    (cond
      ((= 1 ls) (1- ld))
      ((= 1 ld) (1- ls))
      (t
       ;; Instead of building an (m+1)x(n+1) array, we'll use two different
       ;; arrays of size m+1 and n+1 for storing accumulated values in previous
       ;; calls.
       (let ((prev (make-array ls :element-type 'levenshtein-max-distance))
             (curr (make-array ls :element-type 'levenshtein-max-distance)))
         (declare (type (simple-array levenshtein-max-distance (*)) prev curr))
         ;; Initialize the "previous" row.
         (loop for i from 0 below ls
               do (setf (elt prev i) i))
         ;; Walk across supplied strings.
         (loop for j from 1 below ld
               for d across dst
               do (loop ;; Initially, first cell must increment sequentially, to
                        ;; mimic we're on the j'th column of the (m+1)x(n+1)
                        ;; array.
                        initially (setf (elt curr 0) j)
                        for i from 1 below ls
                        for s across src
                        ;; Calculate costs for probable operations.
                        for ins = (+ (elt prev i) insert-cost)      ; Insertion
                        for del = (+ (elt curr (1- i)) delete-cost) ; Deletion
                        for sub = (+ (elt prev (1- i))              ; Substitution
                                     (if (char= s d) 0 substitute-cost))
                        ;; Take the one with the minimum cost.
                        do (setf (elt curr i) (min ins del sub))
                        ;; Finally, swap current row with previous row.
                        finally (rotatef prev curr)))
         ;; Because the final value was swapped from the previous row to the
         ;; current row, that's where we'll find it.
         (elt prev (1- ls)))))))
