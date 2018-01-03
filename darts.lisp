(defpackage #:darts
  (:use #:cl))

(in-package #:darts)

(defparameter *possible-throws*
  (flet ((multiplier (factor)
           (lambda (n) (* n factor))))
    (let* ((singles (loop for i from 1 to 20 collect i))
           (with-doubles (union singles
                                (mapcar (multiplier 2) singles)
                                :test #'=))
           (with-triples (union with-doubles
                                (mapcar (multiplier 3) singles)
                                :test #'=)))
      (push 25 with-triples)
      (push 50 with-triples)
      (sort with-triples #'<))))

(defparameter *goal-points* 501
  "Total of points to win a leg")

(defun can-be-double-p (num)
  "See if the number can be a double shot.
Which is a requirement for the last throw."
  (declare (type (signed-byte 32) num))
  (or (= num 50) ; bullseye
      (and (evenp num)
           (<= 2 num 40))))

(defparameter *biggest-possible-throw*
  (first (last *possible-throws*)))

(defparameter *max-points-in-eight-throws*
  (* 8 *biggest-possible-throw*)
  "Maximum number of points achievable
for the first eight throws.")

(defparameter *left-after-eight-perfect-throws*
  (- *goal-points* *max-points-in-eight-throws*))

(defparameter *possible-last-throws*
  (labels ((can-be-last-throw-p (throw)
             (and (>= throw *left-after-eight-perfect-throws*)
                  (can-be-double-p throw))))
    (remove-if-not #'can-be-last-throw-p *possible-throws*))
  "List of throws which have the potential to be the winning
throw in just nine throws.")

(defun possible-throws-for-first-eight (last-throw)
  "List of possible throws for the first eight throws
given the value of `last-throw'."
  (let ((margin-of-error (- last-throw
                            (- *goal-points*
                               *max-points-in-eight-throws*))))
    (labels ((can-still-work (throw)
               (>= throw (- *biggest-possible-throw*
                            margin-of-error))))
      (remove-if-not #'can-still-work *possible-throws*))))

(defun Y-memoize (F &optional
                      (cache (make-hash-table :test #'equal)))
  "A memoizing version of the Y combinator"
  (lambda (&rest args)
    (multiple-value-bind (result cached-p)
        (gethash args cache)
      (if cached-p
          result
          (setf (gethash args cache)
                (apply (funcall F #'(lambda (&rest arg-list)
                                      (apply (Y-memoize F cache)
                                             arg-list)))
                       args))))))

(defun partitions (sum elements)
  "Find all partitions of the integer `sum'
constructable from elements."
  (labels ((valid-for (expected-sum)
             (lambda (l)
               (let ((sum (apply #'+ l)))
                 (= sum expected-sum))))
           (helper (self)
             (lambda (s e)
               (let ((pivot (first e)))
                 (cond
                   ((or (< s 0) (null e)) nil)
                   ((= s pivot) (list (list pivot)))
                   (t (concatenate 'list
                                   (remove-if-not (valid-for s) (funcall self s (rest e)))
                                   (mapcar #'(lambda (l) (push pivot l))
                                           (remove-if-not (valid-for (- s pivot))
                                                          (funcall self (- s pivot) e))))))))))
    (funcall (Y-memoize #'helper) sum elements)))

(defun solve ()
  "Print out possible solutions (some of them is going to be `NIL')."
  (dolist (last-throw *possible-last-throws*)
    (format t "~%~%Possible first eight throw when last throw is ~A:~%~%~S"
            last-throw
            (remove-if-not #'(lambda (solution) (= 8 (length solution)))
                           (partitions (- *goal-points* last-throw)
                                       (possible-throws-for-first-eight last-throw))))))
