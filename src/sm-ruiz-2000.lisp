;; Generating the Nth prime. (S.M.Ruiz 2000)
;; According to http://zh.wikipedia.org/wiki/%E7%B4%A0%E6%95%B0%E5%85%AC%E5%BC%8F  .

(in-package :cl-prime-maker)

(declaim (optimize (speed 3)))

(defvar *ruiz-pis* (make-hash-table))   ; cache pi(k)
(defvar *ruiz-pis-part1* (make-hash-table)) ; cache a sub-part of pi(k)
(defvar *ruiz-results* (make-hash-table))   ; cache the results of p(n)

(defun compute-ruiz-pis-part1 (j)
  (let ((result-from-hash (gethash j *ruiz-pis-part1*)))
    (if (null result-from-hash)
        (let* ((s-max (floor (sqrt j)))
               (sigma1 (loop
                          for s from 1 to s-max
                          summing
                            (- (floor (/ (1- j) s))
                               (floor (/ j s)))))
               (result (floor (* (/ 2 j)
                                 (1+ sigma1)))))
          (setf (gethash j *ruiz-pis-part1*) result)
          result)              
        result-from-hash)))

(defun compute-ruiz-pi (k)
  (let ((result-from-hash (gethash k *ruiz-pis*)))
    (if (null result-from-hash)
        (let ((result (cond
                        ((= k 1) 0)
                        ((= k 2)
                         (1+ (compute-ruiz-pis-part1 2)))
                        (t (+ 1
                              (compute-ruiz-pis-part1 k)
                              (compute-ruiz-pi (1- k)))))))
          (setf (gethash k *ruiz-pis*) result)
          result)
        result-from-hash)))

(defun get-nth-prime (n)
  (declare (type integer n))
  "Generate the Nth prime number when N >= 1. Otherwise this function always returns 2."
  (if (>= n 1)
      (let ((result-from-hash (gethash n *ruiz-results*)))
        (if (null result-from-hash)
            (let ((result (1+ (loop
                                 for k from 1 to (* 2 (1+ (floor (* n (log n)))))
                                 summing (- 1 (floor (/ (compute-ruiz-pi k)
                                                        n)))))))
              (setf (gethash n *ruiz-results*) result)
              result)
            result-from-hash))
      
      2))
           








