(in-package :nisp.util-system)
(defpackage #:nisp.util-helpers
  (:use :common-lisp :iterate :nisp.util-types)
  (:nicknames :h :helpers)
  (:shadow :time))
(in-package :nisp.util-helpers)

(defun export-some-internals (package-name)
  (iter (for (symbol state) :in-packages package-name :having-access (:internal))
        (when (or (type-specifier-p symbol)
                  (fboundp symbol))
          (export symbol package-name)
          (collect symbol))))

(defun unintern-externals-from (package &optional (from-package *package*))
  "Remove all exports from PACKAGE in FROM-PACKAGE."
  (iter (for (symbol)
             :in-packages package
             :having-access (:external))
        (collect symbol)
        (unintern symbol from-package)))


(defmacro time (count &body body)
  `(cl:time (iterate (for ,(gensym) :from 0 :to ,count)
                     ,@body)))
(defmacro time-to-string (count &body body)
  (declare (type (integer 0 10000000000) count)
           (optimize (debug 3) (safety 3) (speed 0) (space 0)))
  `(with-output-to-string (*trace-output*)
     (time ,count ,@body)))

(defmacro time-to-string-optimized (count &body body)
  (declare (type (integer 0 10000000000) count)
           (optimize (debug 0) (safety 0) (speed 3) (space 1)))
  `(with-output-to-string (*trace-output*)
     (time ,count ,@body)))

(defmacro clock (form &optional (iterations 10000))
  "Execute FORM ITERATIONS times.

The result is an alist of the form:
  ((time . rational) (iterations . integer) (result . <form result>))"
  ;; This macro has a bug, it evaluates FORM one extra time at the end
  ;; of the macro expression to display the result value.
  (declare (type positive-fixnum)
           (optimize (debug 0) (speed 3) (safety 0)
                     (space 1) (compilation-speed 3)))
  (let ((start (gensym))
        (total (gensym))
        (avg (gensym)))
    `(iter (with ,start = (get-internal-real-time))
            (for ,(gensym) :from 0 :to ,iterations)
            ,form
            (finally (let* ((,total (the (or (integer 0 0) positive-fixnum) 
                                      (- (get-internal-real-time) ,start)))
                            (,avg (/ ,total 
                                     (* ,iterations
                                        internal-time-units-per-second))))
                       (return 
                         (list 
                          (cons :time (list 
                                       (/ ,total internal-time-units-per-second)
                                       (format nil "~8f" 
                                               (/ ,total 
                                                  internal-time-units-per-second))))
                          (cons :avg (list ,avg (format nil "~8f" ,avg)))
                          (cons :result ,form))))))))