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