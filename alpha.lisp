(in-package :cl-user)
(defpackage #:nisp.util-alpha
  (:use :common-lisp :iterate :nisp.util-types)
  (:nicknames :ua))
(in-package :nisp.util-alpha)

(defun print-list-for-org-mode (list &optional (stream t) (prefix "  - "))
  "Print LIST to STREAM with PREFIX prepended to each item."
  (apply #'concatenate 'string
         (mapcan (lambda (x)
                   (list (format stream "~A~S~%" prefix x)))
                 list)))

(defun package-classes (package)
  "List all classes defined in PACKAGE."
  (iter (for symbol :in-package package :external-only t)
        (when (find-class symbol nil)
          (collect symbol))))

(defmacro repeated-clock (form &key (from 0)
                          (exponent-base 11/10)
                          (record-above-time 1/1000)
                          (stop-time 10))
  (let ((x (gensym))
        (iterations (gensym))
        (time (gensym))
        (result (gensym))
        (results (gensym))
        (points (gensym))
        (points-x (gensym))
        (total (gensym)))
    `(iterate (for ,x :from ,from)
              (for ,iterations = (round (expt ,exponent-base ,x)))
              (for ,result = (h::clock ,form ,iterations))
              (for ,time = (the (or (integer 0 0) positive-fixnum ratio) 
                             (cadr (assoc :time ,result))))
              (sum ,time :into ,total)
              
             (when (> ,time ,record-above-time)
               (collect ,x :into ,points-x)
               (collect (cons iterations ,time) :into ,points))
             (collect ,result :into ,results)
             (until (> ,total ,stop-time))
             (finally (return (values (cons ,points-x ,points)
                                      ,results
                                      ,x 
                                      ,iterations
                                      (length ,points)))))))
#+ ()
(defun locate-o-notation (points &optional (accuracy 1) (power 1))
  (let ((interpolate (PH_INTERPOLATION:DIRECT_INTERPOLATION points)))
    (approximate-o-notation interpolate accuracy power))
  )
#+ ()
(defun approximate-o-notation (interpolate accuracy power &optional result)
  (let ((res (funcall interpolate power)))
               (cond 
                 ((> (- res 0) accuracy) (approximate-o-notation 
                                          interpolate accuracy (- power 1/10)
                                          res))
                 ((< (- res 0) accuracy) (approximate-o-notation
                                          interpolate accuracy (+ power 1/10)
                                          res))
                 (t power))))

;; (let ((=test= (repeated-clock (allocate-instance (find-class 'identifier))
;;                                :record-above-time 1/20)))
;;   (locate-o-notation =test=)


;; (defmacro time (count &body body)
;;   `(cl:time (iterate (for ,(gensym) :from 0 :to ,count)
;;                      ,@body)))
;; (defmacro time-to-string (count &body body)
;;   (declare (type (integer 0 10000000000) count)
;;            (optimize (debug 3) (safety 3) (speed 0) (space 0)))
;;   `(with-output-to-string (*trace-output*)
;;      (time ,count ,@body)))

;; (defmacro time-to-string-optimized (count &body body)
;;   (declare (type (integer 0 10000000000) count)
;;            (optimize (debug 0) (safety 0) (speed 3) (space 1)))
;;   `(with-output-to-string (*trace-output*)
;;      (time ,count ,@body)))



;;;;;;
;;;;MOP related utilities
(defun call-class-default-initarg (class slot-name)
  "Call SLOT-NAME's default init function for CLASS."
  ;; NOT IMPLEMENTED fully. We just get the default value for the first
  ;; slot in the list paying no heed to SLOT-NAME.
  (declare (ignore slot-name))
  (funcall 
   (caddar (closer-mop:class-direct-default-initargs
           class))))


(in-package :cl-user)
(defpackage #:nisp-util-user
  (:use :cl :closer-mop :iterate :nisp.util-types)
  (:nicknames :nuu)
  (:shadowing-import-from :closer-mop
                          :defmethod :defgeneric
                          :generic-function :standard-generic-function))
(in-package :nisp-util-user)


;;; end file