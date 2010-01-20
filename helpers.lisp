(in-package :nisp.util-system)
(defpackage #:nisp.util-helpers
  (:use :common-lisp :iterate))
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
