(in-package :gol)

;; define an "infinity" value
(defparameter *infinity-f* most-positive-single-float)
(defparameter *neg-infinity-f* most-negative-single-float)

;; graphics config
(defparameter *config-graphics-window-x* 640)
(defparameter *config-graphics-window-y* 480)

;; grid config
(defun recalculate-grid-coords()
  (setf *config-grid-x* (round (/ *config-graphics-window-x* *config-grid-res*)))
  (setf *config-grid-y* (round (/ *config-graphics-window-y* *config-grid-res*))))

(defparameter *config-grid-res* 13)
(defvar *config-grid-x*)  ;; don't give the following values, recalculate-grid-coords will do it for us
(defvar *config-grid-y*)
(recalculate-grid-coords)

