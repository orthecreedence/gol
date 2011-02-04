(in-package :gol)

;;; draws the gaming grid
(defun draw-grid ()
  (gl:color .1 .1 .1)
  (gl:push-matrix)
  (gl:translate 0 0 0)
  (dotimes (y *config-grid-y*)
    (gl:begin :lines)
    (let ((y (* y *config-grid-res*)))
      (gl:vertex 0 y)
      (gl:vertex *config-graphics-window-x* y))
    (gl:end))
  (dotimes (x *config-grid-x*)
    (gl:begin :lines)
    (let ((x (* x *config-grid-res*)))
      (gl:vertex x 0)
      (gl:vertex x *config-graphics-window-y*))
    (gl:end))
  (gl:pop-matrix))

;; draws a life form at an x/y coordinate
(defun draw-life (x y)
  (gl:color .2 .8 .2)
  (gl:push-matrix)
  (let ((x (* x *config-grid-res*))
        (y (* y *config-grid-res*))
        (res *config-grid-res*))
    (gl:translate x y 0)
    (gl:begin :polygon)
    (gl:vertex 0 0)
    (gl:vertex res 0)
    (gl:vertex res res)
    (gl:vertex 0 res)
    (gl:end))
  (gl:pop-matrix))





