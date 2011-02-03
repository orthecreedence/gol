(in-package :gol)

(defun draw-grid ()
  (gl:color .1 .1 .1)
  (gl:push-matrix)
  (gl:translate 0 0 0)
  (dotimes (y (round (/ *config-graphics-window-y* *config-graphics-grid-res*)))
    (gl:begin :lines)
    (let ((y (* y *config-graphics-grid-res*)))
      (gl:vertex 0 y)
      (gl:vertex *config-graphics-window-x* y))
    (gl:end))
  (dotimes (x (round (/ *config-graphics-window-x* *config-graphics-grid-res*)))
    (gl:begin :lines)
    (let ((x (* x *config-graphics-grid-res*)))
      (gl:vertex x 0)
      (gl:vertex x *config-graphics-window-y*))
    (gl:end))
  (gl:pop-matrix))

(defun draw-life (life-form)
  (gl:color .2 .8 .2)
  (gl:push-matrix)
  (let ((x (* (nth 0 life-form) *config-graphics-grid-res*))
        (y (* (nth 1 life-form) *config-graphics-grid-res*))
        (res *config-graphics-grid-res*))
    (gl:translate x y 0)
    (gl:begin :polygon)
    (gl:vertex 0 0)
    (gl:vertex res 0)
    (gl:vertex res res)
    (gl:vertex 0 res)
    (gl:end))
  (gl:pop-matrix))





