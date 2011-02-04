(asdf:operate 'asdf:load-op 'cffi)
(asdf:operate 'asdf:load-op 'cl-opengl)
(asdf:operate 'asdf:load-op 'cl-glu)
(asdf:operate 'asdf:load-op 'lispbuilder-sdl)

(defpackage :gol
	(:use :cl :cffi))

(in-package :gol)

(load "config")
(load "drawer")
(load "world")

(let ((w (make-instance 'world)))
  (world-init w)
  (world-end w))

