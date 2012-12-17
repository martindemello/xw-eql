;;; quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "iterate")

(defpackage :xw
  (:use :common-lisp :eql :iterate)
  (:export
   #:*xw*
   #:start))

(in-package :xw)

(defvar *xw* (qnew "QWidget(QWidget*,Qt::WindowFlags)" nil |Qt.WindowStaysOnTopHint|))

(defvar *N* 14)

(defvar *board* (make-array (list (+ *N* 1) (+ *N* 1)) :initial-element #\Space))
(setf (aref *board* 0 0) #\H)
(setf (aref *board* 0 1) #\E)
(setf (aref *board* 0 2) #\L)
(setf (aref *board* 0 3) #\L)
(setf (aref *board* 0 4) #\O)
(setf (aref *board* 0 5) #\#)

(defun start ()
  (qset *xw* "font" (font 15))
  (connect-grid-painter *xw*)
  (qoverride *xw* "paintEvent(QPaintEvent*)" 'paint)
  (x:do-with (qfun *xw*) "show" "raise"))

(defun brush (color)
  (let ((brush (qnew "QBrush")))
    (x:do-with (qfun brush)
      ("setStyle" |Qt.SolidPattern|)
      ("setColor(QColor)" color))
    brush))

(defun font (size)
  (let ((font (qnew "QFont(QFont)" (qfun "QApplication" "font"))))
    (x:do-with (qfun font)
      ("setPixelSize" size))
    font))

(defun connect-grid-painter (widget)
  (let ((painter (qnew "QPainter"))
        (brush-black (brush "black"))
        (brush-white (brush "white")))
    (defun paint (ev)
      (flet ((! (&rest args)
                (apply 'qfun painter args)))
        (! "begin(QWidget*)" widget)
        (let* ((size (qget widget "size"))
               (scale (floor (/ (apply 'min size) 20))))

          (iter (for j from 0 to *N*)
            (iter (for i from 0 to *N*)
              (let* ((x (* i scale))
                     (y (* j scale))
                     (sq (list (+ x 1) (+ y 1) scale scale))
                     (c (aref *board* j i)))
                (if (eql c #\#)
                  (progn
                    (! "setBrush(QBrush)" brush-black)
                    (! "drawRect(QRect)" sq))
                  (progn
                    (! "setBrush(QBrush)" brush-white)
                    (! "drawRect(QRect)" sq)
                    (! "drawText(QPoint,QString)" (list (+ x 5) (+ y (- scale 3))) (string c))) )))))

        (! "end")))))

(start)
