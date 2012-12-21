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

;;; board and cursor
(defstruct cursor x y dir)

(defvar *cursor* (make-cursor :x 0 :y 0 :dir 'across))

(defvar *N* 15)
(defvar *M* (- *N* 1))

(defun make-board (size)
  (make-array (list size size) :initial-element #\Space))

(defvar *board* (make-board *N*))

(setf (aref *board* 0 0) #\H)
(setf (aref *board* 0 1) #\E)
(setf (aref *board* 0 2) #\L)
(setf (aref *board* 0 3) #\L)
(setf (aref *board* 0 4) #\O)
(setf (aref *board* 0 5) #\#)


;;; gui widget
(defvar *xw* (qnew "QWidget(QWidget*,Qt::WindowFlags)" nil |Qt.WindowStaysOnTopHint|))

(defun start ()
  (qset *xw* "font" (font 15))
  (define-overrides)
  (qadd-event-filter nil |QEvent.KeyPress| 'key-pressed)
  (x:do-with (qfun *xw*) "show" "raise"))

(defun brush (r g b a)
  (let ((brush (qnew "QBrush"))
        (color (qfun "QColor" "fromRgb" r g b a)))
    (x:do-with (qfun brush)
      ("setStyle" |Qt.SolidPattern|)
      ("setColor(QColor)" color))
    brush))

(defun font (size)
  (let ((font (qnew "QFont(QFont)" (qfun "QApplication" "font"))))
    (x:do-with (qfun font)
      ("setPixelSize" size))
    font))

(defun draw ()
  (qfun *xw* "repaint"))

; increment and wraparound
(defun wrap (x dx w)
  (mod (+ x dx) w))

(define-modify-macro wrapf (inc w) wrap)

(defun move (i j)
  (wrapf (cursor-x *cursor*) i *N*)
  (wrapf (cursor-y *cursor*) j *N*)
  (draw))

(defun flip-cursor ()
  (setf (cursor-dir *cursor*)
        (case (cursor-dir *cursor*)
          ('across 'down)
          ('down 'across))))

(defun set-current-square (c)
  (setf (aref *board* (cursor-y *cursor*) (cursor-x *cursor*)) c)
  (case (cursor-dir *cursor*)
    ('across (move 1 0))
    ('down   (move 0 1))))

(defun blackp (x y)
  (eql (aref *board* y x) #\#))

(defun toggle-black-current-square ()
  (let ((x (cursor-x *cursor*))
        (y (cursor-y *cursor*)))
    (setf (aref *board* y x)
          (if (blackp x y) #\Space #\#))))

(defun unset-current-square (move)
  (let ((x (cursor-x *cursor*))
        (y (cursor-y *cursor*)))
    ; do not overwrite a black square
    (if (not (blackp x y))
      (setf (aref *board* y x) #\Space)))
  (if move
    (case (cursor-dir *cursor*)
      ('across (move -1 0))
      ('down   (move 0 -1)))))

(defun key-pressed (obj event)
  (let ((key (qfun* event "QKeyEvent" "key")))
    ; letters
    (if (and (>= key |Qt.Key_A|)
             (<= key |Qt.Key_Z|))
      (set-current-square (code-char key))
      (progn
        (case key
          (#.|Qt.Key_Space|
           (toggle-black-current-square))
          (#.|Qt.Key_Enter|
           (flip-cursor))
          (#.|Qt.Key_Return|
           (flip-cursor))
          (#.|Qt.Key_Delete|
           (unset-current-square nil))
          (#.|Qt.Key_Backspace|
           (unset-current-square t))
          (#.|Qt.Key_Up|
           (move 0 -1))
          (#.|Qt.Key_Down|
           (move 0 1))
          (#.|Qt.Key_Left|
           (move -1 0))
          (#.|Qt.Key_Right|
           (move 1 0))
          (t (return-from key-pressed)))))
    (draw)
    t))


(defun define-overrides ()
  (x:do-with (qoverride *xw*)
    ("paintEvent(QPaintEvent*)" 'paint)
    ("mousePressEvent(QMouseEvent*)" 'mouse-press-event)))

(defun mouse-press-event (event)
  (let* ((pos (qfun event "pos"))
           (x (first pos))
           (y (second pos))
           (size (qget *xw* "size"))
           (scale (floor (/ (apply 'min size) 20)))
           (i (floor (/ x scale)))
           (j (floor (/ y scale))))
    (setf (cursor-x *cursor*) i)
    (setf (cursor-y *cursor*) j)
    (draw)))

(let ((painter (qnew "QPainter"))
      (brush-black (brush 0 0 0 255))
      (brush-cursor-ac (brush 255 192 192 128))
      (brush-cursor-dn (brush 192 255 192 128))
      (brush-white (brush 255 255 255 255)))
  (defun paint (ev)
    (flet ((! (&rest args)
              (apply 'qfun painter args)))
      (! "begin(QWidget*)" *xw*)
      (let* ((size (qget *xw* "size"))
             (scale (floor (/ (apply 'min size) 20))))

        (iter (for j from 0 to *M*)
          (iter (for i from 0 to *M*)
            (let* ((x (* i scale))
                   (y (* j scale))
                   (sq (list (+ x 1) (+ y 1) scale scale))
                   (c (aref *board* j i)))
              ; square
              (if (eql c #\#)
                (progn
                  (! "setBrush(QBrush)" brush-black)
                  (! "drawRect(QRect)" sq))
                (progn
                  (! "setBrush(QBrush)" brush-white))
                  (! "drawRect(QRect)" sq)
                  (! "drawText(QPoint,QString)"
                     (list (+ x 5) (+ y (- scale 3))) (string c)))

              ; cursor
              (if (and (= i (cursor-x *cursor*))
                       (= j (cursor-y *cursor*)))
                (let ((b (if (eql (cursor-dir *cursor*) 'across)
                           brush-cursor-ac
                           brush-cursor-dn)))
                  (! "setBrush(QBrush)" b)
                  (! "drawRect(QRect)" sq)))
            ))))

      (! "end"))))

(start)
