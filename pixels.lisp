#|
 This file is a part of cl-bmp
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.bmp)

(defun decode-pixels (bmp &optional output)
  (let* ((header (slot-value bmp 'header))
         (pixels (slot-value bmp 'pixels))
         (colors (slot-value bmp 'colors))
         (bits/pixel (bitmapcoreheader-bits/pixel header))
         (width (bitmapcoreheader-width header))
         (height (bitmapcoreheader-height header))
         (bytes/color (cond ((= 32 bits/pixel) 4)
                            ((= 24 bits/pixel) 3)
                            ((typep header 'bitmapv5infoheader) 4)
                            ((= 0 (length colors)) 1)
                            (T 3)))
         (bits/row (* 4 8 (ceiling (* width bits/pixel) (* 4 8))))
         (size (* width height bytes/color)))
    (cond ((< 0 (length colors))
           (unless output 
             (setf output (make-array size :element-type '(unsigned-byte 8))))
           (loop for y from 0 below height
                 for bit = (* y bits/row)
                 do (loop for x from 0 below width
                          for i = (* bytes/color (+ x (* y width)))
                          do (multiple-value-bind (byte subbit) (floor bit 8)
                               (let ((c (ldb (byte bits/pixel subbit) (aref pixels byte))))
                                 (loop for ci from 0 below bytes/color
                                       do (setf (aref output (+ ci i)) (aref colors (+ ci c)))))
                               (incf bit bits/pixel)))))
          ((/= size (length pixels))
           (unless output 
             (setf output (make-array size :element-type '(unsigned-byte 8))))
           (loop for y from 0 below height
                 do (loop for x from 0 below width
                          for i = (* bytes/color (+ x (* y width)))
                          do (loop for ci from 0 below bytes/color
                                   do (setf (aref output (+ ci i)) (aref pixels (+ ci i)))))))
          ((null output)
           (setf output pixels))
          (T
           (replace output pixels)))
    (values output
            width
            height
            bytes/color)))

(defun channel-decode-fun (channels)
  (lambda (pixels i)
    (declare (type (simple-array (unsigned-byte 8) (*))))
    (declare (type bs::index i))
    (ecase channels
      (4 (+ (ash  0 (aref pixels (+ 0 i)))
            (ash  8 (aref pixels (+ 1 i)))
            (ash 16 (aref pixels (+ 2 i)))
            (ash 24 (aref pixels (+ 3 i)))))
      (3 (+ (ash  0 (aref pixels (+ 0 i)))
            (ash  8 (aref pixels (+ 1 i)))
            (ash 16 (aref pixels (+ 2 i)))))
      (1 (+ (ash  0 (aref pixels (+ 0 i)))
            (ash  8 (aref pixels (+ 0 i)))
            (ash 16 (aref pixels (+ 0 i))))))))

(defun compute-color-table (pixels channels)
  (let ((table (make-hash-table :test 'eql))
        (decode (channel-decode-fun channels))
        (count 0))
    (loop for i from 0 below (length pixels) by channels
          for c = (funcall decode pixels i)
          do (unless (gethash c table)
               (setf (gethash c table) count)
               (incf count)
               (when (= 256 count)
                 (return NIL))))
    (let* ((channels (max channels 3))
           (colors (make-array (* channels count) :element-type '(unsigned-byte 8))))
      (loop for c being the hash-keys of table using (hash-value i)
            for p = (* channels i)
            do (setf (aref colors (+ 0 p)) (ldb (byte 8  0) c))
               (setf (aref colors (+ 1 p)) (ldb (byte 8  8) c))
               (setf (aref colors (+ 2 p)) (ldb (byte 8 16) c))
               (when (= 4 channels)
                 (setf (aref colors (+ 3 p)) (ldb (byte 8 24) c))))
      (values colors table))))

(defun encode-pixels (bmp pixels width height channels)
  (let ((header (slot-value bmp 'header))
        (decode (channel-decode-fun channels)))
    (multiple-value-bind (colors table) (compute-color-table pixels channels)
      (cond (colors
             (let* ((bits/pixel (integer-length (hash-table-count table)))
                    (bits/row (* 4 8 (ceiling (* width bits/pixel) (* 4 8))))
                    (output (make-array (* (floor bits/row 8) height) :element-type '(unsigned-byte 8))))
               (cond ((typep header 'bitmapinfoheader)
                      (setf (bitmapinfoheader-palette-size header) (length colors))
                      (setf (slot-value bmp 'colors) colors))
                     (T
                      (setf (slot-value bmp 'colors) (adjust-array colors (expt 2 bits/pixel)))))
               (setf (slot-value bmp 'pixels) output)
               (setf (bitmapcoreheader-bits/pixel header) bits/pixel)
               (loop for y from 0 below height
                     for bit = (* y bits/row)
                     do (loop for x from 0 below width
                              for i = (* channels (+ x (* y width)))
                              do (multiple-value-bind (byte subbit) (floor bit 8)
                                   (let ((c (gethash (funcall decode pixels i) table)))
                                     (setf (aref output byte) (dpb c (byte bits/pixel subbit) (aref output byte)))))
                                 (incf bit bits/pixel)))))
            (T
             (let* ((bits/pixel (* 8 channels))
                    (bits/row (* 4 8 (ceiling (* width bits/pixel) (* 4 8))))
                    (output (make-array (* (floor bits/row 8) height) :element-type '(unsigned-byte 8))))
               (setf (slot-value bmp 'colors) (make-array 0 :element-type '(unsigned-byte 8)))
               (when (typep header 'bitmapinfoheader)
                 (setf (bitmapinfoheader-palette-size header) 0))
               (setf (slot-value bmp 'pixels) output)
               (setf (bitmapcoreheader-bits/pixel header) (ecase channels (1 8) (3 24) (4 32)))
               (if (= (length output) (length pixels))
                   (replace output pixels)
                   (loop for y from 0 below height
                         for bit = (* y bits/row)
                         do (loop for x from 0 below width
                                  for i = (* channels (+ x (* y width)))
                                  for oi = (floor bit 8)
                                  do (loop for c from 0 below channels
                                           do (setf (aref output (+ c oi)) (aref pixels (+ c i))))
                                     (incf bit bits/pixel)))))))
      bmp)))
