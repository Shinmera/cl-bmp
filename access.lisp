(in-package #:org.shirakumo.bmp)

(defmacro define-accessor (name type &optional (slot name))
  `(progn (defmethod ,name ((entry ,type))
            (,(intern (format NIL "~a-~a" type slot)) entry))
          (defmethod (setf ,name) (value (entry ,type))
            (setf (,(intern (format NIL "~a-~a" type slot)) entry) value))))

(defmacro define-delegates (type slot &rest delegates)
  `(progn ,@(loop for delegate in delegates
                  collect `(defmethod ,delegate ((entry ,type))
                             (,delegate (,(intern (format NIL "~a-~a" type slot)) entry)))
                  collect `(defmethod (setf ,delegate) (value (entry ,type))
                             (setf (,delegate (,(intern (format NIL "~a-~a" type slot)) entry)) value)))))

(define-accessor r rgb-mask)
(define-accessor g rgb-mask)
(define-accessor b rgb-mask)
(define-accessor a rgba-mask)
(define-accessor x xyz)
(define-accessor y xyz)
(define-accessor z xyz)
(define-accessor width bitmapcoreheader)
(define-accessor height bitmapcoreheader)
(define-accessor planes bitmapcoreheader)
(define-accessor bits/pixel bitmapcoreheader)
(define-accessor compression os22xbitmapheader/short)
(define-accessor horizontal-resolution bitmapinfoheader)
(define-accessor vertical-resolution bitmapinfoheader)
(define-accessor mask bitmapv2infoheader)
(define-accessor mask bitmapv3infoheader)
(define-accessor red-endpoint bitmapv4infoheader)
(define-accessor green-endpoint bitmapv4infoheader)
(define-accessor blue-endpoint bitmapv4infoheader)
(define-accessor gamma bitmapv4infoheader)
(define-accessor intent bitmapv5infoheader)
(define-accessor resolution-unit os22xbitmapheader)
(define-accessor origin os22xbitmapheader)
(define-accessor color-encoding os22xbitmapheader)
(define-accessor identifier os22xbitmapheader)
(define-accessor header bmp)
(define-accessor bit-masks bmp)
(define-accessor colors bmp)
(define-accessor pixels bmp)
(define-accessor width ico-entry)
(define-accessor height ico-entry)
(define-accessor property-1 ico-entry)
(define-accessor property-2 ico-entry)
(define-accessor header bmpcontent)
(define-accessor bit-masks bmpcontent)
(define-accessor colors bmpcontent)
(define-accessor pixels bmpcontent)
(define-accessor type ico)
(define-accessor entries ico)
(define-accessor images ico)

(define-delegates bmpcontent header 
  width height planes bits/pixel compression horizontal-resolution
  vertical-resolution mask red-endpoint green-endpoint blue-endpoint
  gamma intent resolution-unit origin color-encoding identifier)

(define-delegates bmp header
  width height planes bits/pixel compression horizontal-resolution
  vertical-resolution mask red-endpoint green-endpoint blue-endpoint
  gamma intent resolution-unit origin color-encoding identifier)

(defmethod halftoning ((header os22xbitmapheader))
  (list (os22xbitmapheader-halftoning header)
        (os22xbitmapheader-halftoning-parameter-1 header)
        (os22xbitmapheader-halftoning-parameter-2 header)))

(defun decode-pixels (bmp &optional output)
  (let* ((header (slot-value bmp 'header))
         (pixels (slot-value bmp 'pixels))
         (colors (slot-value bmp 'colors))
         (bits/pixel (bitmapcoreheader-bits/pixel header))
         (width (bitmapcoreheader-width header))
         (height (bitmapcoreheader-height header))
         (bytes/color (cond ((= 32 bits/pixel) 4)
                            ((= 24 bits/pixel) 3)
                            ((= 16 bits/pixel) 2)
                            ((=  8 bits/pixel) 1)
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
    (declare (cl:type (simple-array (unsigned-byte 8) (*)) pixels))
    (declare (cl:type bs::index i))
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
