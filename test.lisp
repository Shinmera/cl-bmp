(defpackage #:org.shirakumo.bmp.test
  (:local-nicknames
   (#:bmp #:org.shirakumo.bmp))
  (:use #:cl #:parachute)
  (:export
   #:bmp))

(in-package #:org.shirakumo.bmp.test)

(define-test bmp
  :defun T)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*)))

(defun test-file (file)
  (merge-pathnames file (merge-pathnames "test/" *here*)))

(defstruct (image-data (:conc-name NIL))
  width height channels pixels)

(defun png-data (file)
  (let ((png (pngload:load-file file :flatten T)))
    (make-image-data
     :pixels (pngload:data png)
     :width (pngload:width png)
     :height (pngload:height png)
     :channels (ecase (pngload:color-type png)
                 (:greyscale 1)
                 (:greyscale-alpha 2)
                 (:truecolour 3)
                 (:truecolour-alpha :rgba)
                 (:indexed-colour
                  (truncate (length (pngload:data png))
                            (* (pngload:width png) (pngload:height png))))))))

(defun bmp-data (file)
  (let ((bmp (bmp:read-bmp file)))
    (multiple-value-bind (data width height channels) (bmp:decode-pixels bmp)
      (make-image-data
       :pixels data
       :width width
       :height height
       :channels channels))))

(defun bin-diff (exp got)
  (cond ((< (length got) (length exp)) :too-short)
        ((< (length exp) (length got)) :too-long)
        (T (loop for i from 0 below (length exp)
                 for e = (aref exp i)
                 for g = (aref got i)
                 do (when (/= e g)
                      (return (list :diff i :expected e :got g)))))))

(defmacro define-test-for-file (file)
  `(define-test ,file
     :parent bmp
     :defun T
     (let ((png (png-data ,(test-file (make-pathname :name file :type "png"))))
           (bmp (bmp-data ,(test-file (make-pathname :name file :type "bmp")))))
       (is = (width png) (width bmp))
       (is = (height png) (height bmp))
       (is = (channels png) (channels bmp))
       (is eq NIL (bin-diff (pixels png) (pixels bmp))))))

(macrolet ((define-all ()
             `(progn
                ,@(loop for file in (directory (test-file "*.bmp"))
                        collect `(define-test-for-file ,(pathname-name file))))))
  (define-all))
