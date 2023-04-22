#|
 This file is a part of cl-bmp
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.bmp
  (:local-nicknames
   (#:bs #:org.shirakumo.binary-structures))
  (:shadow #:type)
  (:use #:cl #:org.shirakumo.binary-structures.types)
  (:export
   #:bmp
   #:read-bmp
   #:write-bmp
   #:ico-entry
   #:ico
   #:read-ico
   #:write-ico)
  (:export
   #:r
   #:g
   #:b
   #:a
   #:x
   #:y
   #:z
   #:width
   #:height
   #:planes
   #:bits/pixel
   #:compression
   #:horizontal-resolution
   #:vertical-resolution
   #:mask
   #:red-endpoint
   #:green-endpoint
   #:blue-endpoint
   #:gamma
   #:intent
   #:resolution-unit
   #:origin
   #:color-encoding
   #:identifier
   #:header
   #:bit-masks
   #:colors
   #:pixels
   #:header
   #:property-1
   #:property-2
   #:bit-masks
   #:colors
   #:pixels
   #:type
   #:entries
   #:images
   #:halftoning
   #:decode-pixels
   #:encode-pixels))
