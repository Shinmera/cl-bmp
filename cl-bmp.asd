#|
 This file is a part of cl-bmp
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-bmp
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library for dealing with Windows bitmaps (BMP, DIB, ICO, CUR)"
  :homepage "https://shinmera.github.io/cl-bmp/"
  :bug-tracker "https://github.com/shinmera/cl-bmp/issues"
  :source-control (:git "https://github.com/shinmera/cl-bmp.git")
  :serial T
  :components ((:file "package")
               (:file "bmp")
               (:file "pixels")
               (:file "documentation"))
  :depends-on (:binary-structures
               :documentation-utils))
