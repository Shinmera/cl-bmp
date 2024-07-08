(asdf:defsystem cl-bmp
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library for dealing with Windows bitmaps (BMP, DIB, ICO, CUR)"
  :homepage "https://shinmera.github.io/cl-bmp/"
  :bug-tracker "https://github.com/shinmera/cl-bmp/issues"
  :source-control (:git "https://github.com/shinmera/cl-bmp.git")
  :serial T
  :components ((:file "package")
               (:file "bmp")
               (:file "access")
               (:file "documentation"))
  :depends-on (:binary-structures
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-bmp/test))))

(asdf:defsystem cl-bmp/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the cl-bmp system."
  :homepage "https://shinmera.github.io/cl-bmp/"
  :bug-tracker "https://github.com/shinmera/cl-bmp/issues"
  :source-control (:git "https://github.com/shinmera/cl-bmp.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:cl-bmp :parachute :pngload)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.bmp.test)))
