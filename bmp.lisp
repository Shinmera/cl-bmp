#|
 This file is a part of cl-bmp
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.bmp)

(bs:define-io-structure rgb-mask
  (r uint32)
  (g uint32)
  (b uint32))

(bs:define-io-structure rgba-mask
  (:include rgb-mask)
  (a uint32))

(bs:define-io-structure xyz
  (x sint32)             ; FXPT2DOT30, 2 bits int, 30 bits fractional.
  (y sint32)
  (z sint32))

(bs:define-io-structure bitmapcoreheader
  (width uint32)
  (height uint32)
  (planes uint16)
  (bits/pixel uint16))

(bs:define-io-alias bitmapcompression
    (case uint32
      (0 :rgb)
      (1 :rle8)
      (2 :rle4)
      (3 :bitfields)
      (4 :jpeg)
      (5 :png)
      (6 :alpha-bitfields)
      (11 :cmyk)
      (12 :cmyk-rle8)
      (13 :cmyk-rle4)))

(bs:define-io-structure os22xbitmapheader/short
  (:include bitmapcoreheader)
  (compression bitmapcompression))

(bs:define-io-structure bitmapinfoheader
  (:include os22xbitmapheader/short)
  (image-size uint32)
  (horizontal-resolution sint32)
  (vertical-resolution sint32)
  (palette-size uint32)
  (important-color-count uint32))

(bs:define-io-structure bitmapv2infoheader
  (:include bitmapinfoheader)
  (mask rgb-mask))

(bs:define-io-structure bitmapv3infoheader
  (:include bitmapinfoheader)
  (maks rgba-mask))

(bs:define-io-structure bitmapv4infoheader
  (:include bitmapv3infoheader)
  (cs-type uint32)
  (red-endpoint xyz)
  (green-endpoint xyz)
  (blue-endpoint xyz)
  (gamma rgb))

(bs:define-io-structure bitmapv5infoheader
  (:include bitmapv4infoheader)
  (intent uint32)
  (profile-offset uint32)
  (profile-size uint32)
  uint32)

(bs:define-io-alias halftoning-algorithm
    (case uint16
      (0 NIL)
      (1 :error-diffusion)
      (2 :panda)
      (3 :super-circle)))

(bs:define-io-structure os22xbitmapheader
  (:include bitmapinfoheader)
  (resolution-unit uint16)
  uint16
  (origin uint16)
  (halftoning halftoning-algorithm)
  (halftoning-parameter-1 sint32)
  (halftoning-parameter-2 sint32)
  (color-encoding uint32)
  (identifier uint32))

(bs:define-io-structure bmp
  (type (case (string 2) ("BM" :BM) ("BA" :BA) ("CI" :CI) ("CP" :CP) ("IC" :IC) ("PT" :PT)))
  (size uint32)
  uint16 uint16
  (bitmap-offset uint32)
  (header (case uint32
            (124 bitmapv5infoheader)
            (108 bitmapv4infoheader)
            (64 os22xbitmapheader)
            (56 bitmapv3infoheader)
            (52 bitmapv2infoheader)
            (40 bitmapinfoheader)
            (16 os22xbitmapheader/short)
            (12 bitmapcoreheader)))
  (bit-masks (typecase (bs:slot header)
               (bitmapinfoheader
                (typecase (bs:slot header compression)
                  ((eql :bitfields) rgb-mask)
                  ((eql :alpha-bitfields) rgba-mask)))))
  (colors (typecase (bs:slot header)
            (bitmapinfoheader
             (vector uint8 (* 4 (bs:slot header palette-size))))
            (bitmapcoreheader
             (vector uint8 (* 3 (expt 2 (bs:slot header bits/pixel)))))
            (T NIL)))
  (pixels (vector uint8 (bs:slot header image-size))
          :offset (bs:slot bitmap-offset))
  #++ ;; FIXME: This does NOT work as the offset is emitted BEFORE the typecase.
  (color-profile (typecase (bs:slot header)
                   (bitmapv5infoheader
                    (vector uint8 (bs:slot header profile-size))))
                 :offset (bs:slot header profile-offset)))

(bs:define-io-structure ico-entry
  (width uint8)
  (height uint8)
  (palette-size uint8)
  uint8
  (property-1 uint16)
  (property-2 uint16)
  (octet-size uint32)
  (offset uint32))

(bs:define-io-structure bmpcontent
  (header (case uint32
            (124 bitmapv5infoheader)
            (108 bitmapv4infoheader)
            (64 os22xbitmapheader)
            (56 bitmapv3infoheader)
            (52 bitmapv2infoheader)
            (40 bitmapinfoheader)
            (16 os22xbitmapheader/short)
            (12 bitmapcoreheader)))
  (bit-masks (typecase (bs:slot header)
               (bitmapinfoheader
                (typecase (bs:slot header compression)
                  ((eql :bitfields) rgb)
                  ((eql :alpha-bitfields) rgba)))))
  (colors (typecase (bs:slot header)
            (bitmapinfoheader
             (vector uint8 (* 4 (bs:slot header palette-size))))
            (bitmapcoreheader
             (vector uint8 (* 3 (expt 2 (bs:slot header bits/pixel)))))
            (T NIL)))
  (pixels (vector uint8 (bs:slot header image-size))))

(bs:define-io-structure ico
  #(0 0)
  (type (case uint16
          (1 :ico)
          (2 :cur)))
  (count uint16)
  (entries (vector ico-entry (bs:slot count)))
  (images (vector bmpcontent (bs:slot count) (bs:slot (aref (bs:slot entries) bs:i) offset))))
