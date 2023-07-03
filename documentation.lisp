(in-package #:org.shirakumo.bmp)

(docs:define-docs
  (cl:type bmp
    "Representation of a bitmap file.

See DECODE-PIXELS
See ENCODE-PIXELS
See READ-BMP
See WRITE-BMP
See WIDTH
See HEIGHT
See PLANES
See BITS/PIXEL
See COMPRESSION
See HORIZONTAL-RESOLUTION
See VERTICAL-RESOLUTION
See MASK
See RED-ENDPOINT
See GREEN-ENDPOINT
See BLUE-ENDPOINT
See GAMMA
See INTENT
See RESOLUTION-UNIT
See ORIGIN
See COLOR-ENCODING
See IDENTIFIER")
  
  (function read-bmp
    "Read a bitmap file.

Can decode from a stream, pathname, octet-vector, and pointer.

After decoding you will want to use DECODE-PIXELS to read out
the pixel data.

See BMP (type)
See DECODE-PIXELS")
  
  (function write-bmp
    "Write a bitmap file.

Can encode to a stream, pathname, octet-vector, and pointer.

To write the pixels into the bitmap structure, you should used
ENCODE-PIXELS.

See BMP (type)
See ENCODE-PIXELS")
  
  (cl:type ico-entry
    "Representation of the metadata about an ICO image.

See ICO (type)
See WIDTH
See HEIGHT
See PROPERTY-1
See PROPERTY-2")
  
  (cl:type ico
    "Representation of an ICO or CUR file.

See READ-ICO
See WRITE-ICO
See TYPE
See ENTRIES
See IMAGES")
  
  (function read-ico
    "Read an icon or cursor file.

Can decode from a stream, pathname, octet-vector, and pointer.

See ICO (type)")
  
  (function write-ico
    "Write an icon or cursor file.

Can encode to a stream, pathname, octet-vector, and pointer.

See ICO (type)"))

(docs:define-docs
  (function r
    "Accesses the red mask.

See RGB-MASK (type)")
  
  (function g
    "Accesses the green mask.

See RGB-MASK (type)")
  
  (function b
    "Accesses the blue mask.

See RGB-MASK (type)")
  
  (function a
    "Accesses the alpha mask.

See RGBA-MASK (type)")
  
  (function x
    "Accesses the X component.

See XYZ (type)")
  
  (function y
    "Accesses the Y component.

See XYZ (type)")
  
  (function z
    "Accesses the Z component.

See XYZ (type)")
  
  (function width
    "Accesses the width of the image.

See BITMAPCOREHEADER (type)
See ICO-ENTRY (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function height
    "Accesses the height of the image.

See BITMAPCOREHEADER (type)
See ICO-ENTRY (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function planes
    "Accesses the plane count of the image.

See BITMAPCOREHEADER (type)
See BMP (type)
See BMPCONTENT (type)")

  (function bits/pixel
    "Accesses the bits per pixel count of the image.

See BITMAPCOREHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function compression
    "Accesses the compression method of the image.

See OS22XBITMAPHEADER/SHORT (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function horizontal-resolution
    "Accesses the horizontal physical resolution method ofhorizontal the image.

See BITMAPINFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function vertical-resolution
    "Accesses the vertical physical resolution method of the image.

See BITMAPINFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function mask
    "Accesses the mask of the image.

Returns either an RGB-MASK or an RGBA-MASK.

See RGB-MASK (type)
See RGBA-MASK (type)
See BITMAPV2INFOHEADER (type)
See BITMAPV3INFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function red-endpoint
    "Accesses the red endpoint of the image.

See XYZ (type)
See BITMAPV2INFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function green-endpoint
    "Accesses the green endpoint of the image.

See XYZ (type)
See BITMAPV2INFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function blue-endpoint
    "Accesses the blue endpoint of the image.

See XYZ (type)
See BITMAPV2INFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function gamma
    "Accesses the gamma of the image.

See RGB-MASK (type)
See BITMAPV2INFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function intent
    "Accesses the rendering intent of the image.

See BITMAPV5INFOHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function resolution-unit
    "Accesses the resolution unit identifier of the image.

See OS22XBITMAPHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function origin
    "Accesses the image origin identifier of the image.

See OS22XBITMAPHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function color-encoding
    "Accesses the color encoding identifier of the image.

See OS22XBITMAPHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function identifier
    "Accesses the identifier of the image.

See OS22XBITMAPHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function header
    "Accesses the header of the image.

See BITMAPCOREHEADER (type)
See OS22XBITMAPHEADER/SHORT (type)
See BITMAPINFOHEADER (type)
See BITMAPV2INFOHEADER (type)
See BITMAPV3INFOHEADER (type)
See BITMAPV4INFOHEADER (type)
See BITMAPV5INFOHEADER (type)
See OS22XBITMAPHEADER (type)
See BMP (type)
See BMPCONTENT (type)")

  (function property-1
    "Returns the first property of the icon entry.

For a cursor that's the \"hot zone\" X coordinate.

See ICO-ENTRY (type)")

  (function property-2
    "Returns the second property of the icon entry.

For a cursor that's the \"hot zone\" Y coordinate.

See ICO-ENTRY (type)")
  
  (function bit-masks
    "Accesses the bit masks of the image.

See RGB-MASK (type)
See RGBA-MASK (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function colors
    "Accesses the (packed) color palette of the image.

See BMP (type)
See BMPCONTENT (type)")
  
  (function pixels
    "Accesses the (packed) pixel data array of the image.

This array includes necessary padding and potentially colors in indexed format.

See DECODE-PIXELS
See ENCODE-PIXELS
See BMP (type)
See BMPCONTENT (type)")
  
  (function type
    "Accesses the contained image type.

Can be either :ICO for an icon, or :CUR for a cursor.

See ICO (type)")
  
  (function entries
    "Accesses the vector of ICO-ENTRY instances.

See ICO (type)
See ICO-ENTRY (type)")
  
  (function images
    "Accesses the vector of BMPCONTENT instances.

See ICO (type)
See BMPCONTENT (type)")
  
  (function halftoning
    "Accesses the halftoning method of the image.

See OS22XBITMAPHEADER (type)
See BMP (type)
See BMPCONTENT (type)")
  
  (function decode-pixels
    "Decodes the pixel data from the bitmap.

Returns four values:
  OUTPUT      --- The packed pixel data as an octet vector.
  WIDTH       --- How many columns the image has.
  HEIGHT      --- How many rows the image has.
  BYTES/COLOR --- How many bytes (channels) per color are used.

This will resolve the colour palette and packed bit structure.
If OUTPUT is not passed, the PIXELS array may be returned verbatim
if it can be used directly. If OUTPUT is passed, it is filled with
the packed content and it is up to you to ensure that the array
has sufficient space to hold the data.

See BMP (type)
See BMPCONTENT (type)
See ENCODE-PIXELS
See READ-BMP")
  
  (function encode-pixels
    "Encode pixel data to the bitmap format.

Returns the altered bitmap.
This will alter the PIXELS slot on the bitmap with a new array that
holds the encoded and padded pixel data. If possible it will also
construct a colour palette and appropriately compress the pixel
data to fit.

See BMP (type)
See BMPCONTENT (type)
See DECODE-PIXELS
See WRITE-BMP"))
