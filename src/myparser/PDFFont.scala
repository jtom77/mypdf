package myparser

import java.io.{BufferedOutputStream, FileOutputStream}

/**
  * Represents a pdf font object
  * @param file_ The pdf file this font belongs to
  * @param obj_ The Font dictionary
  */
class PDFFont(file_ : PDFile, obj_ : PDFObject) {

  var file = file_
  var obj = obj_
  var type_ = obj.get("Subtype").value

  /**
    * Get the font program as a stream of bytes
    * @return the font program as a stream of bytes
    */
  def getContent(): Array[Byte] = {
    if(type_.equals("Type0")) {
      val descendantFonts = file.dereference(obj.get("DescendantFonts")).asInstanceOf[PDFList]
      return new PDFFont(file_, file.dereference(descendantFonts.get(0))).getContent()
    }
    val fontDescriptor = file.dereference(obj.get("FontDescriptor")).asInstanceOf[PDFDict]
    var content = fontDescriptor.get("FontFile")
    if(content == null) {
      content = fontDescriptor.get("FontFile2")
    }
    if(content == null) {
      content = fontDescriptor.get("FontFile3")
    }
    if(content == null) {
      return null
    }
    return file.getStreamContent(content.asInstanceOf[PDFRef])
  }

  /**
    * Serialise the font program in a font file
    * @param path the path to store the font file
    */
  def toFile(path : String): Unit = {
    val byteArray = getContent()
    val bos = new BufferedOutputStream(new FileOutputStream(path))
    Stream.continually(bos.write(byteArray))
    bos.close
  }
}




