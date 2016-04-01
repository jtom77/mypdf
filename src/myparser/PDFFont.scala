package myparser

import java.io.{BufferedOutputStream, FileOutputStream}

object PDFFontFactory {
  def createFont(file_ : PDFile, fontDict_ : PDFObject): PDFFont = {
    val typ = fontDict_.get("Subtype").value()
    if (typ.equals("Type0")) {
      return new Type0Font(file_, fontDict_)
    }
    if (typ.equals("Type1")) {
      return new Type0Font(file_, fontDict_)
    }
    if (typ.equals("Type3")) {
      return new Type3Font(file_, fontDict_)
    }
    if (typ.equals("TrueType")) {
      return new TrueTypeFont(file_, fontDict_)
    }
    throw new Exception("Unknown font type: " + typ)
  }
}

/**
  * Represents a pdf font object
  *
  * @param file_     The pdf file this font belongs to
  * @param fontDict_ The Font dictionary
  */
class PDFFont(file_ : PDFile, fontDict_ : PDFObject) {

  var file = file_
  var fontDict = fontDict_
  val ucMap = getUnicodeMap();

  /**
    * Get the font program as a stream of bytes
    *
    * @return the font program as a stream of bytes
    */
  def getContent(): Array[Byte] = {
    val fontDescriptor = file.dereference(fontDict.get("FontDescriptor")).asInstanceOf[PDFDict]
    var content = fontDescriptor.get("FontFile")
    if (content == null) {
      content = fontDescriptor.get("FontFile2")
    }
    if (content == null) {
      content = fontDescriptor.get("FontFile3")
    }
    if (content == null) {
      return null
    }

    return file.getStreamContent(content.asInstanceOf[PDFRef])
  }


  def getUnicodeMap(): Map[Int,Int] = {
    val umap = file.dereference(fontDict.get("ToUnicode"))
    if (umap == null || !umap.isInstanceOf[PDFStream]) {
      return null
    }
    var ucMap = Map[Int, Int]()
    val content = umap.asInstanceOf[PDFStream].getContent
    content.foreach(b => print(b.toChar))
    val parser = new PDFParser(new ByteDataBuffer(content))
    var tok = parser.nextToken();

    def parseHex(): Int = {
      tok = parser.nextToken()
      if (tok.equals("endbfchar") || tok.equals("endbfrange")) {
        return -1
      }
      return Integer.parseInt(tok.toString(), 16)
    }
    def findKeyWord: String = {
      while (!("beginbfchar".equals(tok) || "beginbfrange".equals(tok) || "endcmap".equals(tok))) {
        tok = parser.nextToken()
      }
      return tok.toString
    }

    def parseCharEntry(): Unit = {
      var key = parseHex()
      while (key != -1) {
        ucMap += (key -> parseHex())
        key = parseHex()
      }
    }

    def parseRangeEntry(): Unit = {
      var start = parseHex()
      while (start != -1) {
        val end = parseHex()
        val value = parseHex()
        Range(start, end).foreach(i => ucMap += (i -> value))
      }
    }

    var keyWord: String = null;
    while (!(keyWord = findKeyWord).equals("endcmap")) {
      if ("beginbfchar".equals(keyWord)) {
        println("========> " + tok)
        parseCharEntry()
      } else {
        parseRangeEntry()
      }
    }
    return ucMap
  }

  /**
    * Serialise the font program in a font file
    *
    * @param path the path to store the font file
    */
  def toFile(path: String): Unit = {}
}


class TrueTypeFont(file_ : PDFile, fontDict_ : PDFObject) extends PDFFont(file_, fontDict_) {
  override def toFile(path: String): Unit = {
    val byteArray = getContent()
    val bos = new BufferedOutputStream(new FileOutputStream(path))
    Stream.continually(bos.write(byteArray))
    bos.close
  }
}

class Type1Font(file_ : PDFile, fontDict_ : PDFObject) extends PDFFont(file_, fontDict_) {
  override def toFile(path: String): Unit = {
    val byteArray = getContent()
    val bos = new BufferedOutputStream(new FileOutputStream(path))
    Stream.continually(bos.write(byteArray))
    bos.close
  }
}

class Type3Font(file_ : PDFile, fontDict_ : PDFObject) extends PDFFont(file_, fontDict_) {
}

class Type0Font(file_ : PDFile, fontDict_ : PDFObject) extends PDFFont(file_, fontDict_) {

  val descendendantFonts = file.dereference(fontDict_.get("DescendantFonts")).asInstanceOf[PDFList].list.map(x => new CIDFont(file_, file_.dereference(x)))

  override def getContent(): Array[Byte] = {
    return descendendantFonts.map(i => i.getContent()).reduceLeft((a, b) => {
      val c = new Array[Byte](a.length + b.length)
      System.arraycopy(a, 0, c, 0, a.length)
      System.arraycopy(b, 0, c, a.length, b.length)
      return c
    })
  }
}

class CIDFont(file_ : PDFile, fontDict_ : PDFObject) extends PDFFont(file_, fontDict_) {
  val cidSystemInfo = fontDict_.get("CIDSystemInfo").asInstanceOf[PDFDict]
}


class ToUnicodeMap(pDFParser: PDFParser) {


}



