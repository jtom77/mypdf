package myparser

import java.io.{File, RandomAccessFile}

/**
  * Created by ThomasE on 22.02.2016.
  */
class PDFile(path : String) {

  val parser = new PDFParser(path)
  val trailer = parser.findTrailer()
  val xref = parser.parseXref()
  val catalog = dereference(trailer.get("Root"))
  val pagesRoot = dereference(catalog.get("Pages"))
  val numberOfPages = pagesRoot.get("Count").toInt.toInt

  def getObject(ref: PDFRef): PDFObject = {
    parser.goto(xref(ref.id))
    return parser.parseObject()
  }

  def getObject(id: Int): PDFObject = {
    parser.goto(xref(id))
    return parser.parseObject()
  }

  def dereference(obj: PDFObject): PDFObject = obj match {
    case PDFRef(id, rev) => dereference(getObject(id))
    case _ => obj
  }

  def getPage(page: Int): PDFPage = {

    // todo inherit resources

    def huntDown(pages: PDFObject, _start: Int): PDFPage = {
      if (pages.get("Type").value().equals("Page")) {
        return new PDFPage(this, pages)
      }
      var start = _start
      val kids = dereference(pages.get("Kids")).asInstanceOf[PDFList]
      for (i <- 0 until kids.list.length) {
        val kid = dereference(kids.get(i))
        var count = 1
        val countItem = kid.get("Count")
        if (countItem != null) {
          count = countItem.toInt().toInt
        }
        if (start + count > page) {
          return huntDown(kid, start)
        }
        start += count
      }
      return null
    }
    return huntDown(pagesRoot, 0)
  }
}

class PDFPage(file_ : PDFile, obj_ : PDFObject) {

  val file = file_
  val obj = obj_
  val fontMap = Map[String, PDFFont]()

  def getContent(): Array[Byte] = {
    val streamRef = obj.get("Contents").asInstanceOf[PDFRef]
    file.parser.goto(file.xref(streamRef.id))
    return file.parser.getContent
  }

  def registerFonts(): Unit = {
    val resources = file.dereference(obj.get("Resources"))
    val fonts = resources.get("Font").asInstanceOf[PDFDict]
  }

  def printContent(): Unit = {
    getContent().foreach(b => print(b.toChar))
  }
}


class PDFFont(file_ : PDFile, obj_ : PDFObject) {

  var file = file_
  var obj = obj_

  def getContent(): Array[Byte] = {
    val streamRef = obj.get("FontDescriptor").asInstanceOf[PDFRef]
    file.parser.goto(file.xref(streamRef.id))
    return file.parser.getContent
  }
}


class DataBuffer(path : String) {

  val ra = new RandomAccessFile(new File(path), "r")
  var position = 0L;

  def read(): Byte = {
    val n = ra.read()
    if(n == -1) {
      throw new Exception("Reached End of File!")
    }
    position += 1
    return n.toByte
  }

  def seek(): Byte = {
    val n = ra.read()
    if(n == -1) {
      throw new Exception("Reached End of File!")
    }
    ra.seek(position)
    return n.toByte
  }

  def goto(_position : Long): Unit = {
    position = _position
    ra.seek(position)
  }

  def read(bytes : Array[Byte]): Unit = {
    ra.read(bytes)
    position += bytes.length
  }

  def rewind(): Unit = {
    goto(position - 1)
  }
}




object Main {


  def main(args : Array[String]): Unit = {
    var file = new PDFile("C:/Users/thomase/Documents/samplepdf/begraben.pdf")
    file.getPage(9).printContent()
  }
}