package myparser

import java.util.NoSuchElementException
import java.util.zip.Inflater

/**
  * Created by ThomasE on 22.02.2016.
  */
abstract class PDFObject {

  def toInt: Long = this match {
    case PDFInteger(n) => n
    case _ => sys.error("no Integer")
  }

  def value(): Any = this match {
    case PDFInteger(va) => va
    case PDFDouble(va) => va
    case PDFString(va) => va
    case PDFBoolean(va) => va
    case PDFName(va) => va
    case _ => sys.error("No value for type: " + this + "[getClass]")
  }

  def get(key: String): PDFObject = this match {
    case PDFDict() => try {
      asInstanceOf[PDFDict].map(key)
    } catch {
      case _: Throwable => null
    }
    case _ => sys.error("not a dictionary " + this )
  }

  def get(index: Int): PDFObject = this match {
    case PDFList() => asInstanceOf[PDFList].list(index)
    case _ => sys.error("Not a list")
  }

  def isStreamObject(): Boolean = {
    return isInstanceOf[PDFDict] && asInstanceOf[PDFDict].get("Length") != null
  }

  def print(): Unit = {
    println(this)
  }
}

case class PDFInteger(value_ : Long) extends PDFObject {}

case class PDFDouble(value_ : Double) extends PDFObject {
}

case class PDFString(value_ : String) extends PDFObject {
}

case class PDFBoolean(value_ : String) extends PDFObject {}

case class PDFName(value_ : Any) extends PDFObject {}

case class PDFDict() extends PDFObject {
  var map = collection.mutable.Map[String, PDFObject]()

  def update(pDFName: String, pDFObject: PDFObject): Unit = {
    map(pDFName) = pDFObject
  }

  override def toString: String = {
    val buf = new StringBuilder()
    map.keys.foreach(i => buf.append(i + " " + map(i)))
    buf.toString
  }
}

case class PDFList() extends PDFObject {
  var list: List[PDFObject] = List()

  def +(obj: PDFObject): Unit = {
    list = list :+ obj
  }
}

case class PDFRef(id_ : Int, rev_ : Int) extends PDFObject {
  val id = id_
  val rev = rev_
}

case class PDFStream(dict : PDFDict, position: Long, buf: DataBuffer) extends PDFObject {

  override def get(key : String): PDFObject = {
    return dict.get(key)
  }

  def getContent: Array[Byte] = {
    val length = dict.get("Length")
    val bytes = new Array[Byte](length.toInt.toInt)
    buf.read(bytes)
    val filter = dict.get("Filter")
    if (filter == null) {
      return bytes
    } else if (!"FlateDecode".equals(filter.value())) {
      throw new Exception("Cannot handle decoding: " + filter.toString)
    }
    val inflater = new Inflater()
    inflater.setInput(bytes)
    val size = 1024
    var deflated = size
    var content = new Array[Byte](0)
    while (size == deflated) {
      val b = new Array[Byte](size)
      deflated = inflater.inflate(b)
      val newContent = new Array[Byte](content.length + deflated)
      System.arraycopy(content, 0, newContent, 0, content.length)
      System.arraycopy(b, 0, newContent, content.length, deflated)
      content = newContent
    }
    content
  }
}




