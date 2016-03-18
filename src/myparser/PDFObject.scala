package myparser

import java.util.NoSuchElementException

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
    case _ => sys.error("not a dictionary")
  }

  def get(index: Int): PDFObject = this match {
    case PDFList() => asInstanceOf[PDFList].list(index)
    case _ => sys.error("Not a list")
  }

  def print(): Unit = {
    println(this)
  }
}

case class PDFInteger(value_ : Long) extends PDFObject {
}

case class PDFDouble(value_ : Double) extends PDFObject {
}

case class PDFString(value_ : String) extends PDFObject {
}

case class PDFBoolean(value_ : String) extends PDFObject {
}

case class PDFName(value_ : Any) extends PDFObject {
}

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


