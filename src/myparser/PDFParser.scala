package myparser

import java.awt.image.DataBuffer
import java.util.zip.Inflater

/**
  * Created by ThomasE on 22.02.2016.
  */
class PDFParser(path : String) {

  val buf = new DataBuffer(path)

  def goto(position : Long): Unit = {
    buf.goto(position)
  }

  def isWhiteSpace(b: Byte): Boolean = {
    return b == 0x00 || b == 0x09 || b == 0x0a || b == 0x0c || b == 0x0d || b == 0x20
  }

  def isCharacter(b: Byte): Boolean = {
    return b >= 0x41 && b <= 0x5a || b >= 0x61 && b <= 0x7a
  }

  def isDigit(b: Byte): Boolean = {
    return b >= 0x30 && b <= 0x39
  }

  def skipWhiteSpace(): Unit = {
    var b = buf.read()
    while(isWhiteSpace(b)) {
      b = buf.read()
    }
    buf.rewind()
  }

  def gotoEOL(): Unit = {
    var b = buf.read()
    while( b != 0x0a ) {
      b = buf.read()
    }
  }

  def parseName(c: Byte): String = {
    var b = c
    val strBuilder = new StringBuilder()
    while(isCharacter(b)) {
      strBuilder.append(b.toChar)
      b = buf.read()
    }
    buf.rewind()
    return strBuilder.toString
  }


  def parseString(c : Byte): Unit = {
    val end = if(c==0x28) 0x29 else 0x3e
    var cnt = 1
    var b = buf.read()
    val strBuffer = new StringBuilder
    while(true) {
      if(b == c) {
        cnt += 1
      } else if(b == end) {
        cnt -= 1
        if(cnt == 0) {
          return strBuffer.toString()
        }
      } else {
        strBuffer.append(b.toChar)
      }
      b = buf.read()
    }
    throw new Exception("End of String reached")
  }


  def parseInteger(c: Byte): Long = {
    var b = c
    var strBuilder = new StringBuilder()
    while(isDigit(b)) {
      strBuilder.append(b.toChar)
      b = buf.read()
    }
    buf.rewind()
    return strBuilder.toString.toLong
  }

  def parseDouble(c: Byte): Double = {
    var b = c
    var strBuilder = new StringBuilder()
    while(isDigit(b) || b == 0x2e) {
      strBuilder.append(b.toChar)
      b = buf.read()
    }
    buf.rewind()
    return strBuilder.toString.toDouble
  }

  def nextToken(): Any = {
    skipWhiteSpace()
    val b = buf.read()
    if(b.toChar == '%') {
      gotoEOL()
      return nextToken()
    } else if(b.toChar == '<') {
      val c = buf.read()
      if(c == '<') {
        return "<<"
      } else {
        buf.rewind()
        return parseString(b)
      }
    } else if(b.toChar == '>') {
      var c = buf.read()
      if(c == '>') {
        return ">>"
      } else {
        buf.rewind()
        return ">"
      }
    } else if(isDigit(b) || b == 0x2d) {
      return parseInteger(b)
    } else if(isCharacter(b)) {
      return parseName(b)
    } else {
      return b.toChar.toString
    }
  }

  def parseObject(): PDFObject = {
    val tok = nextToken()
    if(tok.equals("[")) {
      return parseList()
    } else if(tok.equals("<<")) {
      return parseDict()
    } else if(tok.equals("/")) {
      return new PDFName(parseName(buf.read()))
    } else if(tok.equals(">>") || tok.equals("]")) {
      return null
    } else if(tok.isInstanceOf[String]) {
      return new PDFString(tok.toString)
    } else if(tok.isInstanceOf[Long]) {
      val num = tok.asInstanceOf[Long]
      val position = buf.position
      val rev = nextToken()
      if(rev.isInstanceOf[Long]) {
        val next = nextToken()
        if (next.equals("R")) {
          val ref = new PDFRef(num.toInt, rev.asInstanceOf[Long].toInt)
          return ref
        } else if (next.equals("obj")) {
          return parseObject()
        }
      }
      buf.goto(position)
      return new PDFInteger(tok.asInstanceOf[Long])
    } else {
      return null
    }
  }

  def parseList(): PDFList = {
    val list = new PDFList
    var obj = parseObject()
    while(obj != null) {
      list + obj
      obj = parseObject()
    }
    return list
  }

  def parseDict(): PDFDict = {
    val dict = new PDFDict
    var name = parseObject()
    while(name != null) {
      dict.update(name.value.toString, parseObject())
      name = parseObject()
    }
    return dict
  }

  def findTrailer(): PDFDict = {
    var b = buf.read()
    while(true) {
      if(b.toChar == 't') {
        val str = parseName(b)
        if(str.equals("trailer"))
          return parseObject().asInstanceOf[PDFDict]
      }
      b = buf.read()
    }
    throw new Exception("No Trailer found")
  }

  def findXref(): Long = {
    var b = buf.read()
    while(true) {
      if(b.toChar == 's' && parseName(b).equals("startxref")) {
        return nextToken().asInstanceOf[Long]
      }
      b = buf.read()
    }
    throw new Exception("No Trailer found")
  }

  def parseXref(): Array[Long] = {
    buf.goto(findXref())
    var next = nextToken()
    next = nextToken()
    next = nextToken()
    val size = next.asInstanceOf[Long].toInt
    val result = new Array[Long](size)
    for(i <- 0 until size) {
      result(i) = nextToken().asInstanceOf[Long]
      nextToken()
      nextToken()
    }
    return result
  }


  def getContent: Array[Byte] = {
    val dict = parseObject()
    val length = dict.get("Length")
    val streamKey = nextToken()
    if(!streamKey.equals("stream")) {
      throw new Exception("Keyword stream expected")
    }
    skipWhiteSpace()
    val bytes = new Array[Byte](length.toInt.toInt)
    buf.read(bytes)
    val inflater = new Inflater()
    inflater.setInput(bytes)
    val size = 1024
    var deflated = size
    var content = new Array[Byte](0)
    while(size == deflated) {
      var b = new Array[Byte](size)
      deflated = inflater.inflate(b)
      var newContent = new Array[Byte](content.length + deflated)
      System.arraycopy(content, 0, newContent, 0, content.length)
      System.arraycopy(b, 0, newContent, content.length, deflated)
      content = newContent
    }
    return content
  }
}
