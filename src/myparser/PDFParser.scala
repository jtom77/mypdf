package myparser

import java.util.zip.Inflater

/**
  * Created by ThomasE on 22.02.2016.
  */
class PDFParser(buf_ : DataBuffer) {

  var buf = buf_;

  def goto(position: Long): Unit = {
    buf.goto(position)
  }

  def isWhiteSpace(b: Byte): Boolean = {
    b == 0x00 || b == 0x09 || b == 0x0a || b == 0x0c || b == 0x0d || b == 0x20
  }

  def isCharacter(b: Byte): Boolean = {
    b >= 0x41 && b <= 0x5a || b >= 0x61 && b <= 0x7a
  }

  def isDigit(b: Byte): Boolean = {
    b >= 0x30 && b <= 0x39
  }

  def isDelimiter(b: Byte): Boolean = {
    val c = b.toChar
    c == '/' || c == '(' || c == ')' || c == '<' || c == '>' || c == '[' || c == ']' || c == '{' || c == '}' || c == '|' || c == '%'
  }

  def skipWhiteSpace(): Unit = {
    var b = buf.read()
    while (isWhiteSpace(b)) {
      b = buf.read()
    }
    buf.rewind()
  }

  def gotoEOL(): Unit = {
    var b = buf.read()
    while (b != 0x0a) {
      b = buf.read()
    }
  }

  /**
    * Parse a pdf name.
    *
    * @param c The first character of the name content (the '/' is already consumed)
    * @return the pdf name as String
    */
  def parseName(c: Byte): String = {
    if (c.toChar == '(') {
      return parseString(c)
    }
    var b = c
    val strBuilder = new StringBuilder()
    while (!isWhiteSpace(b) && !isDelimiter(b)) {
      strBuilder.append(b.toChar)
      b = buf.read()
    }
    buf.rewind()
    strBuilder.toString
  }


  /**
    * Parse a PDF String
    *
    * @param c the opening delimiter, either '(' or '<'
    * @return
    */
  def parseString(c: Byte): String = {
    val end = if (c == 0x28) 0x29 else 0x3e
    var balance = 1
    var b = buf.read()
    val strBuffer = new StringBuilder
    while (true) {
      if (b == c) {
        balance += 1
      } else if (b == end) {
        balance -= 1
        if (balance == 0) {
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
    var firstTime = true
    var b = c
    var strBuilder = new StringBuilder()
    while (isDigit(b) || (firstTime && b == 0x2d)) {
      strBuilder.append(b.toChar)
      b = buf.read()
      firstTime = false
    }
    buf.rewind()
    strBuilder.toString.toLong
  }

  def parseDouble(c: Byte): Double = {
    var b = c
    val strBuilder = new StringBuilder()
    while (isDigit(b) || b == 0x2e) {
      strBuilder.append(b.toChar)
      b = buf.read()
    }
    buf.rewind()
    strBuilder.toString.toDouble
  }

  def nextToken(): Any = {
    skipWhiteSpace()
    val b = buf.read()
    if (b.toChar == '%') {
      gotoEOL()
      nextToken()
    } else if (b.toChar == '<') {
      val c = buf.read()
      if (c == '<') "<<"
      else {
        buf.rewind();
        parseString(b)
      }
    } else if (b.toChar == '>') {
      val c = buf.read()
      if (c == '>') ">>"
      else {
        buf.rewind();
        ">"
      }
    } else if (isDigit(b) || b == 0x2d) {
      parseInteger(b)
    } else if (isCharacter(b)) {
      parseName(b)
    } else {
      b.toChar.toString
    }
  }

  def parseObject(): PDFObject = {
    val tok = nextToken()
    if (tok.equals("[")) {
      return parseList()
    } else if (tok.equals("<<")) {
      val dict = parseDict()
      val position = buf.position
      if(nextToken().equals("stream")) {
        skipWhiteSpace()
        return new PDFStream(dict, buf.position, buf)
      } else {
        goto(position)
        return dict
      }
    } else if (tok.equals("/")) {
      return new PDFName(parseName(buf.read()))
    } else if (tok.equals("(")) {
      return new PDFString(parseString('('.toByte))
    } else if (tok.equals(">>") || tok.equals("]")) {
      return null
    } else tok match {
      case _: String =>
        return new PDFString(tok.toString)
      case num: Long =>
        val position = buf.position
        val tok2 = nextToken()
        tok2 match {
          case l: Long =>
            val tok3 = nextToken()
            if (tok3.equals("R")) {
              return PDFRef(num.toInt, l.toInt)
            } else if (tok3.equals("obj")) {
              return parseObject()
            }
          case _ => new Exception("Syntax Error: 'R' or 'obj' expected")
        }
        buf.goto(position)
        return new PDFInteger(num)
      case _ =>
        return null
    }
  }

  def parseList(): PDFList = {
    val list = new PDFList
    var obj = parseObject()
    while (obj != null) {
      list + obj
      obj = parseObject()
    }
    list
  }

  def parseDict(): PDFDict = {
    val dict = new PDFDict
    var name = parseObject()
    while (name != null) {
      val value = parseObject()
      dict.update(name.value().toString, value)
      name = parseObject()
    }
    return dict
  }

  def findFirstXref(): Long = {
    buf.gotoEnd()
    while (true) {
      buf.rewind()
      val b = buf.seek()
      if (b.toChar == 's') {
        val position = buf.position
        buf.read()
        if (parseName('s').equals("startxref")) {
          return nextToken().asInstanceOf[Long]
        } else {
          buf.goto(position)
        }
      }
    }
    throw new Exception("No Trailer found")
  }

  def getContent: Array[Byte] = {
    val dict = parseObject()
    val length = dict.get("Length")
    val streamKey = nextToken()
    if (!streamKey.equals("stream")) {
      throw new Exception("Keyword stream expected")
    }
    skipWhiteSpace()
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
