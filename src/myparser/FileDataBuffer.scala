package myparser

import java.io.{File, RandomAccessFile}


trait DataBuffer {
  def position: Long
  def read(): Byte
  def seek(): Byte
  def gotoEnd(): Unit
  def goto(pos: Long): Unit
  def read(bytes: Array[Byte]): Unit
  def rewind(): Unit
}

class ByteDataBuffer(bytes : Array[Byte]) extends DataBuffer {

  var position = 0L
  val limit = bytes.length

  override def read(): Byte = {
    position += 1
    return seek()
  }

  override def goto(pos: Long): Unit = {
    position = pos.toInt
  }

  override def seek(): Byte = {
    if(position >= limit) {
      throw new Exception("Reached end of File!")
    }
    return bytes(position.toInt)
  }

  override def read(dest: Array[Byte]): Unit = {
    System.arraycopy(bytes, position.toInt, dest, 0, dest.length)
  }

  override def gotoEnd(): Unit = {
    goto(limit)
  }

  override def rewind(): Unit = {
    goto(position - 1)
  }
}


/**
  * Created by ThomasE on 01.04.2016.
  */
class FileDataBuffer(path: String) extends DataBuffer {

  val ra = new RandomAccessFile(new File(path), "r")
  var position = 0L;

  def read(): Byte = {
    val n = ra.read()
    if (n == -1) {
      throw new Exception("Reached End of File!")
    }
    position += 1
    return n.toByte
  }

  def seek(): Byte = {
    val n = ra.read()
    if (n == -1) {
      throw new Exception("Reached End of File!")
    }
    ra.seek(position)
    return n.toByte
  }

  def gotoEnd(): Unit = {
    goto(ra.length())
  }

  def goto(_position: Long): Unit = {
    position = _position
    ra.seek(position)
  }

  def read(bytes: Array[Byte]): Unit = {
    ra.read(bytes)
    position += bytes.length
  }

  def rewind(): Unit = {
    goto(position - 1)
  }
}
