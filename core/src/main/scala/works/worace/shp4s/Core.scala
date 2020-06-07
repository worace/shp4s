package works.worace.shp4s

import scodec._
import scodec.bits._
import codecs._
import scodec.codecs.literals
import scodec.Attempt.Successful
import shapeless.HNil

case class ShapefileHeader()

object Core {
  val headerint: Codec[Int] = int32
  val version: Codec[Unit] = constant(hex"e8030000")
  val emptyInt: Codec[Unit] = constant(hex"0000000")
  val fileCode: Codec[Unit] = constant(hex"000270a")
  val fileLength: Codec[Int] = int32
  val shapeType: Codec[Int] = int32L
  val xMin: Codec[Double] = doubleL
  val xMax: Codec[Double] = doubleL
  val yMin: Codec[Double] = doubleL
  val yMax: Codec[Double] = doubleL
  val zMin: Codec[Double] = doubleL
  val zMax: Codec[Double] = doubleL
  val mMin: Codec[Double] = doubleL
  val mMax: Codec[Double] = doubleL

  val header = fileCode :: emptyInt :: emptyInt ::
    emptyInt :: emptyInt :: emptyInt ::
    fileLength :: version :: shapeType ::
    xMin :: yMin :: xMax :: yMax :: zMin :: zMax ::
    mMin :: mMax

  case class Point(x: Double, y: Double)

  case class RecordHeader(shapeType: Int, length: Int)
  val recordHeader = (int32 :: int32).as[RecordHeader]
  val point = (constant(hex"1000000") :: doubleL :: doubleL).as[Point]

  // Header
  // Records:
  //   Record Header: Shape Type (int), Length (int)
  //   Record: Shape Type (int), Length#Bytes
  // Flat:
  // 10 x 16 = 160 bits; (4 + 8 + 8) x 8 = 160
  // 4               4                               4               8           8
  // 1 (Point Type), 10 (Point size - 16 bit words), 1 (Point Type), X (Double), Y (Double)
  def readHeader(bytes: Array[Byte]): Option[ShapefileHeader] = {
    println("header codec")
    println(header)
    println(BitVector(bytes))
    println(headerint.decode(BitVector(bytes)))

    for {
      h <- header.decode(BitVector(bytes))
      rec <- recordHeader.decode(h.remainder)
      p <- point.decode(rec.remainder)
      rec2 <- recordHeader.decode(p.remainder)
      p2 <- point.decode(rec2.remainder)
    } yield {
      println("header:")
      println(h)
      println("first rec:")
      println(rec)
      println("point contents:")
      println(p)

      println("rec2")
      println(rec2)
      println("point2")
      println(p2)
    }
    None
  }

  def hi: String = "hi"
}
