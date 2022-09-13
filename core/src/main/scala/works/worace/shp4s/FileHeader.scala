package works.worace.shp4s

import scodec._
import scodec.bits._
import scodec.codecs._
import shapeless.HNil
import shapeless.::

case class FileHeader(
  length: Int, shapeType: Int, xMin: Double, xMax: Double, yMin: Double, yMax: Double,
  zMin: Double, zMax: Double, mMin: Double, mMax: Double
)

object FileHeader extends ShpCodec[FileHeader] {
  private val fileCode: Codec[Unit] = constant(hex"000270a")
  private val emptyInt: Codec[Unit] = constant(hex"0000000")
  private val version: Codec[Unit] = constant(hex"e8030000")
  private val fileLength: Codec[Int] = int32
  private val shapeType: Codec[Int] = int32L
  private val xMin: Codec[Double] = doubleL
  private val xMax: Codec[Double] = doubleL
  private val yMin: Codec[Double] = doubleL
  private val yMax: Codec[Double] = doubleL
  private val zMin: Codec[Double] = doubleL
  private val zMax: Codec[Double] = doubleL
  private val mMin: Codec[Double] = doubleL
  private val mMax: Codec[Double] = doubleL

  val codec = (fileCode :: emptyInt :: emptyInt ::
    emptyInt :: emptyInt :: emptyInt ::
    fileLength :: version :: shapeType ::
    xMin :: yMin :: xMax :: yMax :: zMin :: zMax ::
    mMin :: mMax).xmap(
    { case (_ :: _ :: _ :: _ :: _ :: _ :: length :: _ ::
      shapeType :: xMin :: yMin :: xMax :: yMax :: zMin :: zMax :: mMin :: mMax :: HNil) =>
      FileHeader(length, shapeType, xMin, xMax, yMin, yMax, zMin, zMax, mMin, mMax)
    },
      (h: FileHeader) => () :: () :: () :: () :: () :: () :: h.length :: () ::
        h.shapeType ::
        h.xMin :: h.yMin :: h.xMax :: h.yMax :: h.zMin :: h.zMax :: h.mMin :: h.mMax :: HNil
  )
}
