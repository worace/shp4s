package works.worace.shp4s

class UtilTest extends munit.FunSuite {
  test("offsetSlices") {
    assertEquals(
      Util.offsetSlices(Vector[Int](), Vector(0)),
      Vector(Vector())
    )
    assertEquals(
      Util.offsetSlices(Vector[Int](1), Vector(0)),
      Vector(Vector(1))
    )
    assertEquals(
      Util.offsetSlices(Vector[Int](1, 2, 3), Vector(0)),
      Vector(Vector(1, 2, 3))
    )
    assertEquals(
      Util.offsetSlices(Vector[Int](1, 2, 3, 4, 5), Vector(0, 2)),
      Vector(Vector(1, 2), Vector(3, 4, 5))
    )
    assertEquals(
      Util.offsetSlices(Vector[Int](1, 2, 3, 4, 5), Vector(0, 2, 3)),
      Vector(Vector(1, 2), Vector(3), Vector(4, 5))
    )
  }

  test("sliceIndices".only) {
    assertEquals(Util.sliceIndices(Vector()), Vector())
    assertEquals(Util.sliceIndices(Vector(Vector(1))), Vector((0, 1)))
    assertEquals(Util.sliceIndices(Vector(Vector(1, 2, 3))), Vector((0, 3)))
    assertEquals(
      Util.sliceIndices(Vector(Vector(1, 2), Vector(3, 4))),
      Vector((0, 2), (2, 4))
    )
    assertEquals(
      Util.sliceIndices(Vector(Vector(1, 2), Vector(3, 4), Vector(5), Vector(6))),
      Vector((0, 2), (2, 4), (4, 5), (5, 6))
    )
    assertEquals(
      Util.sliceIndices(Vector(Vector(1, 2), Vector(3, 4), Vector(), Vector(6))),
      Vector((0, 2), (2, 4), (4, 4), (4, 5))
    )
  }

  test("zippedWithFlatVec") {
    val ident = (a: Int, b: Int) => (a, b)
    assertEquals(
      Util.zippedWithFlatVec(Vector(Vector(1)), Vector(2))(ident),
      Vector(Vector((1, 2)))
    )
    assertEquals(
      Util.zippedWithFlatVec(Vector(Vector(1, 2, 3)), Vector(9, 8, 7))(ident),
      Vector(Vector((1, 9), (2, 8), (3, 7)))
    )
    assertEquals(
      Util.zippedWithFlatVec(Vector(Vector(1), Vector(2, 3)), Vector(9, 8, 7))(ident),
      Vector(Vector((1, 9)), Vector((2, 8), (3, 7)))
    )
  }
}
