package works.worace.shp4s

class DBFTest extends munit.FunSuite {
  test("DBF") {
    val rows = DBFIterator(TestFiles.dbf.path).toVector
    assertEquals(rows.size, 10)
    val rowOne = Map(
      "name" -> DBFString("New Swabia (historic)"),
      "featurecla" -> DBFString("Antarctic claim historic"),
      "sovereignt" -> DBFString("Germany"),
      "sov_a3" -> DBFString("DEU"),
      "scalerank" -> DBFNumeric(8),
      "map_color" -> DBFNumeric(1),
      "note" -> DBFString(""),
      "type" -> DBFString("Historic")
    )
    assertEquals(rows.head, rowOne)
  }
}
