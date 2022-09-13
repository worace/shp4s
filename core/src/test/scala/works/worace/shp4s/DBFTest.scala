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

  test("census dbf") {
    val rows = DBFIterator(TestFiles.censusDbf.path).toVector
    assertEquals(rows.size, 565)
    val rowOne = Map(
      "COUNTYFP20" -> DBFString("010"),
      "ALAND20" -> DBFNumeric(0),
      "GEOID20" -> DBFString("600109501002014"),
      "NAME20" -> DBFString(value = "Block 2014"),
      "AWATER20" -> DBFNumeric(value = 5223471),
      "POP20" -> DBFNull,
      "INTPTLON20" -> DBFString(value = "-170.5856234"),
      "UATYPE20" -> DBFString(""),
      "INTPTLAT20" -> DBFString(value = "-14.2310969"),
      "TRACTCE20" -> DBFString(value = "950100"),
      "UACE20" -> DBFString(value = ""),
      "FUNCSTAT20" -> DBFString(value = "S"),
      "BLOCKCE20" -> DBFString(value = "2014"),
      "STATEFP20" -> DBFString(value = "60"),
      "UR20" -> DBFString(value = ""),
      "HOUSING20" -> DBFNull,
      "MTFCC20" -> DBFString(value = "G5040")
    )
    assertEquals(rows.head, rowOne)
  }
}
