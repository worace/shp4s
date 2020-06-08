package works.worace.shp4s

class DBFTest extends munit.FunSuite {
  test("DBF") {
    val i = new DBFIterator(TestFiles.dbf.path)
    // println(i.fields)
    println(i.next())
    i.size
  }
}
