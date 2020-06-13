package works.worace.shp4s

case class Feature(rowNumber: Int, shape: Shape, properties: Map[String, DBFValue])
