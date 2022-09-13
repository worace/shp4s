### Notes / TODO

Types - 14
* [X] NullShape
* [X] Point
* [X] PolyLine
* [X] Polygon
* [X] MultiPoint
* [X] PointZ
* [x] PolylineZ
* [x] PolygonZ
* [X] MultiPointZ
* [x] PointM
* [x] PolyLineM
* [x] PolygonM
* [x] MultiPointM
* [ ] MultiPatch
* [X] DBF
* [ ] DBF proper resource handling (cats bracket? closing in finally?)

TODO Edge Cases

* [x] Verify PointZ file with M values
* [ ] Verify MultiPointZ with M values
* [x] Convert MultiPointZ to hold Vector[PointZ] values
* [x] PolylineZ with M Values
* [ ] *-Z encoding with empty M values -- should omit entirely rather than encoding 0's
* [x] PolyLineZ Sample File
* [ ] Import test cases from gdal https://github.com/OSGeo/gdal/tree/master/autotest/ogr/data/shp
