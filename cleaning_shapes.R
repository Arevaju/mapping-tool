require(devtools)
install_github("cleangeo", "eblondel")
require(cleangeo)

report <- clgeo_CollectionReport(geo)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]

#try to clean data
mysp.clean <- clgeo_Clean(geo, print.log = TRUE)

#check if they are still errors
report.clean <- clgeo_CollectionReport(mysp.clean)
summary.clean <- clgeo_SummaryReport(report.clean)

mysp <- readShapePoly("nuts0.shp")

plot(mysp.clean, border= "lightgray")



ob <- SpatialPolygons(mysp.clean)# Your SpatialPolygons Object
spp <-SpatialPolygonsDataFrame(mysp.clean,data=as.data.frame("mysp.clean"))
writeOGR(mysp.clean,"shapes","nuts0v",driver="ESRI Shapefile",)

