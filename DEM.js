var dataset = ee.Image("CGIAR/SRTM90_V4");
var elevation = dataset.select('elevation');
var dem = dataset.clip(geometry);
var aoi_dem = dem.select("elevation");
print(aoi_dem, "elevation");

Map.addLayer(aoi_dem, {min: 0, max: 60}, 'aoi_dem');
Map.centerObject(geometry);

Export.image.toDrive({
  image: dataset,
  description: 'aoi_dem',
  region: geometry,
  folder: 'rs',
  scale: 30,
  fileFormat: 'GeoTiff'
});
