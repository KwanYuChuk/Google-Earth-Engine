var point = ee.Geometry.Point([116.965037, -32.502597]);
// Import the Landsat 8 TOA image collection.
var l8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_TOA')
.filterBounds(geometry);

// Get the least cloudy image in 2024.
var image = ee.Image(
  l8.filterDate('2024-01-01', '2024-12-31')
    .sort('CLOUD_COVER')
    .first()
);
// Compute the Normalized Difference Vegetation Index (NDVI).
var nir = image.select('B5');
var red = image.select('B4');
var ndvi = nir.subtract(red).divide(nir.add(red)).rename('NDVI');

var ndviVis = {
  min: -1,
  max: 1,
  palette: [
    'blue', 'white', 'green'
  ],
};

// Display the result.
Map.centerObject(geometry, 12);
Map.addLayer(ndvi.clip(geometry), ndviVis, 'NDVI image');

Export.image.toDrive
({
  image: ndvi,
  description: 'landsat_ndvi_rf',
  scale: 30,
  region: geometry,
  folder: 'rs',
  maxPixels: 1e10
});