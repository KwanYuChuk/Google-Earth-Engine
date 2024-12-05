// load image
var imageCollection = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
.filterBounds(geometry);

// filter by year and month
var img = imageCollection.filter(ee.Filter.calendarRange(2015,2024,'year'))
.filter(ee.Filter.calendarRange(4,111,'month'));

// reduce to mean
var mean = img.mean();

// visualize image
var sentinelVis = {
  min: 0,
  max: 2500,
  bands: ['B8', 'B4', 'B2'],
};
Map.addLayer(mean.clip(geometry), sentinelVis, 'aoi');
Map.centerObject(geometry, 12);

// export to drive
Export.image.toDrive
({
  image: mean,
  description: 'sentinel_mean_b8b4b2',
  scale: 10,
  region: geometry,
  folder: 'rs',
  maxPixels: 1e10
});