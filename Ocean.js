var dataset = ee.ImageCollection('NASA/OCEANDATA/MODIS-Terra/L3SMI')
                  .filterDate('2016-01-01', '2016-01-31')
                  .map(function(image){return image.clip(geometry)})
                  .median();
var remoteSensingReflectance =
    dataset.select(['poc']);
var remoteSensingReflectanceVis = {
  min: 0.0,
  max: 0.02,
};
Map.setCenter(120, -30, 4);
Map.addLayer(
    remoteSensingReflectance, remoteSensingReflectanceVis,
    'Remote Sensing Reflectance');

Export.image.toDrive
({
  image: dataset,
  description: 'dataset_ocean',
  scale: 1000,
  region: geometry,
  folder: 'rs',
  maxPixels: 1e10
});