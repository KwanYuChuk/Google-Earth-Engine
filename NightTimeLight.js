var dataset = ee.ImageCollection('BNU/FGS/CCNL/v1')
                  .filter(ee.Filter.date('2013-01-01', '2013-01-31'))
                  .map(function(image){return image.clip(geometry)})
                  .median();;
var nighttimeLights2013 = dataset.select('b1');
var nighttimeLightsVis = {
  min: 3.0,
  max: 60.0,
};
Map.setCenter(115.8617, -31.9514, 6);
Map.addLayer(nighttimeLights2013, nighttimeLightsVis, 'Nighttime Lights');

Export.image.toDrive
({
  image: nighttimeLights2013,
  description: 'nighttimeLights2013',
  scale: 1000,
  region: geometry,
  folder: 'rs',
  maxPixels: 1e10
});