var point = ee.Geometry.Point([116.965037, -32.502597]);
var dataset = ee.ImageCollection('MODIS/061/MOD09A1')
                  .filter(ee.Filter.date('2024-01-01', '2024-12-31'))
                  .filterBounds(geometry);
var trueColor =
    dataset.select(['sur_refl_b01', 'sur_refl_b04', 'sur_refl_b03']);
var mean = trueColor.mean();
var trueColorVis = {
  min: -100.0,
  max: 3000.0,
};

Map.addLayer(mean.clip(geometry), trueColorVis, 'True Color');
Map.setCenter(116.965037, -32.502597, 12);

// export to drive
Export.image.toDrive
({
  image: mean,
  description: 'modis_mean_rf',
  scale: 500,
  region: geometry,
  folder: 'rs',
  maxPixels: 1e10
});