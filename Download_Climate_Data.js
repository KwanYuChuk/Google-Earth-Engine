print(geometry);
Map.addLayer(geometry);

var early = ('1988');
var late =  ('2025');

// Load in image collection and filter by area and date
var era5_dat = ee.ImageCollection('ECMWF/ERA5_LAND/MONTHLY_AGGR')
.filterDate(early,late) //filter years of interest 
.select('total_precipitation_sum') //
.map(function(image){return image.clip(geometry)}); //Clips data based on 'aoi'

print('collection', era5_dat);

//Create variables and extract data
var scale = era5_dat.first().projection().nominalScale().multiply(0.05); print(scale);
era5_dat = era5_dat.filter(ee.Filter.listContains('system:band_names', era5_dat.first().bandNames().get(0)));

var ft = ee.FeatureCollection(ee.List([]));
//Function to extract values from image collection based on point file and export as a table 
var fill = function(img, ini) {
  var inift = ee.FeatureCollection(ini);
  var ft2 = img.reduceRegions(geometry, ee.Reducer.first(), scale);
  var date = img.date().format("YYYYMMdd");
  var ft3 = ft2.map(function(f){return f.set("date", date)});
return inift.merge(ft3);
};

// Iterates over the ImageCollection
var profile = ee.FeatureCollection(era5_dat.iterate(fill, ft));
print(profile,'profile');

// Export
Export.table.toDrive({
  collection : profile,
  description : "prec_lake",
  fileNamePrefix : "prec_lake",
  fileFormat : 'CSV',
  folder: 'rs',
  selectors: ["date","first"]
});