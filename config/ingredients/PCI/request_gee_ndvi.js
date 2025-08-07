// --- Final Script: NYC Summer NDVI Median for 2020-2022 ---
// This script creates an NDVI raster designed to perfectly match a
// multi-year Land Surface Temperature (LST) median composite.

// 1. Define the Area of Interest (AOI) for New York City.
var nyc_bbox = ee.Geometry.Rectangle([-74.27, 40.48, -73.69, 40.92]);

// Center the map view on our area of interest for context.
Map.centerObject(nyc_bbox, 10);

// 2. Define the multi-year date range to match the LST data.
var startDate = '2020-01-01';
var endDate = '2022-12-31';

// 3. Load the Sentinel-2 Level-2A (Surface Reflectance) image collection.
var s2_collection = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  // Filter by our multi-year date range
  .filterDate(startDate, endDate)
  // **Critical Step**: Filter to include only summer months (June-August)
  .filter(ee.Filter.calendarRange(6, 8, 'month'))
  // Filter by our NYC bounding box
  .filterBounds(nyc_bbox)
  // Filter for images with less than 20% cloud cover to get the clearest views
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20));

// Optional: Print the number of images found to the console.
print('Number of clear summer images found (2020-2022):', s2_collection.size());

// 4. A function to calculate and add an NDVI band to each image.
// NDVI = (NIR - Red) / (NIR + Red)
// For Sentinel-2, NIR is Band 8 and Red is Band 4.
var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
};

// Apply the NDVI function to every image in our filtered collection.
var collectionWithNDVI = s2_collection.map(addNDVI);

// 5. Create the final median composite image.
// This calculates the median value for each pixel across all images.
// This is the most robust way to create a single, cloud-free image
// that represents the "typical" state over the period.
var medianComposite = collectionWithNDVI.median();

// 6. Select just our final NDVI band and clip it to the NYC bounding box.
var finalNDVI = medianComposite.select('NDVI').clip(nyc_bbox);

// 7. Add the final NDVI layer to the map for visualization.
// We use a classic green-to-red color palette for easy interpretation.
var ndviPalette = ['red', 'yellow', 'green'];
Map.addLayer(finalNDVI, {min: 0, max: 0.8, palette: ndviPalette}, 'NYC Avg Summer NDVI (2020-2022)');

// 8. Prepare the export task to get the GeoTIFF file.
Export.image.toDrive({
  image: finalNDVI,
  description: 'NYC_NDVI_2020-2022_Summer_Median', // A descriptive filename
  folder: 'GEE_Exports', // This folder will be in your Google Drive
  scale: 10, // Native resolution of Sentinel-2 bands (10 meters)
  region: nyc_bbox,
  fileFormat: 'GeoTIFF',
  maxPixels: 1e9 // A high value to ensure the export works for the region
});