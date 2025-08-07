// --- Final Script (Corrected): NYC Nighttime Lights Median for 2020-2022 ---
// This version uses a MEDIAN composite to be perfectly consistent with
// the LST and NDVI datasets.

// 1. Define the Area of Interest (AOI) for New York City.
var nyc_bbox = ee.Geometry.Rectangle([-74.27, 40.48, -73.69, 40.92]);

// Center the map view on our area of interest.
Map.centerObject(nyc_bbox, 10);

// 2. Define the multi-year date range to match your other datasets.
var startDate = '2020-01-01';
var endDate = '2022-12-31';

// 3. Load the VIIRS Nighttime Lights monthly composite collection.
var viirs_collection = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG')
  .filterDate(startDate, endDate)
  .filterBounds(nyc_bbox);

// 4. Select the specific band for average radiance.
var radiance_collection = viirs_collection.select('avg_rad');

// 5. Create a single composite image using MEDIAN.
// --- THIS IS THE KEY CHANGE ---
// We now use .median() to match the methodology of the LST and NDVI data.
var medianComposite = radiance_collection.median(); // CHANGED from .mean()

// 6. Clip the final result to the NYC bounding box.
var finalLights = medianComposite.clip(nyc_bbox); // CHANGED variable name for clarity

// 7. Add the final layer to the map for visualization.
var lightPalette = ['black', 'purple', 'red', 'yellow', 'white'];
Map.addLayer(finalLights, {min: 0, max: 100, palette: lightPalette}, 'NYC Nighttime Lights MEDIAN (2020-2022)'); // CHANGED title

// 8. Prepare the export task to get your GeoTIFF file.
Export.image.toDrive({
  image: finalLights,
  description: 'NYC_NighttimeLights_2020-2022_Median', // CHANGED filename
  folder: 'GEE_Exports',
  scale: 500, // VIIRS resolution is ~500m
  region: nyc_bbox,
  fileFormat: 'GeoTIFF',
  maxPixels: 1e9
});