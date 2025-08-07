# Case Study: Park Cooling Index (PCI) in New York City

This document demonstrates how to apply the `towardTreeEquity` framework to a specific analysis: replicating a Park Cooling Index (PCI) study for the Bronx, NY. It details the specific configuration, data schemas, and outputs for this pilot.

## 1. Reference Configuration (`config_nyc.yml`)

This pilot uses a configuration file that specifies the NYC context.

```yaml
city_name: "New York City"
crs_epsg: 2263 # NAD83 / New York Long Island (ftUS)
data_sources:
  parks: "https://.../nyc_parks.geojson"
  land_surface_temp: "bronze/lst/landsat_scene_xxx.tif"
  land_cover: "bronze/landcover/nlcd_nyc.tif"
  # ... other data sources for population, roads, etc.
field_mappings:
  parks:
    source_id: "OMPPROPID"
    park_name: "SIGNNAME"
```

## 2. Pilot-Specific Silver Layer Tables

These metric tables will be created in the Silver database to hold the results of the PCI analysis. They link directly to the universal `parks` table via the `park_id` foreign key.

### Table: `pci_park_metrics`

| Canonical Field Name | Data Type (PostGIS) | Description & Source Concept                               |
| :------------------- | :------------------ | :--------------------------------------------------------- |
| `metric_id`          | `uuid`              | **Primary Key.**                                           |
| `park_id`            | `uuid`              | **Foreign Key,** links to `parks.park_id`.                 |
| `pci`                | `numeric`           | Park Cooling Index (`LST_buffer - LST_park`).              |
| `lst_park_median_c`  | `numeric`           | Median Land Surface Temp. in Celsius inside the park.      |
| `park_area_sqkm`     | `numeric`           | Area of the park polygon (`PA`).                           |
| `ndvi_veg_mean`      | `numeric`           | Mean NDVI within vegetated areas of the park (`NDVIveg`).  |
| `pct_cover_forest`   | `numeric`           | Percent of park area covered by forest/trees (`FAP`).      |

### Table: `pci_buffer_metrics`

| Canonical Field Name | Data Type (PostGIS) | Description & Source Concept                          |
| :------------------- | :------------------ | :---------------------------------------------------- |
| `metric_id`          | `uuid`              | **Primary Key.**                                      |
| `park_id`            | `uuid`              | **Foreign Key,** links to `parks.park_id`.            |
| `buffer_distance_m`  | `integer`           | The radius of the buffer used (e.g., 300).            |
| `lst_buffer_median_c`| `numeric`           | Median Land Surface Temp. in Celsius in the buffer.   |
| `pop_density_mean`   | `numeric`           | Mean population density in the buffer (`Buffer_POP`). |

## 3. Gold Layer Output

The `gold_layer` functions for this pilot will read from the Silver tables above to produce analytical outputs. For example, a `gold_pci_summary` table could be generated to explore the correlation between park features and cooling intensity.

### Example Table: `gold_pci_summary`

| Field Name         | Description                                                    |
| :----------------- | :------------------------------------------------------------- |
| `park_id`          | Foreign Key to the park.                                       |
| `park_name`        | The common name of the park.                                   |
| `pci`              | The calculated Park Cooling Index.                             |
| `pct_cover_forest` | The percent of forest cover in the park.                       |
| `pop_density_mean` | The average population density in the surrounding buffer.      |
| `council_district` | Political geography for policy insight (joined from another table). |