# `towardTreeEquity` Data Standards & Conventions

Welcome to the `towardTreeEquity` project! This document outlines the core, universal data standards that ensure our platform is consistent, reliable, and scalable for any urban environment.

## 1. Medallion Architecture Overview

Our data platform follows the Bronze -> Silver -> Gold Medallion architecture. This structure is universal.

*   **🥉 Bronze (Data Lake):** Raw source data (e.g., shapefiles, GeoTIFFs, CSVs) stored in a cloud data lake.
*   **🥈 Silver (Data Warehouse):** The single source of truth for analysis, stored in a **PostGIS** relational database. Data is cleaned, structured, and conforms to the standards in this document.
*   **🥇 Gold (Data Marts):** Purpose-built, aggregated views powering local dashboards and analyses.

## 2. Configuration-Driven Approach

The `towardTreeEquity` R package is designed to be generic. All location-specific details (like data sources, field names, and CRS) **MUST** be defined in a `config.yml` file for each deployment. This makes the framework adaptable to any city.

## 3. Canonical Coordinate Reference System (CRS)

*   **Principle:** To ensure data integrity, each city's Silver database **MUST** use a single, consistent projected Coordinate Reference System (CRS) suitable for local analysis.
*   **Implementation:** The appropriate `crs_epsg` code for the city **MUST** be defined in the `config.yml` file.
*   **Enforcement:** All `transform_*` R functions will use this `crs_epsg` value to re-project data before loading it into the Silver database.

## 4. Universal Silver Layer Schema

These are the foundational "identity" tables for the `towardTreeEquity` platform. Any deployment should aim to populate these. Specific analyses will add their own metric tables that link back to these core entities.

### Table: `parks`

| Canonical Field Name | Data Type (PostGIS)         | Description                                               |
| :------------------- | :-------------------------- | :-------------------------------------------------------- |
| `park_id`            | `uuid`                      | **Primary Key.** A unique, generated identifier.          |
| `source_id`          | `varchar`                   | The ID from the original source data. Mapped from config. |
| `park_name`          | `varchar`                   | The common name of the park. Mapped from config.          |
| `geometry`           | `geometry(Polygon, <CRS>)`  | The park boundary in the project's canonical CRS.         |

### Table: `parcels`

| Canonical Field Name | Data Type (PostGIS)         | Description                                                 |
| :------------------- | :-------------------------- | :---------------------------------------------------------- |
| `parcel_id`          | `uuid`                      | **Primary Key.** A unique, generated identifier.            |
| `source_id`          | `varchar`                   | The ID from the original source data (e.g., APN).           |
| `land_use_code`      | `varchar`                   | The official land use code.                                 |
| `geometry`           | `geometry(Polygon, <CRS>)`  | The parcel boundary in the project's canonical CRS.         |