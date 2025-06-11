
# 🌳 towardTreeEquity

**the towardTreeEquity package is a flexible spatial and statistical scaffold 
that harmonizes foundational datasets and makes them queryable, 
so team members can build their own modular research modules on top of it.**



---

## 🌱 overview

this project lays the foundation for a structured, spatially aware data 
pipeline to support equity-centered urban forestry analytics. it serves 
three core audiences:

- **policy researchers** exploring correlations between canopy cover, 
demographic patterns, and environmental risk
- **municipal planners** verifying and supplementing local tree 
inventories
- **community organizers + citizen scientists** engaging in 
neighborhood-level data storytelling

---

## 🧬 project architecture

___

| layer             | contents                                           | used for                                            |
| ----------------- | -------------------------------------------------- | --------------------------------------------------- |
| `tree_canopy`     | municipal trees, species, planting date, condition | urban forestry, biodiversity, planting strategy     |
| `demographics`    | ACS, EJ screeners, redlining overlays              | social vulnerability, gentrification, disinvestment |
| `infrastructure`  | zoning, parcel data, flood risk, heat surfaces     | climate resilience, planning, infrastructure audits |
| `air_quality`     | local monitors, EPA AQS, satellite NO₂/PM2.5       | pollution mapping, asthma clusters                  |
| `econ_layers`     | property values, tax assessments, green jobs       | economic justice, displacement, green economy       |
| `health`          | hospitalization rates, chronic illness maps        | public health inequities                            |
| `climate_metrics` | urban heat islands, canopy % by tract              | adaptation, heat resilience                         |

___

This repo supports a multi-phase initiative:

1. **Data Mart Construction**
   - unify municipal tree data, census layers, environmental indices, and 
zoning/flood overlays
   - prepare geospatial joins and block-level summaries

2. **Operational Community Database**
   - format outputs for use by community organizers and citizen-science 
tools

3. **Interactive Dashboard**
   - visualize spatial equity gaps and green infrastructure resilience
   - support ML-driven insights on intervention priorities

---

## 📦 using the project

this project uses [`renv`](https://rstudio.github.io/renv/) for dependency 
management and is organized for modularity and collaboration.

---

## 📁 directory structure

```
towardTreeEquity/
├── data_raw/          # original datasets (not tracked by git)
├── data_clean/        # processed/cleaned datasets
├── data_external/     # partner-provided or supplemental data
├── scripts/           # data processing + analysis scripts
│   ├── setup/         # package setup, renv, utils
│   └── modules/       # team member research modules
├── outputs/           # maps, figures, tables, .rds
├── docs/              # proposals, readings, planning notes
├── dashboard/         # shiny app, Rmds, or viz code
├── tests/             # validation and test scripts (optional)
├── .gitignore
├── README.md
├── renv.lock
└── towardTreeEquity.Rproj
```

---

## 🤝 contributions

this is a collaborative project rooted in climate justice, open science, 
and data equity. all team members are invited to propose modules (data 
pipelines, spatial joins, ML experiments) via issues or pull requests.

---

## 💌 to join, contact:

**Kalli A. Hale** — *data justice, climate resilient green infrastructure, and 
tree-loving tendencies*
questions, proposals, or data drop-offs? open an issue or email: 
`KalliAnn.Hale@gmail.com`

---

## ✨ citations

*in development*
