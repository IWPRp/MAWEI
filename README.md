<p align="center">
  <img src="interface/logo_iwpr.jpg" alt="IWPR Logo" height="60"/>
</p>

# MAWEI - Metro Atlanta Water Energy Interdependencies

<p align="center">
Interactive Sankey diagrams quantifying the coupled water and energy flows of the Metro Atlanta region. <br>
MAWEI web dashboard: <a href="https://iwprp.github.io/MAWEI">IWPRp.github.io/MAWEI</a> 
</p>

## Overview

MAWEI processes publicly available and stakeholder-supplied data (not committed to this repository) on water supply, wastewater, and energy generation/consumption for Metro Atlanta and renders them as interactive, animated Sankey diagrams showing cross-sector interdependencies. The tool supports dynamic year-over-year comparison and export to self-contained HTML files for standalone sharing.

<p align="center">
  <img src="interface/mawei_ew.png" alt="MAWEI Energy-Water Sankey" width="100%"/>
</p>

---

## Outputs 
5-year annual energy, water, and combined energy-water flows for 15 counties and aggregated Metro Atlanta over 2020-2024

- 8 processed data files, 140,000 rows of data
    - Metro Water, County Water
    - Metro Energy, County Energy 
    - Metro Energy Water, County Energy Water
    - Simplified variants (2)

- 2 Resolutions: Metro Atlanta aggregated, 15 counties individually 
    - 3 Sectors: Energy, water, energy-water
        - Data: 6 data files 
        - Diagrams 
            - 1 metro + 15 counties = 16 * 3 = 48 core diagrams 
            - Additional variants: e.g., simplified energy-water diagrams (16)
            - 64 diagrams across 2 formats (HTML + JSON) = 128 diagrams
    - 136 files across resolutions, sectors, data, Sankey diagrams, and variants  

- Interface: open MAWEI.html
- Web: open https://IWPRp.github.io/MAWEI or serve the `web/` folder on a local server using the [serve.command](/web/serve.command) file 

## Repository Structure

```
MAWEI/
├── functions.R                # Shared helpers, constants, Sankey plotting engine
├── launch.R                   # Entry-point script to launch the dashboard
├── data/                      # Processed input data (EIA, EPD, WMP, etc.)
├── R/
│   ├── flows_water.R          # Water supply, wastewater, and self-supply flows
│   ├── flows_energy.R         # Fuel, generation, and end-use energy flows
│   ├── flows_energy_water.R   # Combined energy-water Sankey (entry point)
│   ├── qc.R                   # Mass balance checks and logging
│   ├── prep_data.R            # Convert large and spatial datasets to smaller files
│   ├── analysis.R             # Post-analysis of combined energy-water flows
│   └── figures.R              # Supplementary maps and charts
├── outputs/files/             # Generated Sankey outputs
├── interface/                 # Local browser dashboard (HTML + JS + CSS)
└── web/                       # Web-based dashboard (HTML + JS + CSS)
```

---

## Quick Start

### 1 · Create the Sankey Diagrams

Open the project in RStudio (double-click `MAWEI.Rproj`), then source the entry-point script:

```r
source("R/flows_energy_water.R")
source("R/qc.R")
```

### 2 · Open the Local Dashboard

Double-click or execute [MAWEI.command](/interface/MAWEI.command) (Linux/macOS) or [MAWEI.bat](/interface/MAWEI.bat) (Windows) to launch the dashboard in your default browser. 

Alternatively, simply `source("launch.R")` in RStudio or navigate to the project root in a terminal and run:

```bash
Rscript launch.R
```

This opens [interface/MAWEI.html](/interface/MAWEI.html) in your default browser with no server requirements.

### 3 · Run the Post-Analysis
Post-analysis of the combined energy-water flows creates additional synthesis and visualizations. Run:

```r
source("R/prep_data.R")
source("R/analysis.R")
source("R/figures.R")
```

Scripts resolve all paths relative to their own location, so they work regardless of the session working directory.



---

## Data Sources

For the core MAWEI Sankeys (more datasets are used in the post-analysis):

| Dataset | Source | Coverage |
|---|---|---|
| EIA SEDS | U.S. Energy Information Administration | 2020-2024 |
| EIA 860 / 923 | EIA - generator & fuel data | 2020-2024 |
| Public Water Supply | GA Environmental Protection Division | Annual |
| Water Management Plans | GA EPD - surface, groundwater, wastewater | Annual |
| Self-supply (Agriculture) | GA EPD | Annual |
| Thermoelectric water use | USGS / EIA | Annual |
| Wastewater treatment | GA EPD NPDES | Annual |
| County FIPS | U.S. Census Bureau | 2024 |



---

## Dependencies

R ≥ 4.0.0 with the following packages. Install all at once:

```r
install.packages(c("dplyr","tidyr","readr","ggplot2","plotly","htmlwidgets",
                   "sf","RColorBrewer","ggsci","purrr","zoo"))
```

---

## Outputs

Sankey diagrams are saved to `outputs/files/` when `SAVE_FILES <- TRUE` in `functions.R`:

| Folder | Content |
|---|---|
| `energy/` | Fuel → generation → end-use energy flows |
| `water/` | Supply → treatment → demand → discharge flows |
| `energy-water/` | Combined energy-water interdependency diagram | 

File Name Pattern: `NN_resolution_county_sector.html` 

---

## Contact
Open an issue or contact Hassan Niazi at [hassan.niazi@pnnl.gov](mailto:hassan.niazi@pnnl.gov) or Kelsey Semrod at [kelsey.semrod@pnnl.gov](mailto:kelsey.semrod@pnnl.gov).

**Team**: Hassan Niazi, Kelsey Semrod, Kendall Mongird, Jennie Rice, IWPR Team at Pacific Northwest National Laboratory, and Atlanta Stakeholders  

---


<p align="center">
  Developed at <a href="https://www.pnnl.gov">Pacific Northwest National Laboratory</a> (PNNL)
  in support of the <a href="https://www.pnnl.gov/projects/integrated-water-power-resilience-project">Integrated Water Power Resilience</a> (IWPR) Project <br>
  Supported by the U.S. Department of Energy (DOE), Hydropower and Hydrokinetics Office (H2O), Energy-Water Resources (EWR) Program. 
  
</p>