# Candidate node palettes for review, rendered by R/preview_palettes.R.
#
# Each candidate covers the same key set as the production palette so that any node
# resolves without falling back. The pattern-based fallback in resolve_node_color()
# still handles facility and water-body names in the county diagrams, which are too
# numerous to name individually.
#
# Design intent shared by all three:
#   - fuels carry their conventional physical association (coal dark, gas warm,
#     solar bright, hydro blue) so a reader needs the legend only once
#   - one accent is reserved for the grid node, since it is the hinge of the diagram
#   - every terminal loss is desaturated, so waste recedes and useful output advances
#   - water runs cool and energy runs warm, which is what lets the combined
#     energy-water diagram be read at a glance

# ---- A. Editorial: low-saturation, print-first ----------------------------
# Muted earth and slate tones at roughly equal lightness, so no single ribbon
# dominates and the diagram survives greyscale printing.
PALETTE_EDITORIAL <- list(
  Coal = "#3D3A38", "Natural Gas" = "#C8763C", Petroleum = "#8A5A44",
  Solar = "#E0A93B", Biomass = "#6E8B5A", "Hydroelectric Water" = "#4E7C93",
  Geothermal = "#A2685E", "Energy Storage" = "#7A6C93", Other = "#9E9A94",
  "Onsite / BehindTheMeter" = "#C79A4B", "Onsite Solar/DER" = "#D8B25E",
  "Bowen Plant" = "#5C4742", "Yates Plant" = "#7A6355", "Jack McDonough Plant" = "#6E5B4B",
  "Utility-scale Generation" = "#8C7A66", "Distributed-scale Generation" = "#A08D74",
  "On-Site Backup Generation" = "#B5A18A", "Small-scale generation" = "#A08D74",
  "Thermoelectric Generation" = "#B5603A",
  "Electricity Imports" = "#C99A6B", "Electricity Exports" = "#A8845F",
  "Out-Metro Electricity Imports" = "#C99A6B", "Out-Metro Electricity Exports" = "#A8845F",
  "Residential Use" = "#6E8CA8", "Commercial Use" = "#B08E58", "Industrial Use" = "#7E7B78",
  "Government Use" = "#8A7FA0", "Transportation Use" = "#A66B60",
  "Agricultural Use" = "#7D8F62", "Water Services Energy" = "#5E8583", en4water = "#5E8583",
  "Energy Services" = "#5B8C6A", "Rejected Energy" = "#BDB8B2",
  "Efficiency Losses" = "#A9A4A0", "Transmission & Dist. Losses" = "#B8B4B0",
  "Plants Own Use" = "#918C88", "Energy Losses" = "#ADA8A4",
  "Surface Water (all basins)" = "#5B8CA6", Groundwater = "#3F6B80",
  "Groundwater (all basins)" = "#3F6B80",
  "Chattahoochee Basin" = "#4A7C99", "Coosa_Etowah Basin" = "#5E93AC",
  "Flint Basin" = "#6FA3BA", "Ocmulgee Basin" = "#7FB2C6", "Oconee Basin" = "#8FBFD1",
  "Tallapoosa Basin" = "#9ECBDB", Basins = "#5E93AC",
  "Public Water Supply" = "#3E7290",
  "Infiltration and Inflow" = "#7C9BA0",
  "Wastewater Collection" = "#7B7A62", "In-County Treatment" = "#5F7F5E",
  "Wastewater Treated" = "#6E8E6D",
  "Wastewater Transfer Inflows (within Metro Atlanta)" = "#8F8C6F",
  "Wastewater Transfer Outflows (within Metro Atlanta)" = "#9E9B7E",
  "Septic Systems" = "#9A8F7A", Losses = "#B0AAA4", "Water Losses" = "#B0AAA4",
  Discharge = "#43707C", discharge = "#43707C", Disposal = "#43707C",
  River = "#4A7E88", Creek = "#5C8F97", Lake = "#547F9B", Reservoir = "#456F8A",
  Wetland = "#6D8F72", Reuse = "#5E9A94", Land = "#878A5E"
)

# ---- B. Signature: deeper, higher contrast -------------------------------
# Jewel tones on a wider lightness range. Reads strongly on screen and gives the
# grid node real presence; the intended default for the dashboard.
PALETTE_SIGNATURE <- list(
  Coal = "#2B2B2E", "Natural Gas" = "#E08A2E", Petroleum = "#7C4A2D",
  Solar = "#F2C230", Biomass = "#4E8B4A", "Hydroelectric Water" = "#2E6F94",
  Geothermal = "#B4553F", "Energy Storage" = "#6A5ACD", Other = "#9A9A9A",
  "Onsite / BehindTheMeter" = "#D9A22B", "Onsite Solar/DER" = "#F0C651",
  "Bowen Plant" = "#3F3A44", "Yates Plant" = "#5E5566", "Jack McDonough Plant" = "#4E4755",
  "Utility-scale Generation" = "#7A6E88", "Distributed-scale Generation" = "#94879F",
  "On-Site Backup Generation" = "#ADA0B5", "Small-scale generation" = "#94879F",
  "Thermoelectric Generation" = "#D2691E",
  "Electricity Imports" = "#E8A44C", "Electricity Exports" = "#B87A33",
  "Out-Metro Electricity Imports" = "#E8A44C", "Out-Metro Electricity Exports" = "#B87A33",
  "Residential Use" = "#4A7FB5", "Commercial Use" = "#C08A2E", "Industrial Use" = "#6E6E76",
  "Government Use" = "#7A6BA8", "Transportation Use" = "#B24A3C",
  "Agricultural Use" = "#6E8F3C", "Water Services Energy" = "#2E7F7C", en4water = "#2E7F7C",
  "Energy Services" = "#3E9E63", "Rejected Energy" = "#C4C0BA",
  "Efficiency Losses" = "#A5A19B", "Transmission & Dist. Losses" = "#B7B3AD",
  "Plants Own Use" = "#8B8781", "Energy Losses" = "#ABA7A1",
  "Surface Water (all basins)" = "#2E7CB0", Groundwater = "#1F5878",
  "Groundwater (all basins)" = "#1F5878",
  "Chattahoochee Basin" = "#1F6FA8", "Coosa_Etowah Basin" = "#3585B8",
  "Flint Basin" = "#4A9AC6", "Ocmulgee Basin" = "#5FAED2", "Oconee Basin" = "#77BFDD",
  "Tallapoosa Basin" = "#8FCEE7", Basins = "#3585B8",
  "Public Water Supply" = "#1C6491",
  "Infiltration and Inflow" = "#5F9EA0",
  "Wastewater Collection" = "#7A7A4E", "In-County Treatment" = "#41764A",
  "Wastewater Treated" = "#4F8A58",
  "Wastewater Transfer Inflows (within Metro Atlanta)" = "#8C8A55",
  "Wastewater Transfer Outflows (within Metro Atlanta)" = "#A09E68",
  "Septic Systems" = "#9C8A63", Losses = "#ADA7A1", "Water Losses" = "#ADA7A1",
  Discharge = "#1F6B72", discharge = "#1F6B72", Disposal = "#1F6B72",
  River = "#26757E", Creek = "#39909A", Lake = "#3C7FA5", Reservoir = "#2B6B90",
  Wetland = "#4E8F5C", Reuse = "#3AA39A", Land = "#7E8B44"
)

# ---- C. Cartographic: physical-map convention ---------------------------
# Borrows the colour logic of a physical atlas: water blues, vegetation greens,
# mineral browns and greys. The most literal of the three, and the easiest to
# explain in a caption.
PALETTE_CARTOGRAPHIC <- list(
  Coal = "#333333", "Natural Gas" = "#D98C3F", Petroleum = "#6B4423",
  Solar = "#EFBF42", Biomass = "#5B8C3E", "Hydroelectric Water" = "#3E7FA8",
  Geothermal = "#9C4F3F", "Energy Storage" = "#6C5B9E", Other = "#96968E",
  "Onsite / BehindTheMeter" = "#CFA23A", "Onsite Solar/DER" = "#E4C25A",
  "Bowen Plant" = "#4A4441", "Yates Plant" = "#6B625C", "Jack McDonough Plant" = "#5A524C",
  "Utility-scale Generation" = "#847A70", "Distributed-scale Generation" = "#9E948A",
  "On-Site Backup Generation" = "#B8AEA4", "Small-scale generation" = "#9E948A",
  "Thermoelectric Generation" = "#C2703C",
  "Electricity Imports" = "#DDA95E", "Electricity Exports" = "#AE8244",
  "Out-Metro Electricity Imports" = "#DDA95E", "Out-Metro Electricity Exports" = "#AE8244",
  "Residential Use" = "#5985A8", "Commercial Use" = "#B58A3C", "Industrial Use" = "#75736E",
  "Government Use" = "#7E72A0", "Transportation Use" = "#A85444",
  "Agricultural Use" = "#7A8F42", "Water Services Energy" = "#3E8280", en4water = "#3E8280",
  "Energy Services" = "#4E9557", "Rejected Energy" = "#C6C2BA",
  "Efficiency Losses" = "#A8A49C", "Transmission & Dist. Losses" = "#BAB6AE",
  "Plants Own Use" = "#8E8A82", "Energy Losses" = "#AEAAA2",
  "Surface Water (all basins)" = "#3E85B5", Groundwater = "#2A5F80",
  "Groundwater (all basins)" = "#2A5F80",
  "Chattahoochee Basin" = "#2F7BAE", "Coosa_Etowah Basin" = "#4390BE",
  "Flint Basin" = "#57A4CB", "Ocmulgee Basin" = "#6CB6D6", "Oconee Basin" = "#83C6E0",
  "Tallapoosa Basin" = "#9AD4E9", Basins = "#4390BE",
  "Public Water Supply" = "#276E9C",
  "Infiltration and Inflow" = "#6C9598",
  "Wastewater Collection" = "#83804F", "In-County Treatment" = "#4A7C4F",
  "Wastewater Treated" = "#58905C",
  "Wastewater Transfer Inflows (within Metro Atlanta)" = "#948F58",
  "Wastewater Transfer Outflows (within Metro Atlanta)" = "#A6A16C",
  "Septic Systems" = "#A08D68", Losses = "#B2ACA4", "Water Losses" = "#B2ACA4",
  Discharge = "#2A7078", discharge = "#2A7078", Disposal = "#2A7078",
  River = "#317A84", Creek = "#44949C", Lake = "#4285A8", Reservoir = "#316F94",
  Wetland = "#55925F", Reuse = "#42A79C", Land = "#828E48"
)

SANKEY_PALETTE_CANDIDATES <- list(
  editorial     = PALETTE_EDITORIAL,
  signature     = PALETTE_SIGNATURE,
  cartographic  = PALETTE_CARTOGRAPHIC,
  vivid_current = SANKEY_COLORS_VIVID
)
