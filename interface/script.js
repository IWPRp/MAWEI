// --- Configuration (change here if file paths or counties change) ---
const BASE_PATH = 'files';
const GEOJSON_INLINE = {"type": "FeatureCollection", "features": [{"type": "Feature", "properties": {"GEO_ID": "0500000US13113", "STATE": "13", "COUNTY": "113", "NAME": "Fayette", "LSAD": "County", "CENSUSAREA": 194.342}, "geometry": {"type": "Polygon", "coordinates": [[[-84.497527, 33.257422], [-84.62722, 33.440078], [-84.60954, 33.502511], [-84.458665, 33.550933], [-84.381759, 33.463414], [-84.388118, 33.352465], [-84.432907, 33.2565], [-84.497527, 33.257422]]]}, "id": "13113"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13139", "STATE": "13", "COUNTY": "139", "NAME": "Hall", "LSAD": "County", "CENSUSAREA": 392.782}, "geometry": {"type": "Polygon", "coordinates": [[[-84.062841, 34.167873], [-83.989059, 34.195732], [-83.927284, 34.279399], [-83.957077, 34.334011], [-83.980649, 34.418389], [-83.843405, 34.505494], [-83.666413, 34.503598], [-83.615251, 34.431748], [-83.669473, 34.366689], [-83.620115, 34.295276], [-83.817682, 34.127493], [-83.86803, 34.098281], [-84.062841, 34.167873]]]}, "id": "13139"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13067", "STATE": "13", "COUNTY": "067", "NAME": "Cobb", "LSAD": "County", "CENSUSAREA": 339.549}, "geometry": {"type": "Polygon", "coordinates": [[[-84.724435, 33.881859], [-84.737836, 34.079399], [-84.729235, 34.079199], [-84.722636, 34.079199], [-84.721936, 34.079099], [-84.687222, 34.0785], [-84.684745, 34.0785], [-84.682835, 34.078399], [-84.676415, 34.078298], [-84.674935, 34.078199], [-84.673935, 34.078299], [-84.672635, 34.078199], [-84.659234, 34.077999], [-84.521992, 34.075399], [-84.418927, 34.073298], [-84.383027, 33.9638], [-84.442708, 33.901543], [-84.480134, 33.817319], [-84.578132, 33.743507], [-84.619892, 33.805024], [-84.724139, 33.80617], [-84.723969, 33.815306], [-84.725181, 33.816995], [-84.725035, 33.819905], [-84.724435, 33.881859]]]}, "id": "13067"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13077", "STATE": "13", "COUNTY": "077", "NAME": "Coweta", "LSAD": "County", "CENSUSAREA": 440.892}, "geometry": {"type": "Polygon", "coordinates": [[[-84.939015, 33.224693], [-85.015358, 33.425506], [-84.850713, 33.511457], [-84.60954, 33.502511], [-84.62722, 33.440078], [-84.497527, 33.257422], [-84.508926, 33.245222], [-84.50029, 33.233444], [-84.502352, 33.221055], [-84.85236, 33.22359], [-84.862359, 33.191173], [-84.939015, 33.224693]]]}, "id": "13077"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13015", "STATE": "13", "COUNTY": "015", "NAME": "Bartow", "LSAD": "County", "CENSUSAREA": 459.544}, "geometry": {"type": "Polygon", "coordinates": [[[-85.005775, 34.392446], [-84.862863, 34.396601], [-84.653232, 34.41259], [-84.659234, 34.077999], [-84.672635, 34.078199], [-84.673935, 34.078299], [-84.674935, 34.078199], [-84.676415, 34.078298], [-84.682835, 34.078399], [-84.684745, 34.0785], [-84.687222, 34.0785], [-84.721936, 34.079099], [-84.722636, 34.079199], [-84.729235, 34.079199], [-84.737836, 34.079399], [-84.922742, 34.082497], [-85.046871, 34.096412], [-85.005775, 34.392446]]]}, "id": "13015"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13057", "STATE": "13", "COUNTY": "057", "NAME": "Cherokee", "LSAD": "County", "CENSUSAREA": 421.674}, "geometry": {"type": "Polygon", "coordinates": [[[-84.258075, 34.335156], [-84.258743, 34.185909], [-84.328263, 34.186144], [-84.418927, 34.073298], [-84.521992, 34.075399], [-84.659234, 34.077999], [-84.653232, 34.41259], [-84.58263, 34.381492], [-84.257586, 34.380992], [-84.257652, 34.375111], [-84.257812, 34.372631], [-84.258075, 34.335156]]]}, "id": "13057"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13063", "STATE": "13", "COUNTY": "063", "NAME": "Clayton", "LSAD": "County", "CENSUSAREA": 141.57}, "geometry": {"type": "Polygon", "coordinates": [[[-84.388118, 33.352465], [-84.381759, 33.463414], [-84.458665, 33.550933], [-84.458579, 33.556242], [-84.458924, 33.559759], [-84.458927, 33.565911], [-84.45863, 33.572107], [-84.458399, 33.572743], [-84.458627, 33.586456], [-84.458514, 33.608625], [-84.457726, 33.64887], [-84.365325, 33.647809], [-84.360224, 33.647909], [-84.350224, 33.647908], [-84.282619, 33.647009], [-84.281273, 33.647411], [-84.283518, 33.502514], [-84.353584, 33.436165], [-84.3544, 33.352514], [-84.388118, 33.352465]]]}, "id": "13063"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13223", "STATE": "13", "COUNTY": "223", "NAME": "Paulding", "LSAD": "County", "CENSUSAREA": 312.219}, "geometry": {"type": "Polygon", "coordinates": [[[-85.05031, 33.904488], [-84.978683, 33.951393], [-84.922742, 34.082497], [-84.737836, 34.079399], [-84.724435, 33.881859], [-84.725035, 33.819905], [-84.725181, 33.816995], [-84.723969, 33.815306], [-84.724139, 33.80617], [-84.724122, 33.791439], [-84.725477, 33.788579], [-84.741348, 33.788568], [-84.769935, 33.784704], [-84.791993, 33.781162], [-84.795109, 33.779809], [-84.799853, 33.779909], [-84.832705, 33.778522], [-84.832708, 33.776019], [-84.879151, 33.774758], [-84.901688, 33.780703], [-84.918629, 33.786328], [-85.037926, 33.811942], [-85.036684, 33.904327], [-85.05031, 33.904488]]]}, "id": "13223"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13247", "STATE": "13", "COUNTY": "247", "NAME": "Rockdale", "LSAD": "County", "CENSUSAREA": 129.793}, "geometry": {"type": "Polygon", "coordinates": [[[-84.023713, 33.752808], [-83.984555, 33.784332], [-83.982033, 33.786054], [-83.953332, 33.768034], [-83.914823, 33.744203], [-83.972655, 33.605482], [-84.024279, 33.548226], [-84.024854, 33.547507], [-84.044493, 33.525776], [-84.136289, 33.57233], [-84.17213, 33.621919], [-84.181584, 33.629174], [-84.184143, 33.646157], [-84.104334, 33.636025], [-84.056614, 33.726608], [-84.023713, 33.752808]]]}, "id": "13247"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13117", "STATE": "13", "COUNTY": "117", "NAME": "Forsyth", "LSAD": "County", "CENSUSAREA": 224.021}, "geometry": {"type": "Polygon", "coordinates": [[[-84.258075, 34.335156], [-83.957077, 34.334011], [-83.927284, 34.279399], [-83.989059, 34.195732], [-84.062841, 34.167873], [-84.074624, 34.163687], [-84.094763, 34.131708], [-84.101343, 34.106305], [-84.10261, 34.103788], [-84.105403, 34.102223], [-84.107143, 34.10003], [-84.109894, 34.098423], [-84.117801, 34.065315], [-84.097692, 34.050654], [-84.200373, 34.090118], [-84.258934, 34.109539], [-84.258743, 34.185909], [-84.258075, 34.335156]]]}, "id": "13117"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13089", "STATE": "13", "COUNTY": "089", "NAME": "DeKalb", "LSAD": "County", "CENSUSAREA": 267.582}, "geometry": {"type": "Polygon", "coordinates": [[[-84.254149, 33.647045], [-84.281273, 33.647411], [-84.282619, 33.647009], [-84.350224, 33.647908], [-84.349799, 33.664035], [-84.348325, 33.852503], [-84.348138, 33.857692], [-84.348225, 33.859502], [-84.348525, 33.861903], [-84.348125, 33.864903], [-84.348325, 33.867903], [-84.348125, 33.879203], [-84.348425, 33.881902], [-84.348224, 33.8929], [-84.348225, 33.904802], [-84.348025, 33.918302], [-84.347825, 33.918902], [-84.347925, 33.927001], [-84.347823, 33.938017], [-84.276822, 33.9577], [-84.275722, 33.954201], [-84.271922, 33.9559], [-84.266306, 33.947577], [-84.272216, 33.944853], [-84.265337, 33.932576], [-84.259011, 33.91882], [-84.256022, 33.914401], [-84.250413, 33.910812], [-84.23222, 33.902002], [-84.21663, 33.884976], [-84.20472, 33.877003], [-84.203519, 33.873003], [-84.187005, 33.865515], [-84.179418, 33.864403], [-84.172139, 33.857516], [-84.023713, 33.752808], [-84.056614, 33.726608], [-84.104334, 33.636025], [-84.184143, 33.646157], [-84.223952, 33.646572], [-84.224235, 33.630657], [-84.245453, 33.63073], [-84.254149, 33.647045]]]}, "id": "13089"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13097", "STATE": "13", "COUNTY": "097", "NAME": "Douglas", "LSAD": "County", "CENSUSAREA": 200.067}, "geometry": {"type": "Polygon", "coordinates": [[[-84.769935, 33.784704], [-84.741348, 33.788568], [-84.725477, 33.788579], [-84.724122, 33.791439], [-84.724139, 33.80617], [-84.619892, 33.805024], [-84.578132, 33.743507], [-84.586826, 33.729114], [-84.594332, 33.729007], [-84.601732, 33.724408], [-84.608032, 33.712908], [-84.621232, 33.704508], [-84.632131, 33.700312], [-84.630117, 33.693116], [-84.752735, 33.63021], [-84.763097, 33.614211], [-84.775591, 33.609662], [-84.805655, 33.58642], [-84.808934, 33.574085], [-84.905788, 33.573378], [-84.902546, 33.661066], [-84.901688, 33.780703], [-84.879151, 33.774758], [-84.832708, 33.776019], [-84.832705, 33.778522], [-84.799853, 33.779909], [-84.795109, 33.779809], [-84.791993, 33.781162], [-84.769935, 33.784704]]]}, "id": "13097"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13121", "STATE": "13", "COUNTY": "121", "NAME": "Fulton", "LSAD": "County", "CENSUSAREA": 526.635}, "geometry": {"type": "Polygon", "coordinates": [[[-84.752735, 33.63021], [-84.630117, 33.693116], [-84.632131, 33.700312], [-84.621232, 33.704508], [-84.608032, 33.712908], [-84.601732, 33.724408], [-84.594332, 33.729007], [-84.586826, 33.729114], [-84.578132, 33.743507], [-84.480134, 33.817319], [-84.442708, 33.901543], [-84.383027, 33.9638], [-84.418927, 34.073298], [-84.328263, 34.186144], [-84.258743, 34.185909], [-84.258934, 34.109539], [-84.200373, 34.090118], [-84.097692, 34.050654], [-84.276822, 33.9577], [-84.347823, 33.938017], [-84.347925, 33.927001], [-84.347825, 33.918902], [-84.348025, 33.918302], [-84.348225, 33.904802], [-84.348224, 33.8929], [-84.348425, 33.881902], [-84.348125, 33.879203], [-84.348325, 33.867903], [-84.348125, 33.864903], [-84.348525, 33.861903], [-84.348225, 33.859502], [-84.348138, 33.857692], [-84.348325, 33.852503], [-84.349799, 33.664035], [-84.350224, 33.647908], [-84.360224, 33.647909], [-84.365325, 33.647809], [-84.457726, 33.64887], [-84.458514, 33.608625], [-84.458627, 33.586456], [-84.458399, 33.572743], [-84.45863, 33.572107], [-84.458927, 33.565911], [-84.458924, 33.559759], [-84.458579, 33.556242], [-84.458665, 33.550933], [-84.60954, 33.502511], [-84.850713, 33.511457], [-84.808934, 33.574085], [-84.805655, 33.58642], [-84.775591, 33.609662], [-84.763097, 33.614211], [-84.752735, 33.63021]]]}, "id": "13121"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13135", "STATE": "13", "COUNTY": "135", "NAME": "Gwinnett", "LSAD": "County", "CENSUSAREA": 430.383}, "geometry": {"type": "Polygon", "coordinates": [[[-84.109894, 34.098423], [-84.107143, 34.10003], [-84.105403, 34.102223], [-84.10261, 34.103788], [-84.101343, 34.106305], [-84.094763, 34.131708], [-84.074624, 34.163687], [-84.062841, 34.167873], [-83.86803, 34.098281], [-83.817682, 34.127493], [-83.869115, 34.004316], [-83.799104, 33.929844], [-83.982033, 33.786054], [-83.984555, 33.784332], [-84.023713, 33.752808], [-84.172139, 33.857516], [-84.179418, 33.864403], [-84.187005, 33.865515], [-84.203519, 33.873003], [-84.20472, 33.877003], [-84.21663, 33.884976], [-84.23222, 33.902002], [-84.250413, 33.910812], [-84.256022, 33.914401], [-84.259011, 33.91882], [-84.265337, 33.932576], [-84.272216, 33.944853], [-84.266306, 33.947577], [-84.271922, 33.9559], [-84.275722, 33.954201], [-84.276822, 33.9577], [-84.097692, 34.050654], [-84.117801, 34.065315], [-84.109894, 34.098423]]]}, "id": "13135"}, {"type": "Feature", "properties": {"GEO_ID": "0500000US13151", "STATE": "13", "COUNTY": "151", "NAME": "Henry", "LSAD": "County", "CENSUSAREA": 322.127}, "geometry": {"type": "Polygon", "coordinates": [[[-84.283518, 33.502514], [-84.281273, 33.647411], [-84.254149, 33.647045], [-84.245453, 33.63073], [-84.224235, 33.630657], [-84.223952, 33.646572], [-84.184143, 33.646157], [-84.181584, 33.629174], [-84.17213, 33.621919], [-84.136289, 33.57233], [-84.044493, 33.525776], [-83.923913, 33.444194], [-84.044594, 33.333656], [-84.044597, 33.333495], [-84.042663, 33.333501], [-84.102582, 33.298191], [-84.150581, 33.335639], [-84.3544, 33.352514], [-84.353584, 33.436165], [-84.283518, 33.502514]]]}, "id": "13151"}]};
const SHOW_MAP = true;
const COUNTIES = [
  'Bartow', 'Cherokee', 'Clayton', 'Cobb', 'Coweta',
  'DeKalb', 'Douglas', 'Fayette', 'Forsyth', 'Fulton',
  'Gwinnett', 'Hall', 'Henry', 'Paulding', 'Rockdale'
];

// FIPS -> county name lookup (must match COUNTIES and the GeoJSON id field)
const FIPS_TO_COUNTY = {
  '13015': 'Bartow', '13057': 'Cherokee', '13063': 'Clayton',
  '13067': 'Cobb', '13077': 'Coweta', '13089': 'DeKalb',
  '13097': 'Douglas', '13113': 'Fayette', '13117': 'Forsyth',
  '13121': 'Fulton', '13135': 'Gwinnett', '13139': 'Hall',
  '13151': 'Henry', '13223': 'Paulding', '13247': 'Rockdale'
};

// Distinct mellow pastel fill for each county (stable assignment by sorted name)
const COUNTY_COLORS = {
  'Bartow':   '#b5d8a0', // sage green
  'Cherokee': '#a0c4e8', // sky blue
  'Clayton':  '#e8c3a0', // warm sand
  'Cobb':     '#c4a0e8', // soft lavender
  'Coweta':   '#e8a0b5', // dusty rose
  'DeKalb':   '#a0e8d8', // mint
  'Douglas':  '#e8d8a0', // buttercup
  'Fayette':  '#a0b5e8', // periwinkle
  'Forsyth':  '#d8e8a0', // lime cream
  'Fulton':   '#e8a0a0', // blush
  'Gwinnett': '#a0e8b5', // seafoam
  'Hall':     '#d8a0e8', // orchid
  'Henry':    '#e8d0b0', // peach
  'Paulding': '#b0d8e8', // powder blue
  'Rockdale': '#c8e8a0'  // spring green
};

// --- Build file list dynamically from naming conventions ---
function buildFileList() {
  const files = [];

  // Energy
  files.push({ path: 'energy/01_metro_energy_flows.csv', type: 'data', resolution: 'metro', sector: 'energy' });
  files.push({ path: 'energy/02_county_energy_flows.csv', type: 'data', resolution: 'metro', sector: 'energy' });
  files.push({ path: 'energy/01_metro_energy.html', type: 'diagrams', resolution: 'metro', sector: 'energy' });
  COUNTIES.forEach(c => {
    files.push({ path: `energy/diagrams/02_county_${c}_energy.html`, type: 'diagrams', resolution: c.toLowerCase(), sector: 'energy' });
  });

  // Water
  files.push({ path: 'water/01_metro_water_flows.csv', type: 'data', resolution: 'metro', sector: 'water' });
  files.push({ path: 'water/02_county_water_flows.csv', type: 'data', resolution: 'metro', sector: 'water' });
  files.push({ path: 'water/01_metro_water.html', type: 'diagrams', resolution: 'metro', sector: 'water' });
  COUNTIES.forEach(c => {
    files.push({ path: `water/diagrams/02_county_${c}_water.html`, type: 'diagrams', resolution: c.toLowerCase(), sector: 'water' });
  });

  // Energy-Water
  files.push({ path: 'energy-water/01_metro_ew_flows.csv', type: 'data', resolution: 'metro', sector: 'energy-water' });
  files.push({ path: 'energy-water/02_metro_ew_simplified_flows.csv', type: 'data', resolution: 'metro', sector: 'energy-water' });
  files.push({ path: 'energy-water/03_county_ew_flows.csv', type: 'data', resolution: 'metro', sector: 'energy-water' });
  files.push({ path: 'energy-water/04_county_ew_simplified_flows.csv', type: 'data', resolution: 'metro', sector: 'energy-water' });
  files.push({ path: 'energy-water/01_metro_ew.html', type: 'diagrams', resolution: 'metro', sector: 'energy-water' });
  files.push({ path: 'energy-water/02_metro_ew_simplified.html', type: 'diagrams', resolution: 'metro', sector: 'energy-water' });
  COUNTIES.forEach(c => {
    files.push({ path: `energy-water/diagrams/03_county_${c}_ew.html`, type: 'diagrams', resolution: c.toLowerCase(), sector: 'energy-water' });
    files.push({ path: `energy-water/diagrams/04_county_${c}_ew_simplified.html`, type: 'diagrams', resolution: c.toLowerCase(), sector: 'energy-water' });
  });

  return files;
}

const files = buildFileList();

// --- Derive display name from path ---
function displayName(path) {
  const name = path.split('/').pop();
  return name
    .replace(/^\d+_/, '')
    .replace(/\.(csv|html)$/, '')
    .replace(/_/g, ' ')
    .replace(/\b\w/g, c => c.toUpperCase());
}

// DOM elements
const dataListElement = document.getElementById("data-list");
const diagramsListElement = document.getElementById("diagrams-list");
const searchInput = document.getElementById("search");
const sectorFilter = document.getElementById("sector-filter");
const resolutionFilter = document.getElementById("resolution-filter");
const viewerSection = document.getElementById("viewer-section");
const viewerContent = document.getElementById("viewer-content");
const closeViewerBtn = document.getElementById("close-viewer");
const sectorTabs = document.querySelectorAll('.sector-tab');

// --- Multi-select filter state ---
let activeSectors = new Set();     // empty = all
let selectedCounties = new Set();  // empty = all

function parseCSV(text) {
  const lines = text.trim().split('\n');
  return lines.map(line => {
    const values = [];
    let current = '';
    let inQuotes = false;

    for (let i = 0; i < line.length; i++) {
      const char = line[i];
      if (char === '"') {
        inQuotes = !inQuotes;
      } else if (char === ',' && !inQuotes) {
        values.push(current.trim());
        current = '';
      } else {
        current += char;
      }
    }
    values.push(current.trim());
    return values;
  });
}

function displayInViewer(file) {
  const viewerFilename = document.getElementById('viewer-filename');
  viewerFilename.innerHTML = '<span class="file-label">File:</span> ' + file.path.split('/').pop();
  viewerContent.innerHTML = '';

  if (file.type === 'data') {
    const message = document.createElement('div');
    message.classList.add('csv-message');
    message.innerHTML = '<div class="message-icon">📊</div>' +
                       '<h3>CSV Data File</h3>' +
                       '<p>To view this CSV file, please click the <strong>"Open"</strong> button above.</p>' +
                       '<p class="note">Note: CSV files will open in a new browser tab where you can view the raw data.</p>';
    viewerContent.appendChild(message);
  } else {
    const iframe = document.createElement('iframe');
    iframe.src = BASE_PATH + '/' + file.path;
    iframe.classList.add('viewer-iframe');
    viewerContent.appendChild(iframe);
  }

  viewerSection.classList.remove('hidden');
  viewerSection.scrollIntoView({ behavior: 'smooth', block: 'start' });
}

function closeViewer() {
  viewerSection.classList.add('hidden');
  viewerContent.innerHTML = '';
}

function createFileItem(file) {
  const fileItem = document.createElement('div');
  fileItem.classList.add('file-item');

  const fileType = document.createElement('span');
  fileType.classList.add('file-type', file.type === 'data' ? 'data' : 'diagram');
  fileType.textContent = file.type === 'data' ? 'CSV' : 'HTML';

  const fileName = document.createElement('h3');
  fileName.textContent = displayName(file.path);

  const fileLink = document.createElement('a');
  fileLink.href = BASE_PATH + '/' + file.path;
  fileLink.target = "_blank";
  fileLink.textContent = "Open";
  fileLink.onclick = (e) => { e.stopPropagation(); };

  fileItem.appendChild(fileType);
  fileItem.appendChild(fileName);
  fileItem.appendChild(fileLink);
  fileItem.addEventListener('click', () => displayInViewer(file));

  return fileItem;
}

// --- Centralized filter + render ---
function applyFilters() {
  const searchQuery = searchInput.value.toLowerCase();

  dataListElement.innerHTML = '';
  diagramsListElement.innerHTML = '';

  const filteredFiles = files.filter(file => {
    const matchesSector = activeSectors.size === 0 || activeSectors.has(file.sector);
    const matchesResolution = selectedCounties.size === 0 ||
      file.resolution === 'metro' || selectedCounties.has(file.resolution);
    const matchesSearch = displayName(file.path).toLowerCase().includes(searchQuery);
    return matchesSector && matchesResolution && matchesSearch;
  });

  const dataFiles = filteredFiles.filter(f => f.type === 'data');
  const diagramFiles = filteredFiles.filter(f => f.type === 'diagrams');

  if (dataFiles.length > 0) {
    dataFiles.forEach(f => dataListElement.appendChild(createFileItem(f)));
  } else {
    const el = document.createElement('div');
    el.classList.add('empty-state');
    el.textContent = 'No data files found';
    dataListElement.appendChild(el);
  }

  if (diagramFiles.length > 0) {
    diagramFiles.forEach(f => diagramsListElement.appendChild(createFileItem(f)));
  } else {
    const el = document.createElement('div');
    el.classList.add('empty-state');
    el.textContent = 'No diagrams found';
    diagramsListElement.appendChild(el);
  }

  syncDropdownsFromState();
}

function syncDropdownsFromState() {
  if (activeSectors.size === 0) sectorFilter.value = 'all';
  else if (activeSectors.size === 1) sectorFilter.value = [...activeSectors][0];
  else sectorFilter.value = 'all';

  if (selectedCounties.size === 0) resolutionFilter.value = 'all';
  else if (selectedCounties.size === 1) resolutionFilter.value = [...selectedCounties][0];
  else resolutionFilter.value = 'all';
}

// --- Sector Tabs (multi-select) ---
function toggleSectorTab(value) {
  if (value === 'all') {
    activeSectors.clear();
  } else {
    if (activeSectors.has(value)) {
      activeSectors.delete(value);
    } else {
      activeSectors.add(value);
    }
  }
  updateSectorTabUI();
  applyFilters();
}

function updateSectorTabUI() {
  sectorTabs.forEach(tab => {
    const sector = tab.dataset.sector;
    if (sector === 'all') {
      tab.classList.toggle('active', activeSectors.size === 0);
    } else {
      tab.classList.toggle('active', activeSectors.has(sector));
    }
  });
}

sectorTabs.forEach(tab => {
  tab.addEventListener('click', () => toggleSectorTab(tab.dataset.sector));
});

// Header dropdown still works as single-select override
sectorFilter.addEventListener('change', (e) => {
  if (e.target.value === 'all') {
    activeSectors.clear();
  } else {
    activeSectors.clear();
    activeSectors.add(e.target.value);
  }
  updateSectorTabUI();
  applyFilters();
});

searchInput.addEventListener('input', () => applyFilters());

resolutionFilter.addEventListener('change', (e) => {
  if (e.target.value === 'all' || e.target.value === 'metro') {
    selectedCounties.clear();
  } else {
    selectedCounties.clear();
    selectedCounties.add(e.target.value);
  }
  applyFilters();
  updateMapStyles();
});

closeViewerBtn.addEventListener('click', closeViewer);

// --- County Map Module ---
let map = null;
let geojsonLayer = null;

function darkenColor(hex, factor) {
  const r = parseInt(hex.slice(1, 3), 16);
  const g = parseInt(hex.slice(3, 5), 16);
  const b = parseInt(hex.slice(5, 7), 16);
  const f = 1 - factor;
  return '#' + [r, g, b].map(c => Math.round(c * f).toString(16).padStart(2, '0')).join('');
}

function countyStyle(name, state) {
  const base = COUNTY_COLORS[name] || '#e8ebef';
  if (state === 'selected') {
    return { color: '#ffffff', weight: 2.5, fillColor: darkenColor(base, 0.3), fillOpacity: 0.9 };
  }
  if (state === 'hover') {
    return { color: '#ffffff', weight: 2, fillColor: darkenColor(base, 0.15), fillOpacity: 0.75 };
  }
  return { color: '#ffffff', weight: 1.5, fillColor: base, fillOpacity: 0.2 };
}

function initMap() {
  const container = document.getElementById('map-container');
  if (!SHOW_MAP) { container.classList.add('hidden'); return; }

  map = L.map('county-map', {
    zoomControl: false,
    attributionControl: false,
    dragging: true,
    scrollWheelZoom: false
  });

  L.control.zoom({ position: 'topright' }).addTo(map);

  geojsonLayer = L.geoJSON(GEOJSON_INLINE, {
    style: (feature) => {
      const name = FIPS_TO_COUNTY[feature.id] || feature.properties.NAME;
      return countyStyle(name, 'default');
    },
    onEachFeature: (feature, layer) => {
      const name = FIPS_TO_COUNTY[feature.id] || feature.properties.NAME;
      const lower = name.toLowerCase();

      layer.bindTooltip(name, {
        sticky: true, direction: 'top', offset: [0, -8],
        className: 'county-tooltip'
      });

      layer.on('mouseover', () => {
        if (!selectedCounties.has(lower)) layer.setStyle(countyStyle(name, 'hover'));
      });
      layer.on('mouseout', () => {
        if (!selectedCounties.has(lower)) layer.setStyle(countyStyle(name, 'default'));
      });
      layer.on('click', () => {
        if (selectedCounties.has(lower)) {
          selectedCounties.delete(lower);
        } else {
          selectedCounties.add(lower);
        }
        updateMapStyles();
        applyFilters();
      });
    }
  }).addTo(map);

  map.fitBounds(geojsonLayer.getBounds(), { padding: [4, 4] });

  document.getElementById('map-reset').addEventListener('click', () => {
    selectedCounties.clear();
    updateMapStyles();
    applyFilters();
  });
}

function updateMapStyles() {
  if (!geojsonLayer) return;
  geojsonLayer.eachLayer(layer => {
    const name = FIPS_TO_COUNTY[layer.feature.id] || layer.feature.properties.NAME;
    const state = selectedCounties.has(name.toLowerCase()) ? 'selected' : 'default';
    layer.setStyle(countyStyle(name, state));
  });
}

// --- Initialize ---
applyFilters();
initMap();
