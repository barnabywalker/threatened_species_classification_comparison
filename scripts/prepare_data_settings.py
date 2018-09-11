"""Constants for the prepare_data.py script to tidy it up a bit.
These are mostly name correction maps and column lists.
"""
import numpy as np

# get your own redlist api token from http://apiv3.iucnredlist.org/api/v3/token
REDLIST_API_TOKEN = "4dc85d76b527a90728859a283466b66938cef9beb8810bfc6b1b878ff75da131"
GADM_PATH = "../data/gadm36_levels_shp/gadm36_{}.shp"

# columns lists for outputs of predictor preparation functions
SPECIMEN_COLUMNS = ["species", "latitude", "longitude", "year",
                    "locality_0", "locality_1",  "locality_2", "locality_3", 
                    "group", "category", "criteria"]

RF_COLUMNS = ["species", "genus", "family", "order", "n_habitats",
              "realm_value", "elevation_min", "elevation_max", "range_eoo", 
              "latitude_centroid", "av_temp", "season_temp", "av_precip", "season_precip",
              "mean_hpd", "min_hpd", "mean_hfi", "mean_gdp", "eti", "group", 
              "category", "criteria"]

# CRS strings for geometries and rasters
DEFAULT_CRS = {'init': 'epsg:4326'}
EQUAL_AREA_CRS = {"proj": "moll", "lon_0": 0, "lat_0": 0, 
                  "ellps": "WGS84", "units": "m", "no_defs": True}

# standardising plant group names
GROUP_NAMES = {
    "coffee": "Coffea",
    "legumes": "Legumes",
    "madagascar_palms": "MadPalms",
    "myrcia": "Myrcia",
    "orchids": "OrchidsNG"
}

# standardising names for species
NAME_CORRECTION_MAP = {
    "dendrobium odoardoi": "dendrobium odoardi",
    "bulbophyllum djikstalianum": "bulbophyllum dijkstalianum",
    "bulbophyllum epicranthes": "bulbophyllum epicrianthes",
    "bulbophyllum brachytricha": "bulbophyllum brachytriche",
    "dendrobium renellii": "dendrobium rennellii",
    "coffea farafangensis": "coffea farafanganensis",
    "coffea neoleyroi": "coffea neoleroyi",
    "coffea rakatonasoloi": "coffea rakotonasoloi",
    "coffea travencorensis": "coffea travancorensis"
}

# standardising names for realms
REALM_NAME_MAP = {
    "Afrotropic": "Afrotropical", 
    "Neotropic": "Neotropical",
    "Australasia": "Australasian",
    "Indo-Malay": "Indomalayan"
}

# standardising names for GADM administrative areas
LEVEL1_CORRECTIONS = {
    "Canal Area": "Panamá",
    "RO": "Rondônia",
    "Rondonia": "Rondônia",
    "Espirito Santo": "Espírito Santo",
    "ES": "Espírito Santo",
    "Parana": "Paraná",
    "Ceara": "Ceará",
    "Altagracia": "La Altagracia",
    "Caroni": "Tunapuna/Piarco",
    "St. Andrew": "Couva-Tabaquite-Talparo",
    "Goias": "Goiás",
    "Sao Paulo": "São Paulo",
    "Amapa": "Amapá",
    "Ward of Tobago": "Tobago",
    "Vaupes": "Vaupés",
    "North West District": "Barima-Waini",
    "Rupununi": "Upper Takutu-Upper Essequibo",
    "Caqueta": "Amazonas",
    "Guainia": "Guainía",
    "SP": "São Paulo",
    "SC": "Santa Catarina", 
    "PR": "Paraná",
    "SE": "Sergipe",
    "Distrito Feder": "Distrito Federal"
}

LEVEL2_CORRECTIONS = {
    "Milne Bay": np.nan,
    "Western Highlands Province": np.nan,
    "Cananeia": "Cananéia",
    "HumaitÃ¡": "Humaitá",
    "Camacari": "Camaçari",
    "Itamarajú": "Itamaraju",
    "SEM INFORMAÇÃO": np.nan,
    "PORTO VELHO": "Porto Velho",
    "Bongara": "Bongará",
    "Guanabara": "Rio de Janeiro",
    "Pontal do Parana": "Pontal do Paraná",
    "Santarem": "Santarém",
    "LINHARES": "Linhares",
    "Parnagua": "Parnaguá", 
    "Paranagua": "Parnaguá",
    "Caroni": np.nan,
    "Cumuto": np.nan,
    "Fond St. Jacques": np.nan,
    "Blanchisseuse": np.nan,
    "Alexandra": "Paranaguá",
    "Manaus": "Iranduba",
    "Barra do Garcas": "Barra do Garças",
    "Minarum": np.nan,
    "Fundao": "Fundão",
    "Andaraí": "Andarai",
    "Telemaco Borba": "Telêmaco Borba",
    "Rio Uatuma": "Rio Preto da Eva",
    "Velasco": "José Miguel de Velasco",
    "Sao Francisco do Sul": "São Francisco do Sul",
    "Coarí": "Coari",
    "JAPARATUBA": "Japaratuba",
    "SÃO LOURENÇO DA MATA": "São Lourenço da Mata",
    "Rorainopolis": "Rorainópolis",
    "UNA": "Una",
    "una": "Una",
    "Maues": "Maués",
    "Humaita": "Humaitá",
    "Amazonas": np.nan,
    "San Carlos de Rio Negro": "Río Negro",
    "Jacarepaguá": "Rio de Janeiro",
    "Paraty": "Parati",
    "Marica": "Maricá",
    "Agua Quente": np.nan,
    "Serra do Sincorá": np.nan,
    "Ileheus": "Ilhéus",
    "BUERAREMA": "Buerarema",
    "Sanatrem": "Santarém",
    "Parangua": "Paranaguá",
    "Guaraquecaba": "Guaraqueçaba",
    "Mazagao": "Mazagão", 
    "San Yungas": "Sud Yungas"
}

LEVEL2_TO_LEVEL1 = {
    "Milne Bay": "Milne Bay",
    "Western Highlands Province": "Western Highlands"
}

