"""This is a script to collect and prepare data for analysis.
"""
import importlib.util
import re
import rasterio
import os

import geopandas as gpd
import numpy as np
import pandas as pd

from rasterio.mask import mask
from rasterio.plot import show
from shapely.geometry import mapping
from shapely.geometry import Point, MultiPoint, Polygon, MultiPolygon


NAME_CORRECTION_MAP = {"dendrobium odoardoi": "dendrobium odoardi",
                       "bulbophyllum djikstalianum": "bulbophyllum dijkstalianum",
                       "bulbophyllum epicranthes": "bulbophyllum epicrianthes",
                       "bulbophyllum brachytricha": "bulbophyllum brachytriche",
                       "dendrobium renellii": "dendrobium rennellii",
                       "coffea farafangensis": "coffea farafanganensis",
                       "coffea neoleyroi": "coffea neoleroyi",
                       "coffea rakatonasoloi": "coffea rakotonasoloi",
                       "coffea travencorensis": "coffea travancorensis"}

POINT_COLUMNS = ["species", "latitude", "longitude"]

SPECIMEN_COLUMNS = ["species", "date_collected", "minorarea", "majorarea"]

COORDINATE_COLUMNS = ["species", "latitude", "longitude", "group", "category", "criteria"]

RF_COLUMNS = ["species", "genus", "family", "order", "n_habitats",
              "realm_value", "elevation_min", "elevation_max", "range_eoo", 
              "latitude_centroid", "av_temp", "season_temp", "av_precip", "season_precip",
              "mean_hpd", "min_hpd", "mean_hfi", "mean_gdp", "eti", "group", "category", "criteria"]

US_COLUMNS = ["species", "year", "minorarea", "majorarea", "group", "category", "criteria"]
US_COMPLETE_GROUPS = ["MadPalms", "Myrcia", "OrchidsNG"]

GROUP_PATHS = {
    "coffee": "coffee_data",
    "legumes": "legume_data",
    "madagascar_palms": "madagascar_palms_data",
    "myrcia": "aulomyrcia_data",
    "orchids": "ng_orchid_data",
}

GROUP_NAMES = {
    "coffee": "Coffee",
    "legumes": "Legumes",
    "madagascar_palms": "MadPalms",
    "myrts": "Myrcia",
    "ng_orchids_p": "OrchidsNG"
}

DEFAULT_CRS = {'proj': 'latlong', 'ellps': 'WGS84', 'datum': 'WGS84', 'no_defs': True}


def clean_data(data):
    """Simple cleaning of certain fields in the data.
    """
    cleaned_data = data.copy()

    cleaned_data["name"] = cleaned_data.genus.str.lower() + " " + cleaned_data.species
    cleaned_data["latitude"] = pd.to_numeric(cleaned_data.latitude, "coerce")
    cleaned_data["longitude"] = pd.to_numeric(cleaned_data.longitude, "coerce")
    cleaned_data["species"] = cleaned_data.name.replace(NAME_CORRECTION_MAP)
        
    return cleaned_data
    

def load_point_data(folder, path_map):
    """Loads the point data for each group.
    """
    name_template = os.path.join(folder, "{}.csv")
    loaded_data = {name: pd.read_csv(name_template.format(path)) for name, path in path_map.items()}
    cleaned_data = [clean_data(data) for group, data in loaded_data.items()]

    point_data = pd.concat(cleaned_data, ignore_index=True)
    return point_data


def load_assessment_data(assessment_path):
    """Loads the assessment data for everything.
    """
    assessment_data = pd.read_csv(assessment_path)
    assessment_data["species"] = assessment_data.species.str.lower()
    assessment_data["group"] = assessment_data.group.replace(GROUP_NAMES)

    return assessment_data


def calculate_ranges(point_data):
    """Calculates range polygons for each species using Minimum Convex Polygon
    method.
    """
    polygons = (point_data.groupby("species")
                            .agg({"longitude": lambda g: MultiPoint([xy for xy in zip(g.tolist(), point_data.loc[g.index, "latitude"].tolist())]).convex_hull,
                                  "latitude": lambda g: len(g)})
                            .reset_index()
                            .rename(columns={"longitude": "geometry",
                                             "latitude": "n_occurrences"}))

    return polygons


def get_range_value(range_poly, raster, func=np.nanmean, buffer=None):
    """Extract a summary value for a range from a raster with a specified
    summary function, buffering if necessary.
    """
    if buffer is not None:
        if range_poly.type != "Point":
            range_poly = range_poly.buffer(buffer)
        
    geoms = [mapping(range_poly)]
    out_image, _ = mask(raster, geoms, crop=True)
    data = out_image.data[0]
    no_data = raster.nodata
    values = np.extract(~np.isclose(data, no_data), data)
    if len(values) < 1:
        return np.nan
    
    return func(values)


def prepare_raster_predictor(raster_path, data, **kwargs):
    """Prepare a predictor from a raster file.
    """
    summary_func = kwargs.get("func", np.nanmean)

    with rasterio.open(raster_path) as raster:
        resolution_x = raster.affine[0]
        resolution_y = -raster.affine[4]
        buffer = np.max([resolution_x, resolution_y]) / 2
        predictor_value = data.geometry.apply(lambda x: get_range_value(x, raster, func=summary_func, buffer=buffer))

    return predictor_value


def get_overlaps(inspected_range, all_ranges):
    """Calculate the area of overlap between the range of one species and all others.
    """
    if not isinstance(inspected_range, Polygon):
        return pd.Series([0]*len(all_ranges))
    
    return all_ranges.apply(lambda x: x.intersection(inspected_range).area / inspected_range.area)


def calculate_eti(species_row, all_data, threat_idx):
    """Calculate the external threat index for a species.
    """
    overlaps = get_overlaps(species_row.geometry, all_data.geometry)
    overlaps.loc[species_row.name] = 0
    
    if overlaps.sum() == 0:
        return 0
    
    return (overlaps * threat_idx).sum() / overlaps.sum()


def fill_from_redlist_api(predictor_data, predictors_to_fill):
    """Fill gaps in the predictor data using the Red List API.
    """
    filled_data = predictor_data.copy()

    spec = importlib.util.spec_from_file_location("redlist_api", "../scripts/redlist_api.py")
    redlist_api = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(redlist_api)

    redlist = redlist_api.redListGetter(token=REDLIST_API_TOKEN)

    species_to_scrape = []
    species_predictors = [predictor for predictor in predictors_to_fill if predictor in ("elevation_max", "elevation_min", "category", "criteria")]
    if species_predictors:
        species_to_scrape = filled_data.loc[filled_data[species_predictors].isnull().any(axis=1), "species"].unique().tolist()

    habitats_to_scrape = []
    if "n_habitats" in predictors_to_fill:
        habitats_to_scrape = filled_data.loc[filled_data.n_habitats.isnull(), "species"].unique().tolist()

    species_results = []
    for species in species_to_scrape:
        result = redlist.get_species_info("details", species, field="name")
        if result is not None:
            species_results.extend(result)

    if species_results:
        species_info = {result["scientific_name"].lower(): {"category": result["category"],
                                                             "criteria": result["criteria"],
                                                             "elevation_min": result["elevation_lower"],
                                                             "elevation_max": result["elevation_upper"]}
                        for result in species_results}

    habitat_results = {}
    for species in habitats_to_scrape:
        result = redlist.get_species_info("habitats", species, field="name")
        if result is not None:
            habitat_results.update({species: len(result)})

    for predictor in predictors_to_fill:
        if predictor == "category":
            filled_data.loc[filled_data.category.isnull(), "category"] = pd.Categorical(
                (filled_data.loc[filled_data.category.isnull()]
                        .apply(lambda x: species_info[x.species].get("category", np.nan) if x.species in species_info else np.nan, axis=1)),
            ordered=True,
            categories=["LC", "NT", "VU", "EN", "CR"]
        )
        elif predictor == "n_habitats":
            filled_data.loc[filled_data.n_habitats.isnull(), "n_habitats"] = (filled_data.loc[filled_data.n_habitats.isnull()]
                                                                    .apply(lambda x: habitat_results.get(x.species, np.nan), axis=1))
        else:
            filled_data.loc[filled_data[predictor].isnull(), predictor] = (filled_data.loc[filled_data[predictor].isnull()]
                                                                            .apply(lambda x: species_info[x.species].get(predictor, np.nan) if x.species in species_info else np.nan, axis=1))
    
    return filled_data


def fill_from_realm_map(predictor_data, point_data):
    """Fill missing biogeographic realm data from a map
    of the realms.
    """
    predictor_data = predictor_data.copy()

    realms = gpd.read_file("../data/realms_map.json")
    realms = realms.to_crs(predictor_data.crs)
    realms["realm"] = realms.realm.replace({"Afrotropic": "Afrotropical", 
                                            "Neotropic": "Neotropical",
                                            "Australasia": "Australasian",
                                            "Indo-Malay": "Indomalayan"})

    all_points = (gpd.GeoDataFrame({"species": point_data.species, 
                                    "geometry": point_data.apply(lambda x: Point((x.longitude, x.latitude)), axis=1)}, crs=DEFAULT_CRS)
                    .to_crs(predictor_data.crs))

    realm_fillers = (gpd.sjoin(all_points.loc[all_points.species.isin(predictor_data.loc[predictor_data.realm_value.isnull(), "species"])], 
                               realms, 
                               how="left", 
                               op="within")
                        .groupby("species")
                        .realm.agg(lambda x: x.value_counts().index[0] if len(x.value_counts()) > 1 else x.value_counts().index))

    predictor_data["realm_value"] = (predictor_data.merge(realm_fillers.reset_index(), left_on="species", right_on="species", how="left")
                                                   .apply(lambda x: x.realm if pd.isnull(x.realm_value) else x.realm_value, axis=1))

    # some species fall in the gap between realms
    gap_fillers = (all_points.loc[all_points.species.isin(predictor_data.loc[predictor_data.realm_value.astype(str) == "[]", "species"])]
         .groupby("species")
         .agg(lambda x: x.geometry.apply(lambda x: realms.loc[realms.distance(x).sort_values().index[0]].realm).value_counts().index[0])
         .reset_index()
         .rename(columns={"geometry": "realm"}))
    
    predictor_data["realm_value"] = (predictor_data.merge(gap_fillers.reset_index(), left_on="species", right_on="species", how="left")
                                                   .apply(lambda x: x.realm if pd.isnull(x.realm_value) else x.realm_value, axis=1))

    return predictor_data.realm_value


def fill_missing_random_forests_data(data, point_data):
    """Fill missing data for random forests predictors.
    """
    filled_data = data.copy()

    # fill from the Red List API
    filled_data = fill_from_redlist_api(filled_data, ["category", "criteria", "elevation_min", "elevation_max", "n_habitats"])
    # fill empty ranges by calculating from MCP
    filled_data.loc[(filled_data.n_occurrences > 2) & filled_data.range_eoo.isnull(), "range_eoo"] = filled_data.loc[(filled_data.n_occurrences > 2) & filled_data.range_eoo.isnull(), "geometry"].area / 1e6

    # fill realm from reference map
    filled_data["realm_value"] = fill_from_realm_map(filled_data, point_data)

    # fill elevation from a bioclim raster
    filled_data.loc[filled_data.elevation_min.isnull(), "elevation_min"] = prepare_raster_predictor("../data/elevation.tif", 
                                                                                                    filled_data.loc[filled_data.elevation_min.isnull()], 
                                                                                                    func=np.nanmin)
    filled_data.loc[filled_data.elevation_max.isnull(), "elevation_max"] = prepare_raster_predictor("../data/elevation.tif", 
                                                                                                    filled_data.loc[filled_data.elevation_max.isnull()], 
                                                                                                    func=np.nanmax)
    
    return filled_data


def clean_range(range_str):
    """Clean the range string for a species, given by the Red List.
    """
    if isinstance(range_str, str):
        range_str = range_str.strip("<>")
        if "-" in range_str:
            lower_range, upper_range = range_str.split("-")
            return (float(upper_range) + float(lower_range)) / 2

        try:
            return float(range_str)
        except ValueError:
            return 0
    
    return range_str


def clean_random_forests_predictors(random_forests_predictors):
    """Clean the random forests predictors.
    """
    random_forests_predictors = random_forests_predictors.copy()
    random_forests_predictors["range_eoo"] = random_forests_predictors.range_eoo.apply(clean_range)

    random_forests_predictors = (random_forests_predictors.drop_duplicates(subset=["species"])
                                                          .dropna(subset=[column for column in RF_COLUMNS if column != "criteria"]))

    range_widths = random_forests_predictors.geometry.bounds.maxx - random_forests_predictors.geometry.bounds.minx
    random_forests_predictors = random_forests_predictors.loc[range_widths < 2e7]
    
    return random_forests_predictors


def prepare_coordinate_predictors(point_data_path, assessment_path):
    """Prepare predictors for methods that only use the coordinates of
    a specimen.
    """
    point_data = load_point_data(point_data_path, GROUP_PATHS)
    point_data = point_data.loc[:, POINT_COLUMNS]

    assessment_data = load_assessment_data(assessment_path)

    coordinate_predictors = point_data.merge(assessment_data.loc[:, ["species", "category", "criteria", "group"]], 
                                             left_on="species", 
                                             right_on="species")

    coordinate_predictors = fill_from_redlist_api(coordinate_predictors, ["category"])

    coordinate_predictors = (coordinate_predictors.loc[lambda x: x.category.notnull() & (x.category != "DD")]
                                                  .loc[lambda x: x.latitude.notnull() & x.longitude.notnull() & (x.latitude > -9999) & (x.longitude > -9999) & ((x.longitude != 0) & (x.latitude != 0))]
                                                  .loc[:, ["species", "longitude", "latitude", "category", "criteria", "group"]])

    return coordinate_predictors


def prepare_random_forests_predictors(point_data_path, assessment_path):
    """Prepare the predictors for a random forests model.
    """
    point_data = load_point_data(point_data_path, GROUP_PATHS)
    point_data = (point_data.loc[lambda x: x.latitude.notnull() & x.longitude.notnull() & (x.latitude > -9999) & (x.longitude > -9999) & ((x.longitude != 0) & (x.latitude != 0))]
                            .loc[:, POINT_COLUMNS])

    assessment_data = load_assessment_data(assessment_path)

    range_polygons = calculate_ranges(point_data)
    random_forests_predictors = gpd.GeoDataFrame(range_polygons.merge(assessment_data, left_on="species", right_on="species"), crs=DEFAULT_CRS)

    random_forests_predictors["latitude_centroid"] = random_forests_predictors.geometry.centroid.y

    # reproject to equal area for the rest of the range calculations
    random_forests_predictors = random_forests_predictors.to_crs({"proj": "moll", "lon_0": 0, "lat_0": 0, "ellps": "WGS84", "units": "m", "no_defs": True})

    random_forests_predictors["av_temp"] = prepare_raster_predictor("../data/annual_temperature.tif", random_forests_predictors)
    random_forests_predictors["season_temp"] = prepare_raster_predictor("../data/season_temp.tif", random_forests_predictors)
    random_forests_predictors["av_precip"] = prepare_raster_predictor("../data/annual_precip.tif", random_forests_predictors)
    random_forests_predictors["season_precip"] = prepare_raster_predictor("../data/season_precip.tif", random_forests_predictors)
    random_forests_predictors["mean_hpd"] = prepare_raster_predictor("../data/human_density.tif", random_forests_predictors)
    random_forests_predictors["min_hpd"] = prepare_raster_predictor("../data/human_density.tif", random_forests_predictors, func=np.nanmin)
    random_forests_predictors["mean_hfi"] = prepare_raster_predictor("../data/human_footprint.tif", random_forests_predictors)
    random_forests_predictors["mean_gdp"] = prepare_raster_predictor("../data/gdp.tif", random_forests_predictors)

    random_forests_predictors["category"] = pd.Categorical(random_forests_predictors.category, ordered=True, categories=["LC", "NT", "VU", "EN", "CR"])
    threat_idx = random_forests_predictors.category.cat.codes.replace(-1, 0)
    random_forests_predictors.loc[~random_forests_predictors.geometry.apply(lambda x: x.is_valid), "geometry"] = random_forests_predictors.loc[~random_forests_predictors.geometry.apply(lambda x: x.is_valid), "geometry"].apply(lambda x: x.buffer(0))
    random_forests_predictors["eti"] = random_forests_predictors.apply(lambda row: calculate_eti(row, random_forests_predictors, threat_idx), axis=1)

    random_forests_predictors = random_forests_predictors.rename(columns={"range": "range_eoo"})

    random_forests_predictors = fill_missing_random_forests_data(random_forests_predictors, point_data)

    random_forests_predictors = clean_random_forests_predictors(random_forests_predictors)
    
    random_forests_predictors = random_forests_predictors.loc[random_forests_predictors.category.notnull() & (random_forests_predictors.category != "DD"), RF_COLUMNS]

    return random_forests_predictors


def prepare_us_method_predictors(specimen_data_path, assessment_path):
    """Prepare predictors for the US method.
    """
    specimen_data = load_point_data(specimen_data_path, GROUP_PATHS)
    specimen_data = specimen_data.loc[:, SPECIMEN_COLUMNS].rename(columns={"date_collected": "year"})

    assessment_data = load_assessment_data(assessment_path)
    
    us_method_predictors = specimen_data.merge(assessment_data, left_on="species", right_on="species", how="left")
    
    us_method_predictors = fill_from_redlist_api(us_method_predictors, ["category"])
    us_method_predictors["year"] = us_method_predictors.year.apply(lambda x: re.match("[12][78901][\d]{2}", str(x)).group() if re.match("[12][78901][\d]{2}", str(x)) else np.nan)

    us_method_predictors = (us_method_predictors.loc[us_method_predictors.category.notnull() & (us_method_predictors.category != "DD")]
                                                .loc[:, US_COLUMNS]
                                                .loc[lambda x: x.group.isin(US_COMPLETE_GROUPS)])

    return us_method_predictors


def main():
    coordinate_outfile = os.path.join("..", "output", "coordinate_predictors.csv")
    random_forests_outfile = os.path.join("..", "output", "random_forests_predictors.csv")
    us_method_outfile = os.path.join("..", "output", "us_method_predictors.csv")

    point_data_folder = os.path.join("..", "data")
    assessment_path = os.path.join("..", "data", "all_assessments.csv")

    coordinate_predictors = prepare_coordinate_predictors(point_data_folder, assessment_path)
    #random_forests_predictors = prepare_random_forests_predictors(point_data_folder, assessment_path)
    #us_method_predictors = prepare_us_method_predictors(point_data_folder, assessment_path)

    coordinate_predictors.to_csv(coordinate_outfile, index=False, encoding="utf-8")
    #random_forests_predictors.to_csv(random_forests_outfile, index=False, encoding="utf-8")
    #us_method_predictors.to_csv(us_method_outfile, index=False, encoding="utf-8")


if __name__ == "__main__":
    main()
