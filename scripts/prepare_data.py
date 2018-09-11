"""Script to prepare predictors for comparison of methods to predict if a species is
threatened or not, without doing a full Red List assessment.

This prepares two different types of predictor: "specimen" predictors, which are
derived directly from specimen records and each specimen record is an individual
observation, and predictors for random forests, which are derived for each species
from all the specimen records for that species.

Both require two data sources: a list of specimen records, and a list of Red List
assessment information.

Other data is also required:
Shapefiles to create a locality map, taken from GADM, https://gadm.org/download_world.html.
A map of biogeographic realms, taken from https://ecoregions2017.appspot.com.
Rasters of environmental variables, http://www.worldclim.org/bioclim,
human population density, http://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev10,
human footprint, https://datadryad.org//resource/doi:10.5061/dryad.052q5,
and GDP, http://sedac.ciesin.columbia.edu/data/set/spatialecon-gecon-v4.
"""
import importlib.util
import re
import rasterio
import os

import geopandas as gpd
import numpy as np
import pandas as pd

from collections import defaultdict
from datetime import datetime
from rasterio.mask import mask
from rasterio.plot import show
from shapely.geometry import mapping
from shapely.geometry import Point, MultiPoint, Polygon, MultiPolygon

from prepare_data_settings import *


def load_specimen_data(specimen_data_path):
    """Load and standardise the specimen data and return a DataFrame."""

    specimen_data = (pd.read_csv(specimen_data_path)
                       .assign(group=lambda x: x.group.replace(GROUP_NAMES)))
                    
    return specimen_data


def load_assessment_data(assessment_path):
    """Load and standardise the assessment data and return a DataFrame."""

    assessment_data = (pd.read_csv(assessment_path)
                         .assign(species=lambda x: x.species.str.lower(),
                                 group=lambda x: x.group.replace(GROUP_NAMES)))
    
    return assessment_data


def make_range_polygons(occurrences):
    """Makes range polygons for each species from the Minimum Convex Polygon of occurrences."""
    polygons = (occurrences.groupby("species")
                            .agg({"longitude": lambda g: MultiPoint([xy for xy in occurrences.loc[g.index, ["longitude", "latitude"]].values]).convex_hull,
                                  "latitude": lambda g: len(g)})
                            .reset_index()
                            .rename(columns={"longitude": "geometry",
                                             "latitude": "n_occurrences"}))

    return polygons


def extract_raster_value(range_poly, raster, func=np.nanmean, buffer=None):
    """Extract the value of a raster for a species range.

    Summarises the value of a raster for a species range using
    a given summary function, optionally buffering the range.
    """
    if buffer is not None:
        if range_poly.type != "Point":
            range_poly = range_poly.buffer(buffer)
        
    geoms = [mapping(range_poly)]
    out_image, out_transform = mask(raster, geoms, crop=True)
    data = out_image.data[0]
    no_data = raster.nodata
    values = np.extract(~np.isclose(data, no_data), data)
    if len(values) < 1:
        return np.nan
    
    return func(values)


def prepare_raster_predictor(raster_path, data, **kwargs):
    """Prepare a predictor from a raster file."""
    summary_func = kwargs.get("func", np.nanmean)

    with rasterio.open(raster_path) as raster:
        resolution_x = raster.affine[0]
        resolution_y = -raster.affine[4]
        buffer = kwargs.get("buffer", np.max([resolution_x, resolution_y]) / 2)
        predictor_value = data.geometry.apply(lambda x: extract_raster_value(x, raster, func=summary_func, buffer=buffer))

    return predictor_value


def calculate_range_overlap(inspected_range, all_ranges):
    """Calculate the area of overlap between the range of one species and all others.
    """
    if not isinstance(inspected_range, Polygon):
        return pd.Series([0]*len(all_ranges))
    
    return all_ranges.apply(lambda x: x.intersection(inspected_range).area / inspected_range.area)


def calculate_eti(species_row, all_data, threat_idx):
    """Calculate the external threat index for a species.

    The external threat index is calculated as the mean threat index
    of all species with ranges that overlap the range of the species of
    interest, weighted by the range overlap.
    """
    overlaps = calculate_range_overlap(species_row.geometry, all_data.geometry)
    overlaps.loc[species_row.name] = 0
    
    if overlaps.sum() == 0:
        return 0
    
    return (overlaps * threat_idx).sum() / overlaps.sum()


def fill_nearest_value(point, df, field):
    """Fills a field using the value of the nearest neighbour."""
    nearest_point = df.loc[df[field].notnull()].distance(point).idxmin()

    return df.loc[nearest_point, field]


def fill_from_lookup(data, lookup, data_field, lookup_field=None):
    """Fills a field using a lookup dictionary."""
    if lookup_field is None:
        lookup_field = data_field

    return data.apply(lambda row: lookup[lookup_field].get(row.species, row[data_field]) if pd.isnull(row[data_field]) else row[data_field], axis=1)


def fill_from_redlist_api(predictor_data, predictors_to_fill):
    """Fill gaps in the predictor data using the Red List API."""
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

    species_results = defaultdict(dict)
    for species in species_to_scrape:
        result = redlist.get_species_info("details", species, field="name")
        if result is not None:
            for key, value in result[0].items():
                species_results[key][species] = value

    habitat_results = defaultdict(dict)
    for species in habitats_to_scrape:
        result = redlist.get_species_info("habitats", species, field="name")
        if result is not None:
            habitat_results["n_habitats"][species] = len(result)

    for predictor in predictors_to_fill:
        lookup_field = {"elevation_min": "elevation_lower", "elevation_max": "elevation_upper"}.get(predictor, predictor)
        filled_data[predictor] = fill_from_lookup(filled_data, species_results, predictor, lookup_field)
    
    return filled_data


def convert_to_gdf(df, crs=DEFAULT_CRS, reproject_crs=None):
    """Converts a DataFrame of specimen occurrences to a GeoDataFrame."""
    if reproject_crs is None:
        reproject_crs = crs

    gdf = (df.assign(geometry=lambda x: x.apply(lambda row: Point((row.longitude, row.latitude)), axis=1))
             .pipe(lambda x: gpd.GeoDataFrame(x, crs=crs))
             .to_crs(reproject_crs))

    return gdf


def fill_from_realm_map(predictor_data, point_data):
    """Fill missing biogeographic realm data from a map of the realms.

    Fills the realm of a species using a reference map by:
    1. Finding the realm that most of the specimens occur in.
    2. Assigning the closest realm for species that fall in the gaps of the map.
    """
    predictor_data = predictor_data.copy()

    realms = (gpd.read_file("../data/realms_map.json")
                 .to_crs(predictor_data.crs)
                 .assign(realm=lambda x: x.realm.replace(REALM_NAME_MAP)))
    

    all_points = (point_data.loc[lambda x: x.latitude.notnull() & x.longitude.notnull() &\
                                 (x.latitude != -9999) & (x.longitude != -9999) &\
                                 ((x.longitude != 0) | (x.latitude != 0))]
                            .pipe(lambda x: convert_to_gdf(x, reproject_crs=predictor_data.crs)
                            .loc[:, ["species", "geometry"]]))

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


def fill_range(data):
    """Fills missing range values from the MCP area."""
    return data.apply(lambda x: x.geometry.area / 1e6 if ((x.n_occurrences > 2) & pd.isnull(x.range_eoo)) else x.range_eoo, axis=1)


def fill_missing_random_forests_predictors(data, point_data):
    """Fill missing data for random forests predictors.

    Fills missing data by:
    1. Querying the Red List API.
    2. Calculating the range from the area of the MCP.
    3. Filling the realm from a reference map.
    4. Calculating elevation predictors from rasters.
    5. Assigning the value of the nearest specimen for raster predictors.
    """
    filled_data = (data.pipe(fill_from_redlist_api, ["category", "criteria", "elevation_min", "elevation_max", "n_habitats"])
                       .assign(range_eoo=lambda df: fill_range(df),
                               realm_value=lambda df: fill_from_realm_map(df, point_data)))

    # fill elevation from a bioclim raster
    if filled_data.elevation_min.isnull().any():
        filled_data.loc[filled_data.elevation_min.isnull(), "elevation_min"] = \
            prepare_raster_predictor("../data/elevation.tif", filled_data, func=np.nanmin)
    
    if filled_data.elevation_max.isnull().any():
        filled_data.loc[filled_data.elevation_max.isnull(), "elevation_max"] = \
        prepare_raster_predictor("../data/elevation.tif", filled_data, func=np.nanmax)

    raster_cols = ["av_temp", "season_temp", "season_precip", "av_precip", 
                   "mean_hpd", "min_hpd", "mean_hfi", "mean_gdp"]
    for col in raster_cols:
        filled_data[col] = filled_data.apply(lambda x: fill_nearest_value(x.geometry, filled_data, col) if pd.isnull(x[col]) else x[col], axis=1)
    
    return filled_data


def clean_range(range_str):
    """Clean the range string for a species, given by the Red List.
    """
    if isinstance(range_str, str):
        range_str = range_str.strip("<>")
        if "-" in range_str:
            lower_range, upper_range = range_str.split("-")

            if not all([lower_range.isdecimal(), upper_range.isdecimal()]):
                return np.nan

            return (float(upper_range) - float(lower_range)) / 2

        if range_str.isdecimal():
            return float(range_str)

        return np.nan
    
    return range_str


def clean_random_forests_predictors(random_forests_predictors):
    """Cleans random forests predictors.

    Cleans random forests predictors by fixing bad or empty range values,
    removing duplicated species and species with missing predictor information,
    and removing species with unfeasibly large ranges.
    """
    random_forests_predictors = (random_forests_predictors.assign(range_eoo=lambda df: df.range_eoo.apply(clean_range))
                                                          .assign(range_eoo=lambda df: fill_range(df))
                                                          .assign(range_eoo=lambda df: df.range_eoo.apply(lambda x: 0 if pd.isnull(x) else x))
                                                          .drop_duplicates(subset=["species"])
                                                          .dropna(subset=[column for column in RF_COLUMNS if column != "criteria"]))

    range_widths = random_forests_predictors.geometry.bounds.maxx - random_forests_predictors.geometry.bounds.minx
    random_forests_predictors = random_forests_predictors.loc[range_widths < 2e7]
    
    return random_forests_predictors


def clean_date_string(date_string):
    """Finds the year in possible date strings."""
    date_found = re.match("[12][78901][\d]{2}", str(date_string))
    if date_found:
        return date_found.group()

    return np.nan


def load_GADM(level=0):
    """Loads a GADM shape file as a GeoDataFrame."""
    return (gpd.read_file(GADM_PATH.format(level))
               .assign(NAME=lambda x: x["NAME_{}".format(level)]))


def append_GADM(appending_to, level):
    """Appends a higher GADM level to a lower one and returns a GeoDataFrame."""
    return (load_GADM(level=level).loc[lambda x: ~x["GID_{}".format(level)].isin(appending_to["GID_{}".format(level)])]
                                  .append(appending_to, ignore_index=True))


def build_continent_reference(simple_world=None):
    """Makes a reference of GADM level 0 to continents on a simple map as a GeoDataFrame."""
    if simple_world is None:
        simple_world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))

    level0 = load_GADM(level=0)

    reference_map = level0.merge(simple_world.drop(["geometry"], axis=1),
                                 left_on="GID_0", right_on="iso_a3", 
                                 how="left")

    reference_map.loc[reference_map.continent.isnull(), "continent"] = \
        (reference_map.loc[reference_map.continent.isnull()]
                      .centroid
                      .apply(lambda x: simple_world.distance(x).idxmin())
                      .apply(lambda x: simple_world.loc[x, "continent"]))

    return reference_map


def build_locality_map(continent_lookup=None):
    """Builds a map of all localities from GADM shapefiles and returns a GeoDataFrame.

    Builds a map of all localities for GADM levels 0 to 3 with each row being an 
    admin area at the finest-grained level for that particular country. A column for the
    continent of the admin area can be added by providing a low-res continent map.
    """
    locality_map = (load_GADM(level=3).pipe(append_GADM, 2)
                                      .pipe(append_GADM, 1)
                                      .pipe(append_GADM, 0))
    
    if continent_lookup is not None:        
        locality_map = locality_map.merge(continent_lookup.loc[:, ["GID_0", "continent"]], on="GID_0")

    return locality_map


def is_bad_point(df):
    """Identify a specimen with bad coordinates."""
    null_points = df.latitude.isnull() | df.longitude.isnull()
    null_placeholder = (df.latitude == -9999) | (df.longitude == -9999)
    zero_points = (df.longitude == 0) & (df.latitude == 0)
    
    return null_points | null_placeholder | zero_points


def find_locality_by_continent(point, locality_map, continent_map):
    """Find the closest locality to a point from the closest continent."""
    nearest_continent = continent_map.distance(point).idxmin()
    
    distances = (locality_map.loc[locality_map.continent == nearest_continent]
                             .distance(point))

    return {"distance": distances.min(), "idx": distances.idxmin()}


def fill_missing_localities(gdf, locality_map, simple_world=None):
    """Fills missing localities for specimens with coordinates.
    
    Finds localities for specimens with coordinates that are not exactly within
    a GADM admin area by assigning the closest admin area to it. To speed this up,
    the closest continent is first found from a simpler map.
    """
    gdf = gdf.copy()

    if simple_world is None:
        simple_world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))

    simple_continents = simple_world.dissolve(by="continent")

    subset_gdf = (gdf.loc[gdf.NAME.isnull() & ~gdf.bad_point]
                     .drop([column for column in gdf if column.startswith("NAME")], axis=1)
                     .copy())

    subset_found = (subset_gdf.assign(found_locality=lambda df: df.geometry.apply(lambda x: find_locality_by_continent(x, locality_map, simple_continents)))
                              .assign(min_distance=lambda df: df.found_locality.apply(lambda x: x.get("distance")),
                                      place_id=lambda df: df.found_locality.apply(lambda x: x.get("idx")))
                              .merge(locality_map.loc[:, ["NAME", "NAME_0", "NAME_1", "NAME_2", "NAME_3"]], left_on="place_id", right_index=True)
                              .drop(["found_locality", "min_distance", "place_id"], axis=1))

    gdf.loc[subset_gdf.index] = subset_found
    return gdf


def assign_bad_coord_localities(df):
    """Assigns a locality from specimen information for specimens with no coordinates."""
    return df.assign(NAME_0=lambda df: df.apply(lambda x: x.country if pd.isnull(x.latitude) else x.NAME_0, axis=1),
                     NAME_1=lambda df: df.apply(lambda x: x.majorarea if pd.isnull(x.latitude) else x.NAME_1, axis=1),
                     NAME_2=lambda df: df.apply(lambda x: x.minorarea if pd.isnull(x.latitude) else x.NAME_2, axis=1),
                     NAME_3=lambda df: df.apply(lambda x: np.nan if pd.isnull(x.latitude) else x.NAME_3, axis=1),
                     NAME=lambda df: df.apply(lambda x: x.country if pd.isnull(x.latitude) else x.NAME, axis=1))


def standardise_localities(df):
    """Standardises localities taken from specimen information to GADM areas."""
    return (df.assign(NAME_1=lambda df: df.NAME_1.str.strip(" ?"),
                      NAME_2=lambda df: df.NAME_2.str.strip(" ?"))
              .assign(NAME_1=lambda df: df.NAME_1.replace(LEVEL1_CORRECTIONS))
              .assign(NAME_1=lambda df: df.apply(lambda x: LEVEL2_TO_LEVEL1.get(x.NAME_2, x.NAME_1), axis=1))
              .assign(NAME_2=lambda df: df.NAME_2.replace(LEVEL2_CORRECTIONS)))


def find_locality(gdf):
    """Finds the localities for all points in a geodataframe.

    For specimens with coordinates, this back-computes the locality using
    GADM administrative areas by:
    1. Building a lookup of continents for countries at GADM level 0.
    2. Building a reference map of localities at the most detailed GADM level for all countries.
    3. Filling the locality for any specimens missing one.
    4. Assigning localities to specimens with no coordinates from their locality information.
    """
    simple_world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
    continent_lookup = build_continent_reference(simple_world=simple_world)
    locality_map = build_locality_map(continent_lookup=continent_lookup)

    return (gdf.pipe(lambda df: gpd.sjoin(df, locality_map, how="left", op="within"))
               .pipe(lambda df: fill_missing_localities(df, locality_map))
               .pipe(lambda df: assign_bad_coord_localities(df)))


def prepare_specimen_predictors(specimen_data_path, assessment_path):
    """Prepare predictors that are at the specimen level, for any method.

    Prepares predictors at the specimen level by:
    1. Cleaning dates and identifying specimens with bad or no coordinates.
    2. Joining the specimen data to the assessment data from the Red List.
    3. Filling any missing data using the Red List API.
    4. Back computing localities for georeferenced specimens.
    5. Standardising localities for ungeoreferenced specimens.
    6. Removing unneccessary columns and species with assessments.
    """
    assessment_data = load_assessment_data(assessment_path)
    specimen_data = load_specimen_data(specimen_data_path)

    cleaned_specimen_data = (specimen_data.assign(date_collected=lambda x: x.date_collected.apply(clean_date_string),
                                                  bad_point=lambda x: is_bad_point(x))
                                          .assign(latitude=lambda x: x.apply(lambda row: np.nan if row.bad_point else row.latitude, axis=1),
                                                  longitude=lambda x: x.apply(lambda row: np.nan if row.bad_point else row.longitude, axis=1))
                                          .merge(assessment_data.loc[:, ["species", "group", "category", "criteria"]], 
                                                 on=["group", "species"], how="left")
                                          .pipe(fill_from_redlist_api, ["category", "criteria"])
                                          .pipe(convert_to_gdf)
                                          .pipe(find_locality)
                                          .pipe(standardise_localities)
                                          .drop(["geometry", "NAME"], axis=1)
                                          .rename(columns=lambda x: x.replace("NAME", "locality"))
                                          .rename(columns={"date_collected": "year"})
                                          .loc[:, SPECIMEN_COLUMNS])

    return cleaned_specimen_data


def prepare_random_forests_predictors(specimen_data_path, assessment_path):
    """Prepare the predictors for a random forests model.

    Prepares the predictors for the random forests model by:
    1. Removing specimens with no or bad coordinates.
    2. Making range polygons of each species using the MCP of the specimens.
    3. Joining this to the assessment data from the Red List.
    4. Deriving predictor data from the MCP of each species.
    5. Filling missing predictor data.
    6. Removing unneccessary columns and species without assessments.
    """
    assessment_data = load_assessment_data(assessment_path)
    point_data = pd.read_csv(specimen_data_path)

    random_forests_predictors = (point_data.loc[lambda x: ~is_bad_point(x)]
                                            .pipe(make_range_polygons)
                                            .merge(assessment_data, on="species")
                                            .pipe(lambda x: gpd.GeoDataFrame(x, crs=DEFAULT_CRS))
                                            .assign(latitude_centroid=lambda x: x.geometry.centroid.y)
                                            .to_crs(EQUAL_AREA_CRS)
                                            .assign(av_temp=lambda x: prepare_raster_predictor("../data/annual_temperature.tif", x),
                                                    season_temp=lambda x: prepare_raster_predictor("../data/season_temp.tif", x),
                                                    av_precip=lambda x: prepare_raster_predictor("../data/annual_precip.tif", x),
                                                    season_precip=lambda x: prepare_raster_predictor("../data/season_precip.tif", x),
                                                    mean_hpd=lambda x: prepare_raster_predictor("../data/human_density.tif", x),
                                                    min_hpd=lambda x: prepare_raster_predictor("../data/human_density.tif", x, func=np.nanmin),
                                                    mean_hfi=lambda x: prepare_raster_predictor("../data/human_footprint.tif", x),
                                                    mean_gdp=lambda x: prepare_raster_predictor("../data/gdp.tif", x),
                                                    category=lambda x: pd.Categorical(x.category, ordered=True, categories=["LC", "NT", "VU", "EN", "CR"]),
                                                    geometry=lambda x: x.geometry.apply(lambda geom: geom.buffer(0) if not geom.is_valid else geom)
                                                    ))
    threat_idx = random_forests_predictors.category.cat.codes.replace(-1, 0)

    random_forests_predictors = (random_forests_predictors.assign(eti=lambda x: x.apply(lambda row: calculate_eti(row, x, threat_idx), axis=1))
                                                          .rename(columns={"range": "range_eoo"})
                                                          .pipe(lambda x: fill_missing_random_forests_predictors(x, point_data))
                                                          .pipe(clean_random_forests_predictors)
                                                          .loc[lambda x: x.category.notnull() & (x.category != "DD")]
                                                          .loc[:, RF_COLUMNS])
                                                          
    return random_forests_predictors


def main():
    date = datetime.now().strftime("%Y%m%d")
    specimen_predictors_outfile = os.path.join("..", "output", "specimen_predictors_{}.csv".format(date))
    rf_predictors_outfile = os.path.join("..", "output", "random_forests_predictors_{}.csv".format(date))

    specimen_data_file = os.path.join("..", "data", "all_specimen_data.csv")
    assessmen_data_file = os.path.join("..", "data", "all_assessments.csv")

    random_forests_predictors = prepare_random_forests_predictors(specimen_data_file, assessmen_data_file)
    random_forests_predictors.to_csv(rf_predictors_outfile, index=False, encoding="utf-8")

    specimen_predictors = prepare_specimen_predictors(specimen_data_file, assessmen_data_file)
    specimen_predictors.to_csv(specimen_predictors_outfile, index=False, encoding="utf-8")


if __name__ == "__main__":
    main()
