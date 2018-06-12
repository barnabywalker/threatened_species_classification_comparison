"""Class definition and associated functions for requesting data from
IUCN Red List. Run as a script it downloads all of the latest redlist.
"""

import requests
import requests_cache
import csv
import datetime
import time

from argparse import ArgumentParser

INFO_TYPES = ["redlist", "threats", "details", "habitats", "countries", 
              "conservation_measures", "citations", "narratives", "growth_forms"]


def region_list(token):
    """Get the list of IUCN regions and identifiers"""

    url = "http://apiv3.iucnredlist.org/api/v3/region/list"
    response = requests.get(url, params={"token": token})
    return response.json()["results"]


def make_request(url, token):
    """Utility to make a request and return JSON data"""

    response = requests.get(url=url, params={"token": token})
    response.raise_for_status()

    json_response = response.json()
    result = json_response.get("result", [])
    if len(result) == 0:
        return
    else:
        return result


def make_throttle_hook(timeout=0.1):
    """Returns a hook that sleeps for timeout seconds if response is
    from cache.
    """
    def hook(response, *args, **kwargs):
        if not getattr(response, "from_cache", False):
            time.sleep(timeout)

        return response

    return hook


class redListGetter(object):
    """An object that gets data from the IUCN red list
    """

    def __init__(self, token=None, cache=True, cache_name=None, delay=0.5):
        self.page_url = "http://apiv3.iucnredlist.org/api/v3/species/page/{}"

        self.species_urls = {"details": "http://apiv3.iucnredlist.org/api/v3/species/{field}/{value}",
                             "threats": "http://apiv3.iucnredlist.org/api/v3/threats/species/{field}/{value}",
                             "habitats": "http://apiv3.iucnredlist.org/api/v3/habitats/species/{field}/{value}",
                             "countries": "http://apiv3.iucnredlist.org/api/v3/species/countries/{field}/{value}",
                             "conservation_measures": "http://apiv3.iucnredlist.org/api/v3/measures/species/{field}/{value}",
                             "citations": "http://apiv3.iucnredlist.org/api/v3/species/citation/{field}/{value}",
                             "narrative": "http://apiv3.iucnredlist.org/api/v3/species/narrative/{field}/{value}",
                             "growth_forms": "http://apiv3.iucnredlist.org/api/v3/growth_forms/species/{field}/{value}"
                            }
        
        if token is None:
            raise ValueError("You must provide a token for the IUCN API")
        else:
            self.token = token

        if cache_name is None:
            self.cache_name = "redlist_api_cache"
        else:
            self.cache_name = cache_name

        self.regions = region_list(self.token)

        if cache:
            requests_cache.install_cache(self.cache_name)
            self.session = requests_cache.CachedSession()
        else:
            self.session = requests.Session()

        self.session.hooks = {"response": make_throttle_hook(delay)}


    def get_page(self, page):
        """Request specific page of species data

        parameters:
        page - str, page number to request
        """

        return make_request(self.page_url.format(page), self.token)


    def get_species_info(self, info_type, value, field="id", region=None):
        """Get a given type of information (e.g. threats, habitats) for given
        species name or id.
        """
        url = self.species_urls.get(info_type)

        if not url:
            raise(ValueError("There is no stored url for this information"))
        else:
            url = url.format(field=field, value=value)

        if (field == "name") & (info_type == "details"):
            url = url.replace("/name", "")

        if field not in ["name", "id"]:
            raise ValueError("Not a recognised species search field")

        if region:
            if region not in self.regions.values():
                raise ValueError("Not a recognised region identifier")
            else:
                url = url + "/region/{}".format(region)

        return make_request(url, self.token)


    def get_all_pages(self):
        """Run requests to get all of the species data"""

        species_data = []

        page_idx = 0
        species_returned = None
        while (page_idx == 0) | (species_returned is not None):
            species_returned = self.get_page(page_idx)
            
            if species_returned:
                species_data.extend(species_returned)

            page_idx = page_idx + 1
            
        return species_data


    def get_all_species_info(self, species_list, info_type, field="id", region=None):
        """Get all of a particular type of info (e.g. threats) for a list of species
        names or ids.
        """
        returned_data = []
        for species in species_list:
            results = self.get_species_info(info_type, species, field=field, region=region)
            if results is not None:
                returned_data.extend(results)

        return returned_data


    def get_region_identifier(self, identifier):
        """Utility to get a region identifier for a region"""

        if identifier not in self.regions:
            raise KeyError("Not a recognised region")


def download_info(info_type, token, save_file=None):
    """Utility to download info from the redlist to a file.
    """
    if info_type not in INFO_TYPES:
        raise ValueError("{} is not a recognised piece of information in the Red List API".format(info_type))

    if save_file is None:
        date = datetime.datetime.now()
        save_file = "../output/redlist_download_{info}_{date}.csv".format(info=info_type, date=date.strftime("%Y%m%d"))

    getter = redListGetter(token=token)
    redlist = getter.get_all_pages()
    plants = [name for name in redlist if name["kingdom_name"] == "PLANTAE"]

    if info_type == "redlist":
        data = plants
    else:
        data = getter.get_all_species_info([plant["taxonid"] for plant in redlist], info_type)

    with open(save_file, "w", newline="") as outfile:
        writer = csv.DictWriter(outfile, fieldnames=data[0].keys())
        writer.writeheader()
        writer.writerows(data)


def log_progress(sequence, every=None, size=None, name='Items'):
    """A widget for logging the progess of the requests.
    Copied from https://github.com/alexanderkuk/log-progress.
    """
    from ipywidgets import IntProgress, HTML, VBox
    from IPython.display import display

    is_iterator = False
    if size is None:
        try:
            size = len(sequence)
        except TypeError:
            is_iterator = True
    if size is not None:
        if every is None:
            if size <= 200:
                every = 1
            else:
                every = int(size / 200)     # every 0.5%
    else:
        assert every is not None, 'sequence is iterator, set every'

    if is_iterator:
        progress = IntProgress(min=0, max=1, value=1)
        progress.bar_style = 'info'
    else:
        progress = IntProgress(min=0, max=size, value=0)
    label = HTML()
    box = VBox(children=[label, progress])
    display(box)

    index = 0
    try:
        for index, record in enumerate(sequence, 1):
            if index == 1 or index % every == 0:
                if is_iterator:
                    label.value = '{name}: {index} / ?'.format(
                        name=name,
                        index=index
                    )
                else:
                    progress.value = index
                    label.value = u'{name}: {index} / {size}'.format(
                        name=name,
                        index=index,
                        size=size
                    )
            yield record
    except:
        progress.bar_style = 'danger'
        raise
    else:
        progress.bar_style = 'success'
        progress.value = index
        label.value = "{name}: {index}".format(
            name=name,
            index=str(index or '?')
        )


def main():
    parser = ArgumentParser(description="download information from the Red List API")
    parser.add_argument("info", help="The piece of information to download, must be one of {}".format(INFO_TYPES))
    parser.add_argument("-t", "--token", help="Access token for Red List API")
    parser.add_argument("-o", "--outfile", help="Name of file to save downloaded information")
    args = parser.parse_args()

    if not args.token:
        raise ValueError("No token provided for API, you must provide a token")

    download_info(args.info, args.token, save_file=args.outfile)


if __name__ == "__main__":
    main()





        


    