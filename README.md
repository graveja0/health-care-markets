
# Defining Geographic Markets for Health Care Services

The objective of this repository and README is to lay out some thoughts,
analytics, and data for defining geographic markets for health care
services in the U.S.

# Mapping Geographic Markets

The maps below show various geographic market definitions that appear in
Tennessee.

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# File Descriptions

  - [R/manifest.R](R/manifest.R) sets up all libraries and settings.
  - [R/move-ak-hi.R](R/R/move-ak-hi.R) moves Alaska and Hawaii in a map.
  - [R/map-theme.R](R/map-theme.R) defines plotting and theme parameters
    for mapping.
  - [R/get-geographic-info.R](R/get-geographic-info.R) obtains centroid
    and contiguous geography data for any underlying shape object.
  - [R/zip-code-crosswalk.R](R/zip-code-crosswalk.R) creates a ZCTA to
    FIPS code crosswalk from the [MAPLE geographic crosswalking
    website](http://mcdc.missouri.edu/applications/geocorr2014.html).

## Geographic Mapping Files

  - The file
    [R/construct-county-map-data.R](R/construct-county-map-data.md)
    constructs ggplot-friendly mapping data for U.S. counties
    (`output/tidy-mapping-files/county/df_county.rds`). It also extracts
    contiguous counties and estimates county centroids
    (`output/tidy-mapping-files/county/df_county_info.rds`).

  - The file
    [R/construct-rating-area-data.R](R/construct-rating-area-data.md)
    constructs ggplot-friendly rating area map data based on the
    marketplace rating area definitions as of 2019-01-23 (these have
    barely changed, if any, over time, however). The sub-program
    [R/construct-rating-area-file-from-cciio-website.R](construct-rating-area-file-from-cciio-website.R)
    reads the HTML tables from the CCIIO website, which list either the
    counties or ZIP3 for each rating area. The program also separately
    creates 3-digit ZIP rating areas for Los Angeles county–the only
    county in CA that uses 3-digit ZIPs; however, the 3-digit ZIPs in LA
    County also span other counties, requiring some further manipulation
    to get the intersection.

  - The file
    [R/construct-dartmouth-geography-data.R](R/construct-dartmouth-geography-data.md)
    constructs ggplot-friendly mapping data for Dartmouth Atlas
    geographies including Hospital Referral Region (HRR), Hospital
    Service Region (HSA) and Primary Care Service Region (PCSA). It also
    constructs a data file listed at the geographic market level, and
    which contains data on centroids and contiguous markets. Note that
    the basis for these files is the ZIP to HRR/HSA and PCSA crosswalk
    files available for download at the [NBER
    website](https://www.nber.org/data/dartmouth-atlas-geography.html)
    and in [archived versions of the Dartmouth Atlas
    webpage](http://archive.dartmouthatlas.org/tools/downloads.aspx?tab=42).
    There are also shapefiles already constructed and [available for use
    on the archived Dartmouth
    website](http://archive.dartmouthatlas.org/tools/downloads.aspx?tab=39)–though
    I found these difficult to work with (e.g., I could not easily
    extract contiguous geographies using them, as I can by building up a
    HRR/HSA/PCSA shapefile from a ZCTA map)

  - The file
    [R/construct-commuting-zone-data.R](R/construct-commuting-zone-data.md)
    constructs ggplot-friendly mapping data for Commuting Zones defined
    using the 2000 census. The underying county-to-commuting zone data
    can be found on the [USDA
    website](https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/).

## Patient Sharing Files

  - The file
    [R/read-and-tidy-cms-hospital-service-areas.R](R/read-and-tidy-cms-hospital-service-areas.R)
    reads in the CMS Hospital Service Area file for 2017. Note the
    source of these data are downloaded csv files from the interactive
    CMS data explorer available at the links at [this
    link](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Hospital-Service-Area-File/index.html).
    The final file is rolled up to the FIPS county level and is storeed
    in
    `output/hospital-county-patient-data/2017/hospital-county-patient-data.rds`.
