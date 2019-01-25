
# Defining Geographic Markets for Health Care Services

The objective of this repository is to lay out some thoughts, analytics,
and data for defining geographic markets for health care services in the
U.S.

Geographic market definitions are important for a wide variety of
regulatory and research uses. Therefore, for any given measure (e.g., a
Herfindahl-Hirschman index of market concentration) it is important to
know whether and how that measure varies by alternative market
definitions. Moreover, commonly used geographies, such as hospital
referral regions (HRRs), commuting zones (CZs), metropolitan statistical
areas (MSAs), etc. have both strengths and weaknesses. Whether the
strengths outweigh the weaknesses will depend on the specific question
at hand. As just one example, the latest commuting zone data are derived
from commuting patterns ascertained in the 2000 Census; HRRs and HSAs
are defined by patient hospital flows in 1992-1993. Obviously, flows of
both patients and commuters have changed substantially in some areas in
the last 20-30 years–whether these changes are material to defining
geographic boundaries of contemporary health care markets remains an
open question.

# Commonly Used Geographic Market Definitions

We begin by simply visualizing several commonly used geographic market
definitions. These include HRRs, HSAs, primary care service areas
(PCSAs), commuting zones, and health insurance rating areas as defined
for the Affordable Care Act’s non-group marketplaces.

These various market geographies are plotted for Tennessee in the figure
below. Note that the colors in each polygon are arbitrary and only
indended to further highlight boundaries across geographies.

What is clear here is that the size of the geographic market varies
greatly across these possible definitions. HRRs constitute the largest
service area and further, can stretch across state boundaries. HSAs are
much smaller – many in rural areas will contain only one hospital–so by
construction, many HSAs will be classified as “concentrated” if HSA is
used as the geographic market definition to construct an HHI measure.

Similarly, we see that PCSAs are quite small, while rating areas in
Tennessee are somewhere in-between the broader HRRs and commuting zones.

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- --> \# Geographic
Market Definitions

Before moving on it is useful to put down, in one place, the specific
methods and definitions used to construct each of the aforementioned
geographic market definitions.

## Hospital Service Areas

## Hosptial Referral Regions

## Primary Care Service Areas

## Marketplace Rating Areas

## Commuting Zones

# How Do HHI Measures Compare Across Geographic Market Defintions?

For this exercise we will construct measures of hopsital concentration
across the geographic market definitions covered above. Not all of these
will make sense (e.g., it seems odd to construct a Hospital HHI measure
based on PCSAs) but I will construct the measures anyway for the sake of
comparisons.

# Novel Market Definitions

In this section we will consider several methodological improvements and
extensions to the geographic definitions covered above. These include:

  - Updates to HSAs and HRR definitions based on more recent data.

  - Geographic market definitions based on community detection
    algorithms borrowed from social network analytic methods.
    
      - Based on hospital-ZIP data from Medicare.
    
      - Based on insurer-county data from DRG.
    
      - Based on shared patient networks from Medicare.
    
      - Based on updates to commuting flows from 2009-2013. Census data
        [here](https://www.census.gov/data/tables/time-series/demo/commuting/commuting-flows.html).
        Based on gravity model or based on Dartmouth method (e.g.,
        greatest fraction).
    
      - Based on hospital-specific HHIs.

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
