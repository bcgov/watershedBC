# watershedBC

## Overview

The watershedBC is an [*experimental*]{.underline} interactive R Shiny app that allows users to explore and summarize watershed attributes in British Columbia. The app utilizes data from the BC Data Catalog to provide valuable on-demand reports about various characteristics of watersheds across the province.

## Get started

Stable - <https://bcgov-env.shinyapps.io/watershedBC/>\
Development - <https://bcgov-env.shinyapps.io/watershedBC_dev/>

## How the app works

### Step 1: Watershed Delineation

There are four datasets to chose from to select watersheds, they are:

-   [**Freshwater Atlas Named Watersheds**](https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-named-watersheds): These watersheds are used directly from the Freshwater Atlas of BC with no additional processing.
-   [**Freshwater Atlas Watersheds**](https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-watersheds) **by Stream Order**: These fundamental watershed units have been 'rolled-up' by stream order such that every stream segment now has a polygon that represents the entire watershed.
-   [**Water Survey of Canada Hydrometric Basins**](https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/): These are watersheds published by the federal government that represent the watersheds above actual Water Survey of Canada Hydrometric Station locations.
-   **Custom Watersheds at Point of Interest**: This is an unpublished dataset that uses the the BC TRIM DEM and extracts a watershed at every location 1 km along every waterway in the province.

### Step 2: Current Modules

There are currently six four datasets to chose from to select watersheds, they are:

1.  **Streamflow and Freshwater Resources**

    -   Freshwater Atlas\
        Summarize wetlands, lakes and glaciers from the FWA

    -   Glacier Change\
        Summarize glacier change between 1985 and 2021

    -   Discharge Estimate\
        Estimate the long term mean daily discharge at for any watershed

2.  **Forest Disturbance**

    -   Cutblocks\
        Summarize the total area of cutblocks by harvest year

    -   Wildfire\
        Summarize the total area of wildfires by harvest year

    -   Roads\
        Summarize the total length of roads by type

    -   Simplified ECA\
        Provide a Simplified ECA, which is a 40 year recovery period for harvest and wildfire relative to the total basin area. [**This is NOT ECA!**]{.underline}

3.  **Stream Profile**

    -   Stream Profile**\
        Plot the stream profile of all of the named streams in your watershed

4.  **Water Allocations**

    -   Water Rights\
        Total allocated water rights

    -   Water Authorizations\
        Total allocated water authorizations

5.  **ClimateBC**

    -   Mean Annual Precipitation\
        Sample points across the watershed from ClimateBC MAP

    -   Mean Annual Air Temperature\
        Sample points across the watershed from ClimateBC MAT

    -   Cumulative Moisture Deficit\
        Sample points across the watershed from ClimateBC CMD

6.  **Satellite Imagery**

    -   Sentinel - 2023\
        Cloud free sentinel-2 mosaic of BC

    -   Landsat - 2020\
        Cloud free Landsat 8 mosaic of BC

    -   Landsat - 1985\
        Cloud free Landsat 5 mosaic of BC
