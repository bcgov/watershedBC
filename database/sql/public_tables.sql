-- TABLE THAT ARE NOT CREATED BECAUSE NO REFERENCE IN THE APP:
--  public.bc_seasonal_regimes_alex_analysis_jan18_21clust
--  public.bc_seasonal_regimes_alex_analysis_jan18_21clust_wsc_parde_lon
--  public.bc_seasonal_regimes_alex_analysis_jan18_21clust_wsc_parde_lon_s
--  public.spatial_ref_sys
--  public.test
--  public.vri
--  public.watershed_statistics
--  public.wsc_drainagebasin_4326
--  public.wsc_monthly_with_attri_dams_20240204

-- BASINSV4
CREATE TABLE IF NOT EXISTS public.basinsv4
(
    id integer,
    basin integer,
    area_m2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS basinsv4_geom_idx
    ON public.basinsv4 USING gist
    (geom)
    TABLESPACE pg_default;

-- BC_BOUND
CREATE TABLE IF NOT EXISTS public.bc_bound
(
    island text COLLATE pg_catalog."default",
    geom geometry
);

--BC_BOUND_BUFFER_100
CREATE TABLE IF NOT EXISTS public.bc_bound_buffer_100
(
    geometry geometry
);

--BC_CEF_2021
CREATE TABLE IF NOT EXISTS public.bc_cef_2021
(
    objectid double precision,
    cef_disturb_group text COLLATE pg_catalog."default",
    cef_disturb_group_rank integer,
    cef_disturb_sub_group text COLLATE pg_catalog."default",
    cef_disturb_sub_group_rank integer,
    source_short_name text COLLATE pg_catalog."default",
    cef_extraction_date text COLLATE pg_catalog."default",
    cef_human_disturb_flag text COLLATE pg_catalog."default",
    area_km2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS bc_cef_2021_geom_gist
    ON public.bc_cef_2021 USING gist
    (geom)
    TABLESPACE pg_default;

--CUTBLOCKS
CREATE TABLE IF NOT EXISTS public.cutblocks
(
    harvest_year integer,
    area_m2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS cutblocks_geom_idx
    ON public.cutblocks USING gist
    (geom)
    TABLESPACE pg_default;

-- DAILY_WSC_CLEAN_20240205
CREATE TABLE IF NOT EXISTS public.daily_wsc_clean_20240205
(
    "row.names" text COLLATE pg_catalog."default",
    station_number text COLLATE pg_catalog."default",
    date text COLLATE pg_catalog."default",
    dayof_year double precision,
    mean double precision,
    median double precision,
    minimum double precision,
    maximum double precision,
    p5 double precision,
    p25 double precision,
    p75 double precision,
    p95 double precision,
    mad double precision,
    mean_mad_perc double precision,
    median_mad_perc double precision,
    p5_mad_perc double precision,
    p95_mad_perc double precision
);

-- DAMS
CREATE TABLE IF NOT EXISTS public.dams
(
    total_licence_storage double precision,
    geom geometry
);

-- DRA
CREATE TABLE IF NOT EXISTS public.dra
(
    speed_limit integer,
    total_number_of_lanes integer,
    transport_line_type_code_desc text COLLATE pg_catalog."default",
    transport_line_surface_code_desc text COLLATE pg_catalog."default",
    transport_line_divided_code_desc text COLLATE pg_catalog."default",
    geom geometry
);

CREATE INDEX IF NOT EXISTS dra_geom_idx
    ON public.dra USING gist
    (geom)
    TABLESPACE pg_default;

-- FIRE
CREATE TABLE IF NOT EXISTS public.fire
(
    fire_year integer,
    fire_number text COLLATE pg_catalog."default",
    area_m2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS fire_geom_idx
    ON public.fire USING gist
    (geom)
    TABLESPACE pg_default;

-- FWA_GLACIERS
CREATE TABLE IF NOT EXISTS public.fwa_glaciers
(
    waterbody_type text COLLATE pg_catalog."default",
    area_m2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS fwa_glaciers_geom_idx
    ON public.fwa_glaciers USING gist
    (geom)
    TABLESPACE pg_default;

-- FWA_LAKES
CREATE TABLE IF NOT EXISTS public.fwa_lakes
(
    waterbody_type text COLLATE pg_catalog."default",
    area_m2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS fwa_lakes_geom_idx
    ON public.fwa_lakes USING gist
    (geom)
    TABLESPACE pg_default;

-- FWA_NAMED
CREATE TABLE IF NOT EXISTS public.fwa_named
(
    gnis_name text COLLATE pg_catalog."default",
    gnis_id integer,
    stream_order integer,
    stream_magnitude integer,
    area_m2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS fwa_named_geom_idx
    ON public.fwa_named USING gist
    (geom)
    TABLESPACE pg_default;

-- FWA_NAMED_NAMES
CREATE TABLE IF NOT EXISTS public.fwa_named_names
(
    name text COLLATE pg_catalog."default"
);

-- FWA_ROLLUP
CREATE TABLE IF NOT EXISTS public.fwa_rollup
(
    "iFWA" text COLLATE pg_catalog."default",
    "WATERSHED_ORDER" text COLLATE pg_catalog."default",
    area_m2 double precision,
    geom geometry,
    id integer
);

CREATE INDEX IF NOT EXISTS fwa_rollup_geom_idx
    ON public.fwa_rollup USING gist
    (geom)
    TABLESPACE pg_default;

-- FWA_WETLANDS
CREATE TABLE IF NOT EXISTS public.fwa_wetlands
(
    waterbody_type text COLLATE pg_catalog."default",
    area_m2 double precision,
    geom geometry
);

CREATE INDEX IF NOT EXISTS fwa_wetlands_geom_idx
    ON public.fwa_wetlands USING gist
    (geom)
    TABLESPACE pg_default;

-- USAGE
CREATE TABLE IF NOT EXISTS public.usage
(
    "row.names" text COLLATE pg_catalog."default",
    date_time text COLLATE pg_catalog."default",
    session_token text COLLATE pg_catalog."default",
    gnis_name text COLLATE pg_catalog."default",
    gnis_id text COLLATE pg_catalog."default",
    processing_time text COLLATE pg_catalog."default",
    action text COLLATE pg_catalog."default",
    area_km2 double precision,
    basin_source character varying COLLATE pg_catalog."default"
);

-- WSC_DRAINAGEBASIN
CREATE TABLE IF NOT EXISTS public.wsc_drainagebasin
(
    stationnum text COLLATE pg_catalog."default",
    "NameNom" text COLLATE pg_catalog."default",
    "Status" text COLLATE pg_catalog."default",
    area_km2 double precision,
    "Version" text COLLATE pg_catalog."default",
    "Date" date,
    geom geometry
);

-- WSC_DRAINAGEBASIN_CLEAN_3005
CREATE TABLE IF NOT EXISTS public.wsc_drainagebasin_clean_3005
(
    stationnum text COLLATE pg_catalog."default",
    version text COLLATE pg_catalog."default",
    name text COLLATE pg_catalog."default",
    status text COLLATE pg_catalog."default",
    etat text COLLATE pg_catalog."default",
    area_km2 double precision,
    aire_km2 double precision,
    date date,
    type text COLLATE pg_catalog."default",
    area numeric,
    geometry geometry
);

-- WSC_MONTHLY_WITH_ATTRI_DAMS_NAT_20240204
CREATE TABLE IF NOT EXISTS public.wsc_monthly_with_attri_dams_nat_20240204
(
    "row.names" text COLLATE pg_catalog."default",
    station_number text COLLATE pg_catalog."default",
    area_km2 double precision,
    wetland_sum_perc double precision,
    lake_sum_perc double precision,
    glacier_sum_perc double precision,
    mat double precision,
    map double precision,
    dem_p025 double precision,
    dem_p050 double precision,
    dem_p075 double precision,
    station_cluster text COLLATE pg_catalog."default"
);

-- WSC_MONTHLY_WITH_ATTRI_DAMS_REG_20240204
CREATE TABLE IF NOT EXISTS public.wsc_monthly_with_attri_dams_reg_20240204
(
    "row.names" text COLLATE pg_catalog."default",
    station_number text COLLATE pg_catalog."default",
    area_km2 double precision,
    wetland_sum_perc double precision,
    lake_sum_perc double precision,
    glacier_sum_perc double precision,
    mat double precision,
    map double precision,
    dem_p025 double precision,
    dem_p050 double precision,
    dem_p075 double precision,
    station_cluster text COLLATE pg_catalog."default"
);

-- WSC_POURPOINT
CREATE TABLE IF NOT EXISTS public.wsc_pourpoint
(
    "StationNum" text COLLATE pg_catalog."default",
    "NameNom" text COLLATE pg_catalog."default",
    "Status" text COLLATE pg_catalog."default",
    "Etat" text COLLATE pg_catalog."default",
    "ProvTerr" text COLLATE pg_catalog."default",
    geometry geometry
);

-- WSC_PP_FASSTR_CALC_DAILY_STATS
CREATE TABLE IF NOT EXISTS public.wsc_pp_fasstr_calc_daily_stats
(
    station_number text COLLATE pg_catalog."default",
    date text COLLATE pg_catalog."default",
    yday double precision,
    mean double precision,
    median double precision,
    minimum double precision,
    maximum double precision,
    p5 double precision,
    p25 double precision,
    p75 double precision,
    p95 double precision
);

-- WSC_STATIONLOC
CREATE TABLE IF NOT EXISTS public.wsc_stationloc
(
    stationnum text COLLATE pg_catalog."default",
    name text COLLATE pg_catalog."default",
    status text COLLATE pg_catalog."default",
    "Etat" text COLLATE pg_catalog."default",
    "ProvTerr" text COLLATE pg_catalog."default",
    "HYDAT_ver" text COLLATE pg_catalog."default",
    "OBJECTID" double precision,
    geom geometry
);