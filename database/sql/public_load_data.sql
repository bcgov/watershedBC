-- SQL to load data from ./database/data/ folder

COPY public.basinsv4
from '/tmp/data/basinsv4.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.bc_bound
from '/tmp/data/bc_bound.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.bc_bound_buffer_100
from '/tmp/data/bc_bound_buffer_100.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.bc_cef_2021
from '/tmp/data/bc_cef_2021.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.cutblocks
from '/tmp/data/cutblocks.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.daily_wsc_clean_20240205
from '/tmp/data/daily_wsc_clean_20240205.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.dams
from '/tmp/data/dams.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.dra
from '/tmp/data/dra.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.fire
from '/tmp/data/fire.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.fwa_glaciers
from '/tmp/data/fwa_glaciers.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.fwa_lakes
from '/tmp/data/fwa_lakes.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.fwa_named
from '/tmp/data/fwa_named.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.fwa_named_names
from '/tmp/data/fwa_named_names.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.fwa_rollup
from '/tmp/data/fwa_rollup.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.fwa_wetlands
from '/tmp/data/fwa_wetlands.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.usage
from '/tmp/data/usage.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.wsc_drainagebasin
from '/tmp/data/wsc_drainagebasin.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.wsc_drainagebasin_clean_3005
from '/tmp/data/wsc_drainagebasin_clean_3005.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.wsc_monthly_with_attri_dams_nat_20240204
from '/tmp/data/wsc_monthly_with_attri_dams_nat_20240204.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.wsc_monthly_with_attri_dams_reg_20240204
from '/tmp/data/wsc_monthly_with_attri_dams_reg_20240204.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.wsc_pourpoint
from '/tmp/data/wsc_pourpoint.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.wsc_pp_fasstr_calc_daily_stats
from '/tmp/data/wsc_pp_fasstr_calc_daily_stats.csv'
DELIMITER ',' CSV null 'NULL' HEADER;

COPY public.wsc_stationloc
from '/tmp/data/wsc_stationloc.csv'
DELIMITER ',' CSV null 'NULL' HEADER;