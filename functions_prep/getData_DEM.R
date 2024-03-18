sf::gdal_utils(util = "translate",
               source = "F:/COP_GLO30_FOR_watershedBC.tif",
               destination = "F:/COP_GLO30_FOR_watershedBC_cog.tif",
               options =  c("-of", "COG",
                            "-co", "COMPRESS=LZW",
                            "-co", "BIGTIFF=YES"))
