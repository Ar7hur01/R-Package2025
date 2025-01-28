library(ourPackage)

convert_shp_to_txt("C:/Users/AD/Desktop/R_Package/Enkelboom/Enkelboom punte.shp","C:/Users/AD/Desktop/R_Package", "Label")

combine_raster_vector("C:/Users/AD/Desktop/R_Package/Stacked_S2_Enkelboom_2024_12_29.tif","C:/Users/AD/Desktop/R_Package/coords_label.txt","C:/Users/AD/Desktop/R_Package")

stack_shp_on_rast("C:/Users/AD/Desktop/R_Package/Enkelboom/Enkelboom punte.shp","C:/Users/AD/Desktop/R_Package/08-01-25_enkelboom_stacked.tif","C:/Users/AD/Desktop/R_Package")
a <- vect("C:/Users/AD/Desktop/R_Package/Enkelboom/Enkelboom punte.shp")
