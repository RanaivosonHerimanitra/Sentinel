require(ggmap)
X=get_googlemap(center=c(47.051532 ,-19.0 ),
                #-19.503781
                size = c(640,640),
                maptype = "roadmap",
                format="png8",filename = "mada",zoom = 6)


#t <- tempfile()
saveRDS(X, file = "/media/herimanitra/DONNEES/IPM_sentinelle/sentinel_hrmntr 291115/Sentinel/madagascar.rds")
x <- readRDS("madagascar.rds")
ggmap(x)