library(transformeR)


load('../MODIS_OLCI_ba_200101-202205.Rdata', verbose = TRUE)

dir.create('upscaling_ba')

ba_grid_05 <- upscaleGrid(grid = ba.merge, times = 2, aggr.fun = list(FUN = sum, na.rm = TRUE))
save(ba_grid_05, file = "upscaling_ba/ba_grid_05.Rdata")
rm(ba_grid_05)
print("ba upscaling at 0.5 completed")

ba_grid_1 <- upscaleGrid(grid = ba.merge, times = 4, aggr.fun = list(FUN = sum, na.rm = TRUE))
save(ba_grid_1, file = "upscaling_ba/ba_grid_1.Rdata")
rm(ba_grid_1)
print("ba upscaling at 1 completed")

ba_grid_2 <- upscaleGrid(grid = ba.merge, times = 8, aggr.fun = list(FUN = sum, na.rm = TRUE))
save(ba_grid_2, file = "upscaling_ba/ba_grid_2.Rdata")
rm(ba_grid_2)
print("ba upscaling at 2 completed")

ba_grid_3 <- upscaleGrid(grid = ba.merge, times = 12, aggr.fun = list(FUN = sum, na.rm = TRUE))
save(ba_grid_3, file = "upscaling_ba/ba_grid_3.Rdata")
rm(ba_grid_3)
print("ba upscaling at 3 completed")

ba_grid_4 <- upscaleGrid(grid = ba.merge, times = 16, aggr.fun = list(FUN = sum, na.rm = TRUE))
save(ba_grid_4, file = "upscaling_ba/ba_grid_4.Rdata")
rm(ba_grid_4)
rint("ba upscaling at 4 completed")

ba_grid_5 <- upscaleGrid(grid = ba.merge, times = 20, aggr.fun = list(FUN = sum, na.rm = TRUE))
save(ba_grid_5, file = "upscaling_ba/ba_grid_5.Rdata")
rm(ba_grid_5)
print("ba upscaling at 5 completed")