dim (dat_svy_dau_wave_02)
names(dat_svy_dau_wave_02)
table(dat_svy_dau_wave_02$integration_point_id)

save(dat_svy_dau_wave_02,file = paste(paste(getwd(),"/backup/",sep=""),"dat_svy_dau_wave_02", ".csv", sep = ""))

install.packages("rio")
install_formats()
library(rio)
export(dat_svy_dau_wave_02,"C:\Users\nirlevy\Documents\projects\murders\backup\dat_svy_dau_wave_02.csv")
export(dat_svy_dau_wave_02,"C:/Users/nirlevy/Documents/projects/murders/backup/dat_svy_dau_wave_02.csv")

rm(dat_svy_dau_wave_02)
