# This script was written by Alaina Pearce in 2020
# to generate energy density databases to calculate
# caloric intake for the keller lab
#
#     Copyright (C) 2021 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.


#### Food and Brain Study ####

fbs_ED <- data.frame(meal = c(rep('std_meal', 12), rep('EAH', 10), rep('ps_meal', 6)),
                     food = c("applesauce", "carrot", "cheese_sndwch", "cookies", "ham_sndwch", "milk", "pbj_sndwch", "potatochip", "turkey_sndwch", "ketchup", "mayo", "mustard", "brownies", "cornchips", "hersheys", "icecream", "oreos", "popcorn", "pretzels", "skittles", "starbursts", "water", "chkn_nug", "mac_cheese", "grapes", "broccoli", "ketchup", "water"),
                     ED = c(0.445, 0.417, 2.755, 4.845, 2.208, 0.541, 3.451, 5.713, 2.078, 1.111, 7.258, 0, 4.364, 5.707, 5, 1.875, 4.712, 5.667, 3.923, 4, 4, 0, 2.500, 1.7, 0.695, 1.003, 1.167, 0))

#write out to raw-data
write.csv(fbs_ED, "data-raw/FBS_foodED_ref.csv", row.names = FALSE)

#make a database for the package
usethis::use_data(fbs_ED, overwrite = TRUE)

#### can add below for future studies ....
