# This script was written by Alaina Pearce in 2020
# to generate data that can be referenced to score
# the BRIEF-2
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

brief2_boys5_7 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "boys5-7")
brief2_girls5_7 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "girls5-7")

brief2_boys8_10 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "boys8-10")
brief2_girls8_10 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "girls8-10")

brief2_boys11_13 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "boys11-13")
brief2_girls11_13 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "girls11-13")

brief2_boys14_18 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "boys14-18")
brief2_girls14_18 <- readxl::read_excel("data-raw/brief2_scoringtables.xlsx", sheet = "girls14-18")

# make data list

brief2_scoretables <- list(boys5_7 = brief2_boys5_7,
                        girls5_7 = brief2_girls5_7,
                        boys8_10 = brief2_boys8_10,
                        girls8_10 = brief2_girls8_10,
                        boys11_13 = brief2_boys11_13,
                        girls11_13 = brief2_girls11_13,
                        boys14_18 = brief2_boys14_18,
                        girls14_18 = brief2_girls14_18)


#make a database for the package
usethis::use_data(brief2_scoretables, overwrite = TRUE)

#### can add below for future studies ....
