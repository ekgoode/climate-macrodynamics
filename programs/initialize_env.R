################################################################################
# This program loads all libraries, packages, and custom functions needed to
# run the other programs for the project. This file should always be run before
# programs in the project.
#
#
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 8/12/2022
#
################################################################################

################################################################################
# Section 1 - Load library/packages
################################################################################

#General purpose library
library(tidyverse)
library(readxl)
library(here)
library(parallel)

#Make Latex tables
library(stargazer)

#General Graphics Packages
library(RColorBrewer)
library(viridis)
library(scales)

#Mapping packages
library(ggmap)
library(foreign)
library(maps)

################################################################################
# Section 2 - Load custom functions
################################################################################
source(here('programs', 'solowv2.R'))
source(here('programs', 'regSim.R'))
source(here('programs', 'bootSimv2.R'))
source(here('programs', 'custom_functions.R'))