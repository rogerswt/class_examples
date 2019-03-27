#
# flowDensity_script.R
#
# per the vignette


library(flowCore)
library(flowDensity)
library(GEOmap)
data_dir <- system.file("extdata", package = "flowDensity")
load(list.files(pattern = 'sampleFCS_1', data_dir, full = TRUE))
f
par(mfrow = c(1,1), mar = c(5,4,4,1))
plotDens(f,c("FSC-A","SSC-A"))
plotDens(f, c(9, 8))  # CD3 vs CD19

# position:
#   TRUE  = +
#   FALSE = -
#   NA    = ignore this channel
# upper:
#   TRUE  = find change in slope after peak
#   NA    = ignore this channel
# debris.gate:
#
#
lymph <- flowDensity(obj=f, channels=c('FSC-A', 'SSC-A'),
                    position=c(TRUE, FALSE), upper= c(NA, TRUE))
#                     debris.gate=c(TRUE, FALSE))

# singlet.gate:
#   this is a built-in A vs H filter
sglt  <- flowDensity(obj=lymph, singlet.gate=TRUE)

# CD3   is channel 9
# SSC-A is channel 3
# The position argument says look for CD3- population
bcell <- flowDensity(obj=sglt, channels=c(9, 3),
                     position=c(FALSE, NA))

plot(getflowFrame(sglt), bcell)


