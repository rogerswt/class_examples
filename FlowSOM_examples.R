#
# working with FlowSOM
#
# 20190315  WTR
#

# vignette

############################### All at once ####################################
library(FlowSOM)
fileName <- system.file("extdata","lymphocytes.fcs", package="FlowSOM")
ff = read.FCS(fileName)
fSOM <- FlowSOM(fileName,
                # Input options:
                compensate = TRUE,transform = TRUE,toTransform=c(8:18),
                scale = TRUE,
                # SOM options:
                colsToUse = c(9,12,14:18), xdim = 7, ydim = 7,
                # Metaclustering options:
                nClus = 10,
                # Seed for reproducible results:
                seed = 42)
PlotStars(fSOM$FlowSOM,
          backgroundValues = as.factor(fSOM$metaclustering))


############################### Step at a time #################################
library(flowCore)
library(FlowSOM)
fileName <- system.file("extdata","lymphocytes.fcs",
                         package="FlowSOM")
fSOM <- ReadInput(fileName,compensate = TRUE,transform = TRUE,
                     toTransform=c(8:18),scale = TRUE)
ff <- suppressWarnings(flowCore::read.FCS(fileName))

# read and preprocess the data ...
# "When reading the data, several preprocessing options are available"
fSOM <- ReadInput(ff,compensate = TRUE,transform = TRUE, scale = TRUE)

str(fSOM,max.level = 2)

# ... and then build the SOM
# "The next step in the algorithm is to build a self-organizing map."
fSOM <- BuildSOM(fSOM,colsToUse = c(9,12,14:18))
str(fSOM$map,max.level = 2)

# "The third step of FlowSOM is to build the minimal spanning tree."
fSOM <- BuildMST(fSOM,tSNE=TRUE)
str(fSOM$MST)

PlotStars(fSOM)
PlotStars(fSOM,view="grid")
PlotStars(fSOM,view="tSNE")

PlotMarker(fSOM,"Pacific Blue-A")


############################### Chapter 6 ######################################
# Look for CD8+ ab T cells
query <- c("PE-Cy7-A" = "high",         #CD3
             "APC-Cy7-A" = "high",      #TCRb
             "Pacific Blue-A" = "high") #CD8
query_res <- QueryStarPlot(UpdateNodeSize(fSOM,reset = TRUE), query,
                                           plot = FALSE)
cellTypes <- factor(rep("Unknown",49),levels = c("Unknown","CD8 T cells"))
cellTypes[query_res$selected] <- "CD8 T cells"
PlotStars(fSOM,
          backgroundValues = cellTypes,
          backgroundColor = c("#FFFFFF00","#0000FF22"))

# going off script, let's see if we can find the B cells
query <- c("PE-Cy5-A" = "high")         #CD19
query_res <- QueryStarPlot(UpdateNodeSize(fSOM,reset = TRUE), query,
                           plot = FALSE)
cellTypes <- factor(rep("Unknown",49),levels = c("Unknown","CD19 B cells"))
cellTypes[query_res$selected] <- "CD19 B cells"
PlotStars(fSOM,
          backgroundValues = cellTypes,
          backgroundColor = c("#FFFFFF00","#0000FF22"))


############################# Metaclustering ###################################
# if there's time...


########################## Custom Extensions ###################################
source("~/git/R/class_examples/FlowSOM_utils.R")

# re-read ff
ff <- suppressWarnings(flowCore::read.FCS(fileName))
fl_params = 8:18
# there's one that's NA
fl_names = parameters(ff)$desc[fl_params]
fl_names[4] = "nada"
colnames(ff)[fl_params] = fl_names
colnames(keyword(ff)$SPILL) = fl_names

useful_params = c(9,12,14:18)
fSOM <- ReadInput(ff,compensate = TRUE,transform = TRUE, scale = TRUE)
fSOM <- BuildSOM(fSOM,colsToUse = useful_params)
fSOM <- BuildMST(fSOM,tSNE = TRUE)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 1))
plot_marker_tree(fsom = fSOM,, marker = "CD4")

# now, because we're in control, we can make a useful spread
par(mfrow = c(3, 3), mar = c(1, 1, 4, 1))
params = colnames(ff)[useful_params]
for (p in params) {
  plot_marker_tree(fsom = fSOM, node.scale = 0.1, marker = p)
}



