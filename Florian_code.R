#
# example code to go over in class

# Florian's homework
# example files are in ...WORKSHOP/student_correspondence/FilesFlorian

Comp <- function(ff) {
  # Nested "if" statements are used to find the first non-NULL matrix:
  if(!is.null(keyword(ff)$'SPILL')) {
    spilloverMatrix <- keyword(ff)$'SPILL'
    print("Spillover matrix: 'SPILL'")
  } else {
    if (!is.null(keyword(ff)$'spillover')) {
      spilloverMatrix <- keyword(ff)$'spillover'
      print("Spillover matrix: 'spillover'")
    } else {
      if (!is.null(keyword(ff)$'$SPILLOVER')) {
        spilloverMatrix <- keyword(ff)$'$SPILLOVER'
        print("Spillover matrix: '$SPILLOVER'")
      }
    }
  }
  #
  ff <- compensate(ff, spillover = spilloverMatrix)
  message("Compensation completed.")
  return(ff)
  #  }
}

BiexTransform <- function( flowFrame){
  #    grep function is used to identify an index vector that points to the
  #    parameters *FSC*, *SSC*, or *Time*  (coded by Wade)
  idx1 <- grep(pattern = "FSC", x = colnames(flowFrame), fixed = TRUE)
  idx2 <- grep(pattern = "SSC", x = colnames(flowFrame), fixed = TRUE)
  idx3 <- grep(pattern = "Time", x = colnames(flowFrame), fixed = TRUE)
  # negative indices return all EXCEPT those indices
  flIdx  <- (1:length(colnames(flowFrame)))[-c(idx1, idx2, idx3)]    #FL indices
  #flIdx  <- match(flNames, colnames(ff))     #FL indices (another way)
  # read the names of the
  flNames <- colnames(flowFrame)[-c(idx1, idx2, idx3)]   #names of fl. channels

  #Biexponential transformation
  ff <- doTransform(f = flowFrame, cols=flIdx, method='biexp')
  message('Biexponential transformation completted.')
  return(ff)
}

CompTransf <- function(fileName, comp=TRUE, biexTransform=TRUE) {
  library(flowCore, flowViz)
  ##Read FCS file. Stop if the file does not have the extension FCS)
  if(toupper(strsplit(basename(fileName),
                      split="\\.")[[1]][-1]) != 'FCS') {
    stop (c(fileName, ' does not seem to be an FCS file.'))
  } else {
    ff <- read.FCS(fileName)
  }
  #
  # Call the Comp Function to compensate
  if (comp == TRUE) ff <- Comp(ff)
  #
  #Call BiexTransform function if arg biexTransform=TRUE
  if (biexTransform == TRUE) ff <- BiexTransform(ff)
}






