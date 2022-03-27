# TODO: Add comment
# 
###############################################################################

a__f_createDir <- function(dirPath) {
	
	if (!dir.exists(dirPath)) dir.create(dirPath, recursive = TRUE)
	
	dirPath
}

