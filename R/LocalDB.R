#' Create a local database file.
#' 
#' WARNING: It is possible for the bankers problem (race conditions) to occur, 
#' therefore be careful using in environments where multiple threads may access 
#' the same underlying data files.
RDB <- function(name, dir=getwd(), stringsAsFactors=FALSE) {
	rdb <- list()
	##### Attributes ###########################################################
	rdb$name <- name
	rdb$strings.as.factors <- stringsAsFactors
	if(!is.null(dir)) {
		rdb$filename <- paste0(dir, '/', name, '.Rda')
		if(file.exists(rdb$filename)) { # Load already existing data file
			load(rdb$filename)
			rdb$df <- df
			rm(df)
		} else { # Initalize new datafile
			rdb$df <- data.frame(stringsAsFactors=rdb$strings.as.factors)
			df <- rdb$df
			save(df, file=rdb$filename)
			rm(df)
		}
		rdb$file.info <- file.info(rdb$filename)
	} else {
		rdb$filename <- NULL
		rdb$df <- data.frame(stringsAsFactors=rdb$strings.as.factors)
	}
	
	##### Methods ##############################################################
	#' Check to see if the data file on disk has been updated since the last
	#' loading of the data file.
	#' @return TRUE if the loaded file is current with what is on disk (i.e. another
	#'         thread has not updated the file).
	rdb$is.current <- function() {
		if(is.null(rdb$filename)) {
			return(TRUE)
		} else {
			fi <- file.info(rdb$filename)
			return(fi$mtime <= rdb$file.info$mtime)
		}
	}
	
	#' Check to see if object has most recent data.
	#' @return returns TRUE if the file was reloaded (i.e. there was a newer
	#'         version on disk)
	rdb$check <- function() {
		if(!rdb$is.current()) { # Reload the data
			warning('Data in memory is stale. Reloading data from disk.')
			load(rdb$filename)
			rdb$df <- df
			rdb$file.info <- file.info(rdb$filename)
			invisible(TRUE)
		} else {
			invisible(FALSE)
		}
	}
	
	#' Save the Rda file.
	rdb$save <- function() {
		if(!is.null(rdb$filename)) {
			if(!rdb$is.current()) {
				stop('Data data on disk is more current!')
			}
			df <- rdb$df
			save(df, file=rdb$filename)
			rm(df)
			rdb$file.info <- file.info(rdb$filename)
		} else {
			warning('No filename specified to save to!')
		}
	}
	
	#' Return a subset of the data.frame.
	rdb$select <- function(...) {
		rdb$check() # Will reload the data.frame if necessary
		params <- list(...)
		rows <- rep(TRUE, nrow(rdb$df))
		for(i in seq_len(length(params))) {
			if(!names(params)[i] %in% names(rdb$df)) {
				warning(paste0(names(params)[i], ' is not a valid column!'))
			} else {
				rows <- rows & rdb$df[,names(params)[i]] %in% params[[i]]
			}
		}
		if(length(rows) > 0) {
			return(rdb$df[rows,])
		} else {
			return(data.frame())
		}
	}
	
	#' Add a row to the data.frame
	rdb$insert <- function(...) {
		rdb$check()
		
		params <- list(...)
		
		if(length(params) == 0) { stop('No data to add!') }
		
		newrow <- data.frame(params, stringsAsFactors=rdb$strings.as.factors)
		if(nrow(rdb$df) == 0) { # First record
			rdb$df <- newrow
		} else {
			# The purpose of these two for loops is to ensure that the existing
			# data.frame and the new row match on their structure
			for(j in which(!names(params) %in% names(rdb$df))) {
				# This will add columns and set old values to NA
				rdb$df[,names(params)[j]] <- NA
			}
			for(j in which(!names(rdb$df) %in% names(params))) {
				# This will add values from df not in the new row, setting those to NA
				newrow[,names(rdb$df)[j]] <- NA
			}
			rdb$df <- rbind(rdb$df, newrow[,names(rdb$df)])
		}
		suppressWarnings(rdb$save())
		invisible(TRUE)
	}
	
	#' Delete a row from the data.frame.
	rdb$delete <- function(rows) {
		if(!rdb$is.current()) {
			stop('Data in memory is stale. The file on disk had been changed by another thread!')
		}
		rdb$df <- rdb$df[-rows,]
		suppressWarnings(rdb$save())
	}
	
	#' Update a row from the data.frame.
	rdb$update <- function(row, ...) {
		stop('Not currently supported!')
		
		params <- list(...)
		
		rdb$save()
	}
	
	rdb <- list2env(rdb)
	class(rdb) <- 'RDB'
	return(rdb)
}

#' Print information about \code{RDB} object.
#' @param x an \code{RDB} object.
#' @return nothing
#' @export
print.RDB <- function(x) {
	cat(paste0('Local database ', x$name, ' has ', nrow(x$df), ' observations of ',
			   ncol(x$df), ' variables.\n',
			   'File is located at: ', x$filename))
	invisible()
}

#' S3 method for str. Returns the structure of the underlying data.frame.
#' @param object an \code{RDB} object.
#' @param the structure of the underlying data.frame.
#' @export
str.RDB <- function(object, ...) {
	str(object$df)
}

if(FALSE) { # For testing
	testdb <- RDB(name='test', dir=tempdir())
	testdb$select(username='admin') # Will throw a warning
	testdb$insert(band='Rush', album='Counterparts')
	testdb$select()
	testdb$insert(band='Rush', album='2112')
	testdb$insert(band='Peter Gabriel', album='Up')
	testdb$select()
	testdb$delete(rows=3)
	testdb$select()
	testdb$insert(band='Rush', year=2112)
	testdb$select()
	testdb$insert(band='Sting', album='Mercury Falling')
	testdb$select()
	
	testdb$select(band='Rush')
	testdb$select(band='Rush', album='2112')
	
	# Test S3 methods
	testdb
	str(testdb)
	unlink(testdb$filename)
}
