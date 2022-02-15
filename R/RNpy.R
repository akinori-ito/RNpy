#
# Npy file I/O for R
#

whatis <- function(typestr) {
  endian <- .Platform$endian
  pos <- 1
  c <- substr(typestr,1,1)
  if (c == "<") {
    endian <- "little"
    pos <- 2
  } else if (c == ">") {
    endian <- "big"
    pos <- 2
  }
  what <- "numeric"
  signed <- TRUE
  size <- -1
  c <- substr(typestr,pos,pos)
  if (c == "i") {
    what <- "integer"
  } else if (c == "u") {
    what <- "integer"
    signed <- FALSE
  } else if (c == "f") {
    what <- "numeric"
  } else if (c == "b") {
    what <- "integer"
    size <- 1
  } else if (c == "B") {
    what <- "integer"
    size <- 1
  } else if (c == "?") {
    what <- "logical"
  } else if (c == "c") {
    what <- "complex"
  } else if (c == "U") {
    what <- "character"
  } else {
    stop(paste("Format",c,"is not implemented"))
  }
  pos <- pos+1
  if (size < 0) {
    r <- regexpr("[0-9]+",substr(typestr,pos,stringr::str_length(typestr)))
    if (r < 0) {
      stop(paste("Illegal type format:",typestr))
    }
    size <- as.integer(substr(typestr,pos,pos+attributes(r)$match.length-1))
  }
  list(what=what,size=size,signed=signed,endian=endian)
}

#' Read numpy binary file
#' @param filename filename of the npy file
#' @return vector, matrix of arra
#' @export
read.npy <- function(filename) {
  con <- file(filename,"rb")
  magic <- readBin(con,"raw",n=6,size=1)
  if (!all(magic == c(0x93,0x4e,0x55,0x4d,0x50,0x59))) {
    close(con)
    stop("Invalid file format: magic does not match")
  }
  major_version <- readBin(con,"raw",n=1,size=1)
  minor_version <- readBin(con,"raw",n=1,size=1)
  header_len <- readBin(con,"integer",n=1,size=2,signed=FALSE,endian="little") 
  header <- rawToChar(readBin(con,"raw",n=header_len,size=1))
  fmt <- parsePy(header)
  whattype <- whatis(fmt$descr)
  shape <- unlist(fmt$shape)
  if (length(shape) == 1) {
    size <- shape
  } else {
    size <- prod(shape)
  }
  empty <- 0
  if (whattype$what == "character") {
    empty <- ""
    ret <- rep("",size)
    for (i in 1:size) {
      cs <- rep("",whattype$size)
      for (k in 1:whattype$size) {
        x <- readBin(con,"integer",size=1,n=4)
        cs[k] <- intToUtf8(x)
      }
      ret[i] <- paste(cs,collapse="")
    }
  } else {
    ret <- readBin(con,whattype$what,
                 size=whattype$size,
                 n=size,
                 signed=whattype$signed,
                 endian=whattype$endian)
  }
  if (length(shape) == 2) {
    ret <- matrix(ret,nrow=shape[1],ncol=shape[2],byrow=!fmt$fortran_order)
  } else if (length(shape) == 3) {
    res <- array(empty,dim=shape)
    if (fmt$fortran_order) {
      n <- 1
      for (k in 1:shape[3]) {
        for (j in 1:shape[2]) {
          for (i in 1:shape[1]) {
            res[i,j,k] <- ret[n]
            n <- n+1
          }
        }
      }
    } else {
      n <- 1
      for (i in 1:shape[1]) {
        for (j in 1:shape[2]) {
          for (k in 1:shape[3]) {
            res[i,j,k] <- ret[n]
            n <- n+1
          }
        }
      }
    }
    ret <- res
  } else {
    stop("Reading more than 3-dimensional array is not implemented. sorry!")
  }
  close(con)
  ret
}