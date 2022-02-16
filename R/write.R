#' @param con binary connection
#' @param typestr Type string such as {'discr': '<f4', 'fortran_order': False, 'shape': (100,200)}
write.header <- function(con,typestr) {
    # magic : length 6
    writeBin(as.raw(c(0x93,0x4e,0x55,0x4d,0x50,0x59)),con)
    # major and minor version : length 2
    writeBin(as.raw(c(1,0)),con)
    # calculate header length
    total_header_len <- ceiling((stringr::str_length(typestr)+10)/64)*64
    header_len <- total_header_len-10
    writeBin(as.integer(header_len),con,size=2,endian="little")
    pad_len <- header_len-stringr::str_length(typestr)-1
    padding <- paste(rep(" ",pad_len),collapse="")
    ts <- paste(typestr,padding,"\n",sep="")
    writeBin(ts,con)
    # rewind a trailing zero
    seek(con,where=-1,origin="current")
}

#' @param class_str class string such as "numeric", "integer" etc.
#' @param size size of each element
#' @param shape a vector of the dimension of the data
#' @param signed is the type signed?
#' @param fortran_order is the output in fortran order?
get_typestr <- function(class_str, size, shape, signed=TRUE, fortran_order = TRUE) {
    endian <- ifelse(.Platform$endian == "little", "<", ">")
    types <- switch(class_str,
        "numeric" = "f",
        "integer" = ifelse(signed,"i","u"),
        "logical" = "b",
        "complex" = "c",
        "raw" = "B",
        "character" = "U",
        stop(paste("Type",class_str,"is not implemented"))
    )
    if (size == 1) {
        endian <- "|"
    }
    size <- as.character(size)
    if (length(shape) == 1) {
      shapestr <- paste(shape,",",sep="")
    } else {
      shapestr <- paste(shape,collapse=",")
    }
    sprintf("{'descr': '%s', 'fortran_order': %s, 'shape': (%s),}",
            paste(endian,types,size,sep=""),
            ifelse(fortran_order,"True","False"),
            shapestr
    )
}

natural_size <- function(obj) {
    classname <- typeof(obj)
    if (classname == "character") {
       return(max(sapply(obj,nchar)))
    }
    switch(classname,
        "numeric" = 8,
        "integer" = 4,
        "logical" = 1,
        "complex" = 16,
        stop("Unsupported object class:",classname)
    )  
}

#'
write.npy.vector <- function(obj,con,size) {
    if (is.na(size)) {
        size <- natural_size(obj)
    } 
    write.header(con,get_typestr(typeof(obj),size,length(obj)))
    if (is.character(obj)) {
       for (i in 1:length(obj)) {
         for (j in 1:size) {
             if (j <= stringr::str_length(obj[i])) {
               chr <- charToRaw(substr(obj[i],j,j))
               chr <- c(rep(0,4-length(chr)),chr)
               writeBin(as.raw(rev(chr)),con)
             } else {
               writeBin(0L,con,size=4)
             }
         }
       }
    } else {
       writeBin(obj,con,size=size)
    }
}

write.npy.matrix <- function(obj,con,size) {
    if (is.na(size)) {
        size <- natural_size(obj)
    } 
    write.header(con,get_typestr(typeof(obj),size,dim(obj)))
    if (is.character(obj)) {
       obj <- c(obj)
       for (i in 1:length(obj)) {
         for (j in 1:size) {
             if (j <= stringr::str_length(obj[i])) {
               chr <- charToRaw(substr(obj[i],j,j))
               chr <- c(rep(0,4-length(chr)),chr)
               writeBin(as.raw(rev(chr)),con)
             } else {
               writeBin(0L,con,size=4)
             }
         }
       }
    } else {
       writeBin(c(obj),con,size=size)
    }
}

write.npy.array <- function(obj,con,size) {
    stop("Not implemented yet. Sorry!")
}
