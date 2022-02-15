parsePy <- function(str) {
  if (is.null(str)) {
    return(NULL)
  }
  str <- stringr::str_trim(str,side="both")
  len <- stringr::str_length(str)
  if (len == 0) {
    return(NULL)
  }
  x <- parsePyItem(str,1)
  return(x$value)
}

parsePyItem <- function(str,pos) {
  first <- substr(str,pos,pos)
  while (first %in% c(" ","\t","\n")) {
    pos <- pos+1
    first <- substr(str,pos,pos)
  }
  if (first == '{') {
    return(parsePyDict(str,pos+1))
  }
  if (first == '(') {
    return(parsePySeq(str,pos+1,')'))
  }
  if (first == '[') {
    return(parsePySeq(str,pos+1,']'))
  }
  if (first == '"' || first == "'") {
    return(parsePyStr(str,pos))
  }
  if (regexpr("[0-9]",first) > 0) {
    return(parsePyNumber(str,pos))
  }
  ret <- regexpr("^[[:alpha:]]+",substr(str,pos,stringr::str_length(str)))
  if (ret > 0) {
    idlen <- attributes(ret)$match.length
    id <- substr(str,pos,pos+idlen-1)
    if (id == "True") {
      return(list(value=TRUE,pos=pos+4))
    }
    if (id == "False") {
      return(list(value=FALSE,pos=pos+5))
    }
  }
  stop(paste("Invalid format:",str,"position=",pos))
}

parsePyDict <- function(str,pos) {
  res <- list()
  while (TRUE) {
    while (substr(str,pos,pos) %in% c(" ","\t","\n")) {
      pos <- pos+1
    }
    if (pos > stringr::str_length(str)) {
      stop("Unterminated dict")
    }
    x <- parsePyItem(str,pos)
    key <- x$value
    pos <- x$pos
    while (substr(str,pos,pos) %in% c(" ","\t","\n")) {
      pos <- pos+1
    }
    if (pos > stringr::str_length(str)) {
      stop("Unterminated dict")
    }
    if (substr(str,pos,pos) != ":") {
      stop("Illegal dict format")
    }
    pos <- pos+1
    x <- parsePyItem(str,pos)
    pos <- x$pos
    res[[key]] <- x$value
    while (substr(str,pos,pos) %in% c(" ","\t","\n")) {
      pos <- pos+1
    }
    if (pos > stringr::str_length(str)) {
      stop("Unterminated dict")
    }
    c <- substr(str,pos,pos)
    if (c == ",") {
      pos <- pos+1
      while (substr(str,pos,pos) %in% c(" ","\t","\n")) {
        pos <- pos+1
      }
      if (substr(str,pos,pos) == "}") {
        return(list(value=res,pos=pos+1))
      }
      next
    } else if (c == "}") {
      return(list(value=res,pos=pos+1))
    } else {
      stop("Illegal dict format")
    }
    
  }
}

parsePyNumber <- function(str,pos) {
  r <- regexpr("^-?(0x)?[0-9]+(\\.[0-9_]+)?([egEG]-?[0-9]+)?",substr(str,pos,stringr::str_length(str)))
  if (r == -1) {
    stop(paste("Illegal number format:",substr(str,pos,stringr::str_length(str))))
  }
  endpos <- pos+attributes(r)$match.length
  list(value=as.numeric(substr(str,pos,endpos-1)),pos=endpos)
}

parsePyStr <- function(str,pos) {
  quote <- substr(str,pos,pos)
  pos <- pos+1
  s <- rep("",stringr::str_length(str)-pos)
  k <- 1
  while (TRUE) {
    if (pos > stringr::str_length(str)) {
      stop("Unterminated string")
    }
    c <- substr(str,pos,pos)
    if (c == quote) break
    if (c == "\\") {
      s[k] <- c
      s[k+1] <- substr(str,pos+1,pos+1)
      k <- k+2
      pos <- pos+2
      next
    }
    s[k] <- c
    k <- k+1
    pos <- pos+1
  }
  list(value=paste(s,collapse=""),pos=pos+1) 
}

parsePySeq <- function(str,pos,termchar) {
  res <- list()
  i <- 1
  while (TRUE) {
    if (pos > stringr::str_length(str)) {
      stop("Unterminated list")
    }
    if (substr(str,pos,pos) %in% c(" ","\t","\n")) {
      pos <- pos+1
      next
    }
    x <- parsePyItem(str,pos)
    if (pos > stringr::str_length(str)) {
      stop("Unterminated list")
    }
    res[[i]] <- x$value
    pos <- x$pos
    c <- substr(str,pos,pos)
    if (c == ",") {
      pos <- pos+1
      while (substr(str,pos,pos) %in% c(" ","\t","\n")) {
        pos <- pos+1
      }
      if (substr(str,pos,pos) == termchar) {
        pos <- pos+1
        break
      }
      i <- i+1
      next
    }
    if (c == termchar) { 
      pos <- pos+1
      break
    }
  }
  list(value=res,pos=pos)
}