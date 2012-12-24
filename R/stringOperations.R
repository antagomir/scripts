rename.duplicates <- function (s) {

  # If there are duplicate names in the list, add unique identifiers

  # List duplicates
  dups <- unique(s[duplicated(s)])
 
  s.uniq <- s
  for (i in 1:length(s.uniq)) {
    si <- s.uniq[[i]]
    if (si %in% dups) {
      s.uniq[[i]] <- paste(si, i, sep = "-rep")
    }
  }

  if (!length(unique(s.uniq)) == length(s)) {stop("rename.duplicates does not return unique entries yet")}

  s.uniq

}


strstrip <- function (mystr) {

  # (C) Leo Lahti 2008-2011
  # FreeBSD license (keep this notice)

  # Strip string i.e. remove spaces from the beginning and end
  while (substr(mystr,1,1)==" ") {
    mystr <- substr(mystr,2,nchar(mystr))
  }
  while (substr(mystr,nchar(mystr),nchar(mystr))==" ") {
    mystr <- substr(mystr,1,nchar(mystr)-1)
  }
  mystr
}
