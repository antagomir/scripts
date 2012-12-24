
replace.description.field <- function (version.name, field, entry) {

  # Replaces given field in the DESCRIPTION file
  
   # define DESCRIPTION file based on version.name
   f <- paste(version.name,"/DESCRIPTION",sep="")
  
    # Read DESCRIPTION file
   lins <- readLines(f)

   # search the line that contains the field
   field.line <- grep(field,lins)

   # Replace the line
   lins[[field.line]] <- paste(field,"",entry,sep="")

   # Write new Rd file contents
   writeLines(lins,file(f))
   
}

replace.rd.fields <- function(rdfile, fills)
{

  # Replace the given fields (in fills) in a man/*.Rd file

  # fills is a list that lists the replacement texts for the fields
  # fills <- list(author = "N.N", reference = "RandomJournal", ...)
  
  # Read the Rd file
  rdlines <- readLines(rdfile)

  # Go through specified fields
  for (field in names(fills)) {
    # If replacment is given for the field..
    if (!is.na(fills[[field]])) {
      # Replace the given field with replacement text
      rdlines <- replace.rd.field(rdlines, field, replacement = fills[[field]])
    }
  }

  # Write new Rd file contents
  writeLines(rdlines,file(rdfile))
}


replace.rd.field <- function(rdlines, field, replacement) {

  # Replace annotation field in a man/*.Rd file

  # Detect starting line for the field
  sind <- grep(paste(field,"\\{",sep=""),rdlines)

  # Proceed only if the field is present in the file
  if (length(sind)>0) {
    
    # start line
    sline <- rdlines[[sind]]
    
    # Detect last line of the field
    eind <- sind
    while (length(grep("}", rdlines[[eind]]))==0) {
      eind <- eind+1
    }
    
    # Set new field on the first line of old field
    rdlines[[sind]] <- paste("\\",field,"{",replacement,"}",sep="")

    # remove the rest of the old field
    if (eind>sind) {
      rdlines <- rdlines[-((sind+1):eind)]
    }
  } else {}
    
  rdlines

}



set.keywords <- function (rdfile, keywords) {
    # Read the Rd file
    rdlines <- readLines(rdfile)

    # detect keyword lines
    inds <- grep("keyword\\{",rdlines)

    # store file lines before and after keywords
    after.keywords <- rdlines[-seq(max(inds))]
    before.keywords <- rdlines[seq(min(inds)-1)]

    # write keyword lines
    keywordlines <- vector(mode="character")
    for (i in 1:length(keywords)) {
      keywordlines[[i]] <- paste("\\keyword{",keywords[[i]],"}",sep="")
    }

    # add the parts
    rdlines <- c(before.keywords, keywordlines, after.keywords)
    
    # Write new Rd file contents
    writeLines(rdlines,file(rdfile))

}



remove.help.lines <- function (rdfile, helplines) {

  # Remove specified (help) lines from the Rd files
  
  # Read the Rd file
  rdlines <- readLines(rdfile)

  remove.idx <- c()
  for (lin in helplines) {
    remove.idx <- c(remove.idx, grep(lin, rdlines))
  }

  if (length(remove.idx)>0) {
    rdlines <- rdlines[-remove.idx]

    # Write new Rd file contents
    writeLines(rdlines,file(rdfile))
  } else {}
}
   
