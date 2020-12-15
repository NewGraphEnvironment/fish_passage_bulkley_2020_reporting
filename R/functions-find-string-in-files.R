##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {

  fils <- list.files(path = where, pattern = in_files, recursive = recursive)

  found <- FALSE

  file_cmd <- Sys.which("file")

  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }

    contents <- readLines(fil)

    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)

    if (length(res) > 0) {

      found <-  TRUE

      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")

    }

  }

  if (!found) message("(No results found)")

}



