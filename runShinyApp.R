print ('Loading libraries')
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}
print ('before error')
script.dir <- dirname(thisFile())
setwd(script.dir)

FILE <- getwd ()
#FILE <- choose.dir()
#print (FILE)
DIRNAMES <- unlist(strsplit(FILE, "[\\\\]|[^[:print:]]",fixed=FALSE))
mywd <- paste(DIRNAMES,collapse = "/")
mywd <- paste0 (mywd, "/")

#set working directory of the path
setwd (mywd)
.libPaths(paste0(mywd, 'R-Portable/App/R-Portable/library'))
chrome.portable = file.path(getwd(),'GoogleChromePortable/App/Chrome-bin/chrome.exe')

launch.browser = function(appUrl, browser.path=chrome.portable) {
  message('Browser path: ', browser.path)
  shell(sprintf('"%s" --app=%s', browser.path, appUrl))
}

require(shiny)
shiny::runApp(paste0(mywd,'SShiny/SShiny/App.R'),port=7777)

#shiny::runApp(paste0(mywd,'SShiny/SShiny/App.R'), launch.browser=launch.browser)