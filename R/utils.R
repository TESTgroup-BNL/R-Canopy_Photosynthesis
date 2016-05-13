#--------------------------------------------------------------------------------------------------#
# A set of helper functions
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' Read model.options.xml file
##' 
##' @name model.options
##' @title parse model.options.xml file used to set parameters and other options for model runs
##' @param input.file model.options.xml file containing information needed for run
##'
##' @examples
##' \dontrun{
##' opt <- model.options()
##' model.options <- model.options('/home/$USER/model.options.xml')
##' }
##'
##' @export
##'
##' @author Jin Wu, Shawn P. Serbin
##'
model.options <- function(input.file=NULL){
  model.options.xml <- NULL
  
  ### Parse input settings file
  if (!is.null(input.file) && file.exists(input.file)) {
    model.options.xml <- xmlParse(input.file)  
    # convert the xml to a list
    settings.list <- xmlToList(model.options.xml)
    
  } else {
    print("***** WARNING: no model.options.xml file defined *****")
  }
  
  # make sure something was loaded
  if (is.null(model.options.xml)) {
    stop("Did not find any model.options.xml file to load.")
  }
  
  ### Remove comment or NULL fields
  model.options.list <- model.options.list[model.options.list !="NULL" ]
  
  # Return settings file as a list
  #invisible(settings.list) # invisible option
  return(model.options.list)
  
} ### End of function
#--------------------------------------------------------------------------------------------------#