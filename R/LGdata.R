filenames = c("atm" = "data/atm_chem.RData",
              "monthly" = "data/monthly_lake_level.RData",
              "exo" = "data/exo.RData",
              "ice" = "data/ice_cover.RData",)

#' @title Load Lake George Data
#'
#' @description
#' Loads data from locally downloaded RData files.
#'
#' @param data_name A string choosing the data to load.
#' \tabular{ll}{
#' \strong{Data name (data_name)} \tab \strong{Data Description} \cr
#' atm \tab ATM Chemistry \cr
#' monthly \tab Monthly Lake Level \cr
#' exo \tab exofull data \cr
#' ice \tab Ice Cover Levels \cr
#' }
#'
#'
#' @import utils
#'
#' @examples
#' \dontrun{
#'
#' #grab secchi data and plot it
#' ice.levels = LGdata('ice')
#' }
#' @export
LGdata = function(data_name){
  data_name = tolower(data_name)
  data_name = match.arg(data_name, names(filenames))
  path = local_path()

  return(load(file.path(local_path(), filenames[[data_name]])))
}
