

#' S4 Class "SimulationObject"
#'
#' Contains crop parameters, rates and states for the current time step in
#' the simulation.
#' @importFrom methods new
#' @export SimulationObject
#'
SimulationObject <- setClass('SimulationObject', slots = list(
  crop.parameters = 'list',
  rates = 'numeric',
  states = 'numeric'
  ))
