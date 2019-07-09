

#' S4 Class "SimulationObject"
#'
#' Contains crop parameters, rates and states for the current time step in
#' the simulation.
#'
#' @importFrom methods new
#' @export SimulationObject
#'
SimulationObject <- setClass('SimulationObject', slots = list(
  parameters = 'list',
  rates = 'numeric',
  states = 'numeric'
  ))

#' S4 Method for generic "show()"
#'
#' Prints a summary of the simulation object when typed or when
#' "show(object_name)" is called
#' @export
#'
setMethod('show', 'SimulationObject',
          function(object){
            cat('WofostR Simulation Object:', '\n')
            cat('>>',length(object@parameters),
                ' crop parameters specified','\n')
            cat('>>',length(object@rates),
                '  rate variables saved','\n')
            cat('>>',length(object@states),
                '  state variables saved')
          }
          )
