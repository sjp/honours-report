# This is hardly graphics-system-neutral
# e.g., it uses the grid notion of what constitutes a viewport
# But anyway ...

#################
# General Device Stuff
#################

setClass("graphicsDevice",
         representation(name="character",
                        width="numeric",
                        height="numeric"))

setClass("graphicsParams",
         representation(pars="list"))

setGeneric("inchToDevX",
           function(x, device) {
             standardGeneric("inchToDevX")
           })

setGeneric("inchToDevY",
           function(x, device) {
             standardGeneric("inchToDevY")
           })

setGeneric("devStartGroup",
           function(group, gp, device) {
             standardGeneric("devStartGroup")
           })

setGeneric("devEndGroup",
           function(device) {
             standardGeneric("devEndGroup")
           })

setGeneric("devLines",
           function(lines, gp, device) {
             standardGeneric("devLines")
           })

setGeneric("devPolygon",
           function(polygon, gp, device) {
             standardGeneric("devPolygon")
           })

setGeneric("devPath",
           function(path, gp, device) {
             standardGeneric("devPath")
           })

setGeneric("devRect",
           function(rect, gp, device) {
             standardGeneric("devRect")
           })

setGeneric("devText",
           function(text, gp, device) {
             standardGeneric("devText")
           })

setGeneric("devCircle",
           function(circle, gp, device) {
             standardGeneric("devCircle")
           })

setGeneric("devClose",
           function(device) {
             standardGeneric("devClose")
           })
           
