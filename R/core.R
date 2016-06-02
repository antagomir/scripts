#' Core3D
#'
#' Description: Core visualization 3D
#'
#' Arguments:
#'  @param coreMat core matrix
#'  @param title title
#'  @param xlab X axis label
#'  @param cex.axis axis text size
#'
#' Returns:
#'  @return Used for its side effects
#'
#' @examples 
#'  # data(peerj32); 
#'  # c3d <- Core3D(core_matrix(t(peerj32$microbes)))
#' @export 
#' 
#' @references 
#'   Jalanka-Tuovinen et al. Intestinal Microbiota in Healthy Adults: Temporal 
#'   Analysis Reveals Individual and Common Core and Relation to Intestinal 
#'   Symptoms. PLoS ONE 6(7):e23035, 2012.
#'   To cite the microbiome R package, see citation('microbiome') 
#' @author Contact: Leo Lahti \email{microbiome-admin@@googlegroups.com}
#' @keywords utilities

Core3D <- function(coreMat, title = "Core microbiota", 
                   xlab = "Minimum Intensity", cex.axis = 0.7) {
    
    MinimumPrevalence <- as.numeric(colnames(coreMat))
    MinimumLogIntensity <- as.numeric(rownames(coreMat))
    tmp <- persp(MinimumLogIntensity, MinimumPrevalence, coreMat, 
                 theta = 60, phi = 5, 
        main = title, col = "light blue", axes = TRUE, ticktype = "detailed", 
        nticks = 9, 
        shade = 0.58, cex.axis = cex.axis, ylab = "Minimum Prevalence", 
        xlab = xlab, 
        zlab = "Core Size")
    
    return(NULL)
    
}


