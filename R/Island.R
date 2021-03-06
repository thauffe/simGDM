#' @title Island properties through time
#'
#' @description Island properties from Whittaker et al. (2008).
#' Data captured from Supplement 1, Figure S3 of Borregaard et al. (2016).
#'
#' @docType data
#'
#' @format A data.frame with 5000 rows and 4 columns.
#' Rows represent 5 million years of island evolution and columns measures of
#' area, elevation and topographic complexity in arbitrary units bounded [0,1].
#'
#' @usage data(Island)
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' Whittakker, R. J., K. Triantis and R. J. Ladle (2008).
#' A general dynamic theory of oceanic island biogeography.
#' Journal of Biogeography, 35(6), 977–994.
#'
#' @examples
#' data(Island)
#' # Reproduce Figure S3 (Borregaard et al., 2016)
#' plot(Area ~ Age, type = "l", lty = 3, data = Island,
#'      ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "Island properties")
#' lines(Topography ~ Age, data = Island)
#' lines(Elevation ~ Age, data = Island, lty = 2)
"Island"
