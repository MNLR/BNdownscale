#     climatologyVariogram.R Empirical variogram of a climatology
#
#     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Empirical variogram of a climatology
#' @description Calculate (and draw) the empirical semivariogram of a climatological grid
#' @param clim A climatological grid (see \code[transformeR]{climatology})
#' @param n.classes The number of distance classes used to compute the variogram
#' @param do.log Logical. Should the variable be log-transformed prior to variogram calculation? Default to \code{FALSE}.
#' @return Plots a semivariogram and invisibly returns the data used to construct it (semivariance, distance class and number
#'  of pairs of points per distance class).
#' @details The x-axis of the variogram is tyically truncated at half the maximum distance of the dataset.
#' 
#' Note that the empirical semi-variogram is used under the assumtion of normality. Thus, the 
#' \code{do.log} option may need to be activated for non-gaussian fields.
#' 
#' @author J Bedia
#' @export
#' @importFrom sp coordinates spDists
#' @importFrom transformeR clim2sgdf
#' @examples
#' require(transformeR)
#' data("EOBS_Iberia_tp")
#' # We compute the mean annual DJF precipitation
#' aggr.fun <- list(FUN = "sum")
#' annual.tp <- aggregateGrid(EOBS_Iberia_tp, aggr.m = aggr.fun, aggr.y = aggr.fun)
#' # Now the winter precipitation climatology is computed
#' tp.clim <- climatology(annual.tp)
#' # The empirical variogram:
#' climatologyVariogram(clim = tp.clim, n.classes = 20, do.log = TRUE)

climatologyVariogram <- function(clim, n.classes = 20, do.log = FALSE) {
    spatdf <- clim2sgdf(clim, set.max = NULL, set.min = NULL)
    # Tomamos la matriz de coordenadas
    coords <- coordinates(spatdf)
    # Matriz de distancias entre todos los pares de puntos
    dist.matrix <- spDists(coords)
    # Eliminamos pares de puntos duplicados
    # (la matriz es simetrica, nos quedamos con la parte superior de la diagonal)
    dist.matrix[lower.tri(dist.matrix, diag = TRUE)] <- NA
    dist.range <- range(dist.matrix, na.rm = TRUE)
    # Dividimos en intervalos de distancia 
    dist.classes <- seq(dist.range[1], ceiling(dist.range[2]), length.out = n.classes)
    y <- spatdf[[1]]
    if (do.log) y <- log1p(y)
    semivar <- rep(NA, length(dist.classes) - 1)
    n.pares <- semivar
    for (i in 2:length(dist.classes)) {
        ind.class <- which(dist.matrix <= dist.classes[i] & dist.matrix > dist.classes[i - 1], arr.ind = TRUE)
        N <- nrow(ind.class)
        if (N == 0) {
            semivar[i] <- NA
            n.pares[i] <- 0
        } else {
            err <- rep(NA, N)
            for (j in 1:N) {
                pt <- ind.class[j,]
                err[j] <- (y[pt[1]] - y[pt[2]])^2
            }
            semivar[i] <- sum(err, na.rm = TRUE)/(2*N)
            n.pares[i] <- N
        }
    }
    plot(dist.classes, semivar, ty = "b", cex = 0,
         xlim = c(0, tail(dist.classes,1)/2),
         ylim = c(0, max(semivar, na.rm = TRUE)),
         ylab = "semivariance",
         xlab = "distance classes",
         main = "Empirical climatology semivariogram")
    text(dist.classes, semivar, n.pares, cex = .8)
    abline(h = var(x = y, na.rm = TRUE, use = "complete.obs"), col = "red")
    invisible(cbind.data.frame("dist.class" = dist.classes, "n.pairs" =  n.pares, "semivar" = semivar))
}
