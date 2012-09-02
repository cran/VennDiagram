### FUNCTION TO DRAW CIRCLE (IN FACT A POLYGON) ###################################################
circle <- function(x, y, r, gp = NULL) {

	poly <- VennDiagram::ell2poly(x, y, r, r, 0, 3000);

	return(
		polygonGrob(
			x = poly$x,
			y = poly$y,
			gp = gp
			)
		);

	}
