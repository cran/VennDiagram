### FUNCTION TO DRAW ELLIPSE (IN FACT A POLYGON) ###################################################
ellipse <- function(x, y, a, b, rotation = 0, gp = NULL) {

	poly <- VennDiagram::ell2poly(x, y, a, b, rotation, 3000);

	return(
		polygonGrob(
			x = poly$x,
			y = poly$y,
			gp = gp
			)
		);
		
	}
