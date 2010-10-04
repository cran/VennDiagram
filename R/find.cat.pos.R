### FUNCTION TO FIND POSITION OF CATEGORY NAMES ###################################################
find.cat.pos <- function(x, y, pos, dist, r = NULL) {
	if (!is.null(r)) {
		cat.x <- (r + dist) * sin(pos * pi / 180) + x;
		cat.y <- (r + dist) * cos(pos * pi / 180) + y;
		}
	
	if (is.null(r)) {
		cat.x <- dist * sin(pos * pi / 180) + x;
		cat.y <- dist * cos(pos * pi / 180) + y;
		}
		
		return(
			list(
				x = cat.x,
				y = cat.y
				)
			)
		
		}
