### FUNCTION TO OBTAIN POLYGON COORDINATES SIMILAR TO A CIRCLE ####################################
ell2poly <- function(x, y, a, b, rotation, n.sides) {
	rotation <- rotation * pi / 180;
	# calculate the angle corresponding to each "section" of the polygon
	# (there are as many sections as there are sides in the polygon)
	theta <- 2 * pi / n.sides;
	angles <- seq(0, 2 * pi, theta);
	
	# initialize vectors to hold the x and y coordinates of each vertex of the polygon
	x.coord <- vector(length = n.sides + 1, mode = "numeric");
	x.coord[1] <- x + a * cos(rotation);
	y.coord <- vector(length = n.sides + 1, mode = "numeric");
	y.coord[1] <- y + a * sin(rotation);
	
	# starting from the initial point, sequentially obtain the coordinates of each vertex of the polygon and store them
	for (i in 1:n.sides) {
		
		x.coord[i + 1] <- x + a * cos(angles[i + 1]) * cos(rotation) - b * sin(angles[i + 1]) * sin(rotation);
		y.coord[i + 1] <- y + a * cos(angles[i + 1]) * sin(rotation) + b * sin(angles[i + 1]) * cos(rotation);
		
		}
	return(
		list(
			x = x.coord,
			y = y.coord
			)
		)
		
	}
