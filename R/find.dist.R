### FUNCTION TO FIND DISTANCE BETWEEN CIRCLES #####################################################
find.dist <- function(area1, area2, cross.area, inverted = FALSE) {
	if (!inverted) {
		r1 <- sqrt(area1 / pi);
		r2 <- sqrt(area2 / pi);
		}
	if (inverted) {
		r2 <- sqrt(area1 / pi);
		r1 <- sqrt(area2 / pi);
		}
	# set up a sequence of distances corresponding to full intersection to 0 intersection with set resolution (step)
	d <- r1 + r2
	resolution <- 0.001;
	d.list <- seq(r1 - r2 + resolution, d, resolution);
	int.list <- sapply(d.list, find.intersect, r1, r2);
	match.list <- (int.list >= cross.area);
	index.true <- length(match.list[match.list]);
	index.false <- index.true + 1;
	return(mean(d.list[index.true], d.list[index.false]))
		
	}
	
