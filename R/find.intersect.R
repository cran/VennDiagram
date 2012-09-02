# find the intersection of two circles
find.intersect <- function(d, r1, r2) {

	beta  <- (r1^2 + d^2 - r2^2) / (2 * r1 * d);
	gamma <- (r2^2 + d^2 - r1^2) / (2 * r2 * d);
	
	area <- r1^2 * (acos(beta) - 0.5 * sin(2 * acos(beta))) + r2^2 * (acos(gamma) - 0.5 * sin(2 * acos(gamma)));
	return(area);

	}
