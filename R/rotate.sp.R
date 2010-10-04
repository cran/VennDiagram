### ROTATION WITHOUT NEED OF OTHER ARGUMENTS ######################################################
rotate.sp <- function(area.vector, category.vector, rotation, reverse) {
	rot.1.f.7 <- 1:7;
	rot.1.f.3 <- 1:3;
	rot.1.r.7 <- c(3,2,1,6,5,4,7);
	rot.1.r.3 <- c(2,1,3);
	rot.2.f.7 <- c(3,6,7,2,5,4,1);
	rot.2.f.3 <- c(2,3,1);
	rot.2.r.7 <- c(7,6,3,4,5,2,1);
	rot.2.r.3 <- c(3,2,1);
	rot.3.f.7 <- c(7,4,1,6,5,2,3);
	rot.3.f.3 <- c(3,1,2);
	rot.3.r.7 <- c(1,4,7,2,5,6,3);
	rot.3.r.3 <- c(1,3,2);
	
	if (rotation == 1 & reverse == FALSE) {
		order.7 <- rot.1.f.7;
		order.3 <- rot.1.f.3
		}
	if (rotation == 1 & reverse == TRUE) {
		order.7 <- rot.1.r.7;
		order.3 <- rot.1.r.3
		}
	if (rotation == 2 & reverse == FALSE) {
		order.7 <- rot.2.f.7;
		order.3 <- rot.2.f.3
		}
	if (rotation == 2 & reverse == TRUE) {
		order.7 <- rot.2.r.7;
		order.3 <- rot.2.r.3
		}
	if (rotation == 3 & reverse == FALSE) {
		order.7 <- rot.3.f.7;
		order.3 <- rot.3.f.3
		}
	if (rotation == 3 & reverse == TRUE) {
		order.7 <- rot.3.r.7;
		order.3 <- rot.3.r.3
		}
	
	return(
		list(
				a = area.vector[order.7],
				c = category.vector[order.3]
			)
		);
	
}
