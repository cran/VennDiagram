### FUNCTION TO ROTATE DIAGRAM BY DEGREES #########################################################
rotate.venn.degrees <- function(gList1, angle = 90, x.centre = 0.5, y.centre = 0.5) {
	x.vect <- vector();
	y.vect <- vector();
	x.list <- list();
	y.list <- list();
	x2.list <- list();
	y2.list <- list();
	
	for (i in 1:length(gList1)) {
		x.vect <- c(x.vect, as.vector(gList1[i][[1]]$x, mode = "numeric"));		
		x.list[[i]] <- as.vector(gList1[i][[1]]$x, mode = "numeric");
		}
	for (i in 1:length(gList1)) {
		y.vect <- c(y.vect, as.vector(gList1[i][[1]]$y, mode = "numeric"));		
		y.list[[i]] <- as.vector(gList1[i][[1]]$y, mode = "numeric");
		}
	for (i in 1:length(x.list)) { 
		
		x2.list[[i]] <- (x.list[[i]] - x.centre) * cos(angle * pi / 180) - (y.list[[i]] - y.centre) * sin(angle * pi / 180) + x.centre;
		y2.list[[i]] <- (x.list[[i]] - x.centre) * sin(angle * pi / 180) + (y.list[[i]] - y.centre) * cos(angle * pi / 180) + y.centre;
	}
	for (i in 1:length(gList1)) { 
		gList1[i][[1]]$y <- unit(y2.list[[i]], "npc");
		gList1[i][[1]]$x <- unit(x2.list[[i]], "npc")
	}
	return(gList1)
	
	}	
	
