# flip a Venn diagram
flip.venn <- function(gList1, axis = 'v') {

	x.vect <- vector();
	y.vect <- vector();
	x.list <- list();
	y.list <- list();

	if ('v' == axis) {
		for (i in 1:length(gList1)) {
			x.vect <- c(x.vect, as.vector(gList1[i][[1]]$x, mode = 'numeric'));		
			x.list[[i]] <- as.vector(gList1[i][[1]]$x, mode = 'numeric');
			}
		for (i in 1:length(x.list)) { x.list[[i]] <- unit(1 - x.list[[i]], 'npc') }
		for (i in 1:length(gList1)) { gList1[i][[1]]$x <- x.list[[i]] }
			
		return(gList1);
		}

	else if ('h' == axis) {
		for (i in 1:length(gList1)) {
			y.vect <- c(y.vect, as.vector(gList1[i][[1]]$y, mode = 'numeric'));		
			y.list[[i]] <- as.vector(gList1[i][[1]]$y, mode = 'numeric');
			}
		for (i in 1:length(y.list)) { y.list[[i]] <- unit(1 - y.list[[i]], 'npc') }
		for (i in 1:length(gList1)) { gList1[i][[1]]$y <- y.list[[i]] }
			
		return(gList1);
		}

	else {
		stop('Unknown axis type');
		}

	}
