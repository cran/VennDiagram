### FUNCTION TO FIT VENN DIAGRAM TO SIZE ##########################################################
adjust.venn <- function(gList1, margin = 0.01, ...) {

	x.vect <- vector();
	y.vect <- vector();

	x.list <- list();
	y.list <- list();

	for (i in 1:length(gList1)) {

		x.vect <- c(x.vect, as.vector(gList1[i][[1]]$x, mode = 'numeric'));
		y.vect <- c(y.vect, as.vector(gList1[i][[1]]$y, mode = 'numeric'));

		x.list[[i]] <- as.vector(gList1[i][[1]]$x, mode = 'numeric');
		y.list[[i]] <- as.vector(gList1[i][[1]]$y, mode = 'numeric');

		}
		
	max.x <- max(x.vect) + margin;
	min.x <- min(x.vect) - margin;
	max.y <- max(y.vect) + margin;
	min.y <- min(y.vect) - margin;
	x.centre <- (max.x + min.x) / 2;
	y.centre <- (max.y + min.y) / 2;

	size <- 0.99;

	if (max.x - min.x >= max.y - min.y) {
		for (i in 1:length(x.list)) {
			x.list[[i]] <- unit((x.list[[i]] - x.centre) * (size / (max.x - min.x)) + 0.5, 'npc');
			y.list[[i]] <- unit((y.list[[i]] - y.centre) * (size / (max.x - min.x)) + 0.5, 'npc');
			}
		}

	if (max.x - min.x < max.y - min.y) {
		for (i in 1:length(x.list)) {
			x.list[[i]] <- unit((x.list[[i]] - x.centre) * (size / (max.y - min.y)) + 0.5, 'npc');
			y.list[[i]] <- unit((y.list[[i]] - y.centre) * (size / (max.y - min.y)) + 0.5, 'npc');
			}
		}

	for (i in 1:length(gList1)) {
		gList1[i][[1]]$x <- x.list[[i]];
		gList1[i][[1]]$y <- y.list[[i]];
		}

	return(gList1);

	}
