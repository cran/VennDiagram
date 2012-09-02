### FUNCTION TO DRAW SPECIAL CASE 012A #############################################################
draw.012AA <- function(a1, a2, a3, a4, a5, a6, a7, category = rep("", 3), reverse = FALSE, cat.default.pos = "outer", lwd = rep(2, 3), lty = rep("solid", 3), col = rep("black", 3), label.col = rep("black", 7), cex = rep(1, 7), fontface = rep("plain", 7), fontfamily = rep("serif", 7), cat.pos = c(-40, 40, 180), cat.dist = c(0.05, 0.05, 0.025), cat.col = rep("black", 3), cat.cex = rep(1, 3), cat.fontface = rep("plain", 3), cat.fontfamily = rep("serif", 3), cat.just = list(c(0.5, 1), c(0.5, 1), c(0.5, 0)), cat.prompts = FALSE, fill = NULL, alpha = rep(0.5, 3), ...) {

	for (i in 1:3) {
		tmp <- VennDiagram::rotate.sp(c(a1, a2, a3, a4, a5, a6, a7), category, i, reverse);
		if (0 == tmp[[1]][1] & 0 == tmp[[1]][4] & 0 == tmp[[1]][7] ) { break; }
		}

	a2 <- tmp[[1]][2];
	a3 <- tmp[[1]][3];
	a5 <- tmp[[1]][5];
	a6 <- tmp[[1]][6];
	category <- tmp[[2]];
	
	x.centre.1 <- 0.4;
	y.centre.1 <- 0.5;
	r1 <- 0.2;
	x.centre.2 <- 0.5;
	y.centre.2 <- 0.5;
	r2 <- 0.35;
	x.centre.3 <- 0.6;
	y.centre.3 <- 0.5;
	r3 <- 0.2;
	
	a2.x.pos <- 0.3;
	a2.y.pos <- 0.5;
	a3.x.pos <- 0.5;
	a3.y.pos <- 0.775;
	a5.x.pos <- 0.5;
	a5.y.pos <- 0.5;
	a6.x.pos <- 0.7;
	a6.y.pos <- 0.5;
	
	return(
		VennDiagram::draw.sp.case(
			area.list = c(0, a2, a3, 0, a5, a6, 0),
			enabled.areas = c(2, 3, 5, 6),
			area.x = c(0, a2.x.pos, a3.x.pos, 0, a5.x.pos, a6.x.pos, 0),
			area.y = c(0, a2.y.pos, a3.y.pos, 0, a5.y.pos, a6.y.pos, 0),
			attach.label.to = c(2, 3, 6),
			x.centres = c(x.centre.1, x.centre.2, x.centre.3),
			y.centres = c(y.centre.1, y.centre.2, y.centre.3),
			a.list = c(r1, r2, r3),
			b.list = c(r1, r2, r3),
			straight.reverse = FALSE,
			category = category,
			cat.default.pos = cat.default.pos, 
			lwd = lwd, 
			lty = lty, 
			col = col, 
			label.col = label.col, 
			cex = cex, 
			fontface = fontface, 
			fontfamily = fontfamily, 
			cat.pos = cat.pos, 
			cat.dist = cat.dist, 
			cat.col = cat.col, 
			cat.cex = cat.cex, 
			cat.fontface = cat.fontface, 
			cat.fontfamily = cat.fontfamily, 
			cat.just = cat.just,
			fill = fill, 
			alpha = alpha,
			...
			)
		);
	}
