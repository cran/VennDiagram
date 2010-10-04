### FUNCTION TO DRAW SPECIAL CASE 010 #############################################################
draw.010 <- function(a1, a2, a3, a4, a5, a6, a7, category = rep("", 3), reverse = FALSE, cat.default.pos = "outer", lwd = rep(2, 3), lty = rep("solid", 3), col = rep("black", 3), label.col = rep("black", 7), cex = rep(1, 7), fontface = rep("plain", 7), fontfamily = rep("serif", 7), cat.pos = c(-40, 40, 180), cat.dist = c(0.05, 0.05, 0.025), cat.col = rep("black", 3), cat.cex = rep(1, 3), cat.fontface = rep("plain", 3), cat.fontfamily = rep("serif", 3), cat.just = list(c(0.5, 1), c(0.5, 1), c(0.5, 0)), cat.prompts = FALSE, fill = NULL, alpha = rep(0.5, 3), ...) {
	
	i <- 1;
	while (i <= 3) {
		tmp <- rotate.sp(c(a1, a2, a3, a4, a5, a6, a7), category, i, reverse);
		if (tmp[[1]][2] == 0) {i <- 4}
		else {i <- i + 1}
		}
	a1 <- tmp[[1]][1];
	a3 <- tmp[[1]][3];
	a4 <- tmp[[1]][4];
	a5 <- tmp[[1]][5];
	a6 <- tmp[[1]][6];
	a7 <- tmp[[1]][7];
	category <- tmp[[2]];
	
	x.centre.1 <- 0.35;
	y.centre.1 <- 0.5;
	r1 <- 0.25;
	x.centre.2 <- 0.65;
	y.centre.2 <- 0.5;
	r2 <- 0.25;
	x.centre.3 <- 0.5;
	y.centre.3 <- 0.55;
	r3 <- 0.25;
	
	a1.x.pos <- 0.18;
	a1.y.pos <- 0.5;
	a3.x.pos <- 0.82;
	a3.y.pos <- 0.5;
	a4.x.pos <- 0.32;
	a4.y.pos <- 0.54;
	a5.x.pos <- 0.5;
	a5.y.pos <- 0.51;
	a6.x.pos <- 0.68;
	a6.y.pos <- 0.54;
	a7.x.pos <- 0.5;
	a7.y.pos <- 0.75;
	
	return(
		draw.sp.case(
			area.list = c(a1, 0, a3, a4, a5, a6, a7),
			enabled.areas = c(1, 3, 4, 5, 6, 7),
			area.x = c(a1.x.pos, 0, a3.x.pos, a4.x.pos, a5.x.pos, a6.x.pos, a7.x.pos),
			area.y = c(a1.y.pos, 0, a3.y.pos, a4.y.pos, a5.y.pos, a6.y.pos, a7.y.pos),
			attach.label.to = c(1, 3, 5),
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
	)
}
