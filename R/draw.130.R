### FUNCTION TO DRAW SPECIAL CASE 130 #############################################################
draw.130 <- function(a1, a2, a3, a4, a5, a6, a7, category = rep("", 3), reverse = FALSE, cat.default.pos = "outer", lwd = rep(2, 3), lty = rep("solid", 3), col = rep("black", 3), label.col = rep("black", 7), cex = rep(1, 7), fontface = rep("plain", 7), fontfamily = rep("serif", 7), cat.pos = c(-40, 40, 180), cat.dist = c(0.05, 0.05, 0.025), cat.col = rep("black", 3), cat.cex = rep(1, 3), cat.fontface = rep("plain", 3), cat.fontfamily = rep("serif", 3), cat.just = list(c(0.5, 1), c(0.5, 1), c(0.5, 0)), cat.prompts = FALSE, fill = NULL, alpha = rep(0.5, 3), scaled = TRUE, sep.dist = rep(0.05, 3), ...) {
	if (length(sep.dist) == 1) { sep.dist <- rep(sep.dist, 3) }
	i <- 1;
	while (i <= 3) {
		tmp <- VennDiagram::rotate.sp(c(a1, a2, a3, a4, a5, a6, a7), category, i, reverse);
		if (tmp[[1]][2] == 0 & tmp[[1]][4] == 0 & tmp[[1]][5] == 0 & tmp[[1]][6] == 0) {i <- 4}
		else {i <- i + 1}
		}
	a1 <- tmp[[1]][1];
	a3 <- tmp[[1]][3];
	a7 <- tmp[[1]][7];
	category <- tmp[[2]];
	
	if (scaled) {
		r1 <- sqrt(a1 / pi);
		r2 <- sqrt(a3 / pi);
		r3 <- sqrt(a7 / pi);
		shrink.factor <- 0.2 / max(r1, r2, r3);
		r1 <- r1 * shrink.factor;
		r2 <- r2 * shrink.factor;
		r3 <- r3 * shrink.factor;
		}
		
	if (!scaled) {
		r1 <- 0.18;
		r2 <- 0.18;
		r3 <- 0.18;
		}
	
	a <- (r1 + r2) * (1 + sep.dist[1]);
	b <- (r2 + r3) * (1 + sep.dist[2]);
	c <- (r1 + r3) * (1 + sep.dist[3]);
	
	beta <- (a^2 + c^2 - b^2) / (2 * a * c);
	gamma <- sqrt(1 - beta^2);
	x.centre.1 <- (r1 - r2 - a + 1) / 2;
	x.centre.3 <- x.centre.1 + c * beta;
	y.centre.3 <- (r3 - r1 + 1 - c * gamma) / 2;
	y.centre.1 <- y.centre.3 + c * gamma;
	x.centre.2 <- x.centre.1 + a;
	y.centre.2 <- y.centre.1;
	
	a1.x.pos <- x.centre.1;
	a1.y.pos <- y.centre.1;
	a3.x.pos <- x.centre.2;
	a3.y.pos <- y.centre.2;
	a7.x.pos <- x.centre.3;
	a7.y.pos <- y.centre.3;
	
	return(
		VennDiagram::draw.sp.case(
			area.list = c(a1, 0, a3, 0, 0, 0, a7),
			enabled.areas = c(1, 3, 7),
			area.x = c(a1.x.pos, 0, a3.x.pos, 0, 0, 0, a7.x.pos),
			area.y = c(a1.y.pos, 0, a3.y.pos, 0, 0, 0, a7.y.pos),
			attach.label.to = c(1, 3, 7),
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
