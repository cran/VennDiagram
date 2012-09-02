### FUNCTION TO DRAW SPECIAL CASE 032 #############################################################
draw.032 <- function(a1, a2, a3, a4, a5, a6, a7, category = rep("", 3), reverse = FALSE, cat.default.pos = "outer", lwd = rep(2, 3), lty = rep("solid", 3), col = rep("black", 3), label.col = rep("black", 7), cex = rep(1, 7), fontface = rep("plain", 7), fontfamily = rep("serif", 7), cat.pos = c(-40, 40, 180), cat.dist = c(0.05, 0.05, 0.025), cat.col = rep("black", 3), cat.cex = rep(1, 3), cat.fontface = rep("plain", 3), cat.fontfamily = rep("serif", 3), cat.just = list(c(0.5, 1), c(0.5, 1), c(0.5, 0)), cat.prompts = FALSE, fill = NULL, alpha = rep(0.5, 3), scaled = TRUE, offset = 0, ...) {

	for (i in 1:3) {
		tmp <- VennDiagram::rotate.sp(c(a1, a2, a3, a4, a5, a6, a7), category, i, reverse);
		if (0 == tmp[[1]][2] & 0 == tmp[[1]][3] & 0 == tmp[[1]][4] & 0 == tmp[[1]][6] & 0 == tmp[[1]][7]) { break; }
		}

	a1 <- tmp[[1]][1];
	a5 <- tmp[[1]][5];
	category <- tmp[[2]];
	
	if (scaled) {
		r1 <- sqrt((a1 + a5) / pi);
		r2 <- sqrt(a5 / pi);
		r3 <- sqrt(a5 / pi);
		shrink.factor <- 0.2 / max(r1, r2, r3);
		r1 <- r1 * shrink.factor;
		r2 <- r2 * shrink.factor;
		r3 <- r3 * shrink.factor;
		}
	else {
		r1 <- 0.4;
		r2 <- 0.2;
		r3 <- 0.2;
		}
		
	x.centre.1 <- 0.5;
	y.centre.1 <- 0.5;
	x.centre.2 <- 0.5 - offset * (r1 - r2);
	y.centre.2 <- 0.5;
	x.centre.3 <- 0.5 - offset * (r1 - r3);
	y.centre.3 <- 0.5;
	
	a1.x.pos <- (x.centre.1 + x.centre.2 + r1 + r2) / 2;
	a1.y.pos <- 0.5;
	a5.x.pos <- x.centre.2;
	a5.y.pos <- 0.5;
	
	return(
		VennDiagram::draw.sp.case(
			area.list = c(a1, 0, 0, 0, a5, 0, 0),
			enabled.areas = c(1, 5),
			area.x = c(a1.x.pos, 0, 0, 0, a5.x.pos, 0, 0),
			area.y = c(a1.y.pos, 0, 0, 0, a5.y.pos, 0, 0),
			attach.label.to = c(1, 5, 5),
			x.centres = c(x.centre.1, x.centre.2, x.centre.3),
			y.centres = c(y.centre.1, y.centre.2, y.centre.3),
			a.list = c(r1, r2, r3),
			b.list = c(r1, r2, r3),
			straight.reverse = FALSE,
			reverse = reverse,
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
