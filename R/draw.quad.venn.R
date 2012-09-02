### FUNCTION TO DRAW VENN DIAGRAM WITH FOUR SETS #################################################
draw.quad.venn <- function(area1, area2, area3, area4, n12, n13, n14, n23, n24, n34, n123, n124, n134, n234, n1234, category = rep("", 4), lwd = rep(2, 4), lty = rep("solid", 4), col = rep("black", 4), fill = NULL, alpha = rep(0.5, 4), label.col = rep("black", 15), cex = rep(1, 15), fontface = rep("plain", 15), fontfamily = rep("serif", 15), cat.pos = c(-15, 15, 0, 0), cat.dist = c(0.22, 0.22, 0.11, 0.11), cat.col = rep("black", 4), cat.cex = rep(1, 4), cat.fontface = rep("plain", 4), cat.fontfamily = rep("serif", 4), cat.just = rep(list(c(0.5, 0.5)), 4), rotation.degree = 0, rotation.centre = c(0.5, 0.5), ind = TRUE, ...) {

	#area1 > area2 > area3 > area4
	# check parameter lengths
	if (length(category) == 1) {cat <- rep(category, 4)}
	if (length(category) != 1 & length(category) != 4) { stop("Unexpected parameter length for 'category'") }
	if (length(lwd) == 1) {lwd <- rep(lwd, 4)}
	if (length(lwd) != 1 & length(lwd) != 4) { stop("Unexpected parameter length for 'lwd'") }
	if (length(lty) == 1) {lty <- rep(lty, 4)}
	if (length(lty) != 1 & length(lty) != 4) { stop("Unexpected parameter length for 'lty'") }
	if (length(col) == 1) {col <- rep(col, 4)}
	if (length(col) != 1 & length(col) != 4) { stop("Unexpected parameter length for 'col'") }
	if (length(label.col) == 1) {label.col <- rep(label.col, 15)}
	if (length(label.col) != 1 & length(label.col) != 15) { stop("Unexpected parameter length for 'label.col'") }
	if (length(cex) == 1) {cex <- rep(cex, 15)}
	if (length(cex) != 1 & length(cex) != 15) { stop("Unexpected parameter length for 'cex'") }
	if (length(fontface) == 1) {fontface <- rep(fontface, 15)}
	if (length(fontface) != 1 & length(fontface) != 15) { stop("Unexpected parameter length for 'fontface'") }
	if (length(fontfamily) == 1) {fontfamily <- rep(fontfamily, 15)}
	if (length(fontfamily) != 1 & length(fontfamily) != 15) { stop("Unexpected parameter length for 'fontfamily'") }
	if (length(fill) == 1) {fill <- rep(fill, 4)}
	if (length(fill) != 1 & length(fill) != 4 & length(fill) != 0) { stop("Unexpected parameter length for 'fill'") }
	if (length(alpha) == 1) {alpha <- rep(alpha, 4)}
	if (length(alpha) != 1 & length(alpha) != 4 & length(alpha) != 0) { stop("Unexpected parameter length for 'alpha'") }
	if (length(cat.pos) == 1) {cat.pos <- rep(cat.pos, 4)}
	if (length(cat.pos) != 1 & length(cat.pos) != 4) { stop("Unexpected parameter length for 'cat.pos'") }
	if (length(cat.dist) == 1) {cat.dist <- rep(cat.dist, 4)}
	if (length(cat.dist) != 1 & length(cat.dist) != 4) { stop("Unexpected parameter length for 'cat.dist'") }
	if (length(cat.col) == 1) {cat.col <- rep(cat.col, 4)}
	if (length(cat.col) != 1 & length(cat.col) != 4) { stop("Unexpected parameter length for 'cat.col'") }
	if (length(cat.cex) == 1) {cat.cex <- rep(cat.cex, 4)}
	if (length(cat.cex) != 1 & length(cat.cex) != 4) { stop("Unexpected parameter length for 'cat.cex'") }
	if (length(cat.fontface) == 1) {cat.fontface <- rep(cat.fontface, 4)}
	if (length(cat.fontface) != 1 & length(cat.fontface) != 4) { stop("Unexpected parameter length for 'cat.fontface'") }
	if (length(cat.fontfamily) == 1) {cat.fontfamily <- rep(cat.fontfamily, 4)}
	if (length(cat.fontfamily) != 1 & length(cat.fontfamily) != 4) { stop("Unexpected parameter length for 'cat.fontfamily'") }
	if (!(class(cat.just) == "list" & length(cat.just) == 4 & length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 2 & length(cat.just[[3]]) == 2 & length(cat.just[[4]]) == 2)) { stop("Unexpected parameter format for 'cat.just'") }
	cat.pos <- cat.pos + rotation.degree;
	
	# generate partial areas from given arguments
	a6  <- n1234;
	a12 <- n123 - a6;
	a11 <- n124 - a6;
	a5  <- n134 - a6;
	a7  <- n234 - a6;
	a15 <- n12 - a6 - a11 - a12;
	a4  <- n13 - a6 - a5 - a12;
	a10 <- n14 - a6 - a5 - a11;
	a13 <- n23 - a6 - a7 - a12;
	a8  <- n24 - a6 - a7 - a11;
	a2  <- n34 - a6 - a5 - a7;
	a9  <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15;
	a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15;
	a1  <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13;
	a3  <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11;

	# check plausibility and 0 partial areas
	if (any(a1 < 0, a2 < 0, a3 < 0, a4 < 0, a5 < 0, a6 < 0, a7 < 0, a8 < 0, a9 < 0, a10 < 0, a11 < 0, a12 < 0, a13 < 0, a14 < 0, a15 < 0)) { stop("Impossible: partial areas negative") }

	# initialize gList to hold all Grobs generated
	grob.list <- gList();
	
	# plot the ellipses of the Venn diagram
	tmp <- VennDiagram::ellipse(
		x = 0.65, 
		y = 0.47, 
		a = 0.35,
		b = 0.2,
		rotation = 45,
		gp = gpar(
			lty = 0,
			fill = fill[2],
			alpha = alpha[2]
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- VennDiagram::ellipse(
		x = 0.35, 
		y = 0.47, 
		a = 0.35,
		b = 0.2,
		rotation = 135,
		gp = gpar(
			lty = 0,
			fill = fill[1],
			alpha = alpha[1]
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- VennDiagram::ellipse(
		x = 0.5, 
		y = 0.57, 
		a = 0.33,
		b = 0.15,
		rotation = 45,
		gp = gpar(
			lty = 0,
			fill = fill[4],
			alpha = alpha[4]
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- VennDiagram::ellipse(
		x = 0.5, 
		y = 0.57, 
		a = 0.33,
		b = 0.15,
		rotation = 135,
		gp = gpar(
			lty = 0,
			fill = fill[3],
			alpha = alpha[3]
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- ellipse(
		x = 0.65, 
		y = 0.47, 
		a = 0.35,
		b = 0.2,
		rotation = 45,
		gp = gpar(
			lwd = lwd[4],
			lty = lty[4],
			col = col[4],
			fill = 'transparent'
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- VennDiagram::ellipse(
		x = 0.35, 
		y = 0.47, 
		a = 0.35,
		b = 0.2,
		rotation = 135,
		gp = gpar(
			lwd = lwd[1],
			lty = lty[1],
			col = col[1],
			fill = 'transparent'
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- VennDiagram::ellipse(
		x = 0.5, 
		y = 0.57, 
		a = 0.33,
		b = 0.15,
		rotation = 45,
		gp = gpar(
				lwd = lwd[3],
				lty = lty[3],
				col = col[3],
				fill = 'transparent'
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- VennDiagram::ellipse(
		x = 0.5, 
		y = 0.57, 
		a = 0.33,
		b = 0.15,
		rotation = 135,
		gp = gpar(
				lwd = lwd[2],
				lty = lty[2],
				col = col[2],
				fill = 'transparent'
			)
		);
	grob.list <- gList(grob.list, tmp);

	label.matrix <- matrix(
		nrow = 15,
		ncol = 3
		);
	colnames(label.matrix) <- c('label', 'x', 'y');

	label.matrix[ 1,] <- c(a1,  0.350, 0.77);
	label.matrix[ 2,] <- c(a2,  0.500, 0.69);
	label.matrix[ 3,] <- c(a3,  0.650, 0.77);
	label.matrix[ 4,] <- c(a4,  0.310, 0.67);
	label.matrix[ 5,] <- c(a5,  0.400, 0.58);
	label.matrix[ 6,] <- c(a6,  0.500, 0.47);
	label.matrix[ 7,] <- c(a7,  0.600, 0.58);
	label.matrix[ 8,] <- c(a8,  0.690, 0.67);
	label.matrix[ 9,] <- c(a9,  0.180, 0.58);
	label.matrix[10,] <- c(a10, 0.320, 0.42);
	label.matrix[11,] <- c(a11, 0.425, 0.38);
	label.matrix[12,] <- c(a12, 0.575, 0.38);
	label.matrix[13,] <- c(a13, 0.680, 0.42);
	label.matrix[14,] <- c(a14, 0.820, 0.58);
	label.matrix[15,] <- c(a15, 0.500, 0.28);
	
	for (i in 1:nrow(label.matrix)) {
		tmp <- textGrob(
			label = label.matrix[i,'label'],
			x = label.matrix[i,'x'],
			y = label.matrix[i,'y'],
			gp = gpar(
				col = label.col[i],
				cex = cex[i],
				fontface = fontface[i],
				fontfamily = fontfamily[i]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}

	# find the location and plot all the category names
	cat.pos.1 <- find.cat.pos(0.18, 0.58, cat.pos[1], cat.dist[1])
	tmp <- textGrob(
		label = category[1],
		x = cat.pos.1$x,
		y = cat.pos.1$y,
		just = cat.just[[1]],
		gp = gpar(
			col = cat.col[1],
			cex = cat.cex[1],
			fontface = cat.fontface[1],
			fontfamily = cat.fontfamily[1]
			)
		);
	grob.list <- gList(grob.list, tmp);

	cat.pos.2 <- find.cat.pos(0.82, 0.58, cat.pos[2], cat.dist[2])
	tmp <- textGrob(
		label = category[2],
		x = cat.pos.2$x,
		y = cat.pos.2$y,
		just = cat.just[[2]],
		gp = gpar(
			col = cat.col[2],
			cex = cat.cex[2],
			fontface = cat.fontface[2],
			fontfamily = cat.fontfamily[2]
			)
		);
	grob.list <- gList(grob.list, tmp);

	cat.pos.3 <- find.cat.pos(0.35, 0.77, cat.pos[3], cat.dist[3])
	tmp <- textGrob(
		label = category[3],
		x = cat.pos.3$x,
		y = cat.pos.3$y,
		just = cat.just[[3]],
		gp = gpar(
			col = cat.col[3],
			cex = cat.cex[3],
			fontface = cat.fontface[3],
			fontfamily = cat.fontfamily[3]
			)
		);
	grob.list <- gList(grob.list, tmp);

	cat.pos.4 <- find.cat.pos(0.65, 0.77, cat.pos[4], cat.dist[4])
	tmp <- textGrob(
		label = category[4],
		x = cat.pos.4$x,
		y = cat.pos.4$y,
		just = cat.just[[4]],
		gp = gpar(
			col = cat.col[4],
			cex = cat.cex[4],
			fontface = cat.fontface[4],
			fontfamily = cat.fontfamily[4]
			)
		);
	grob.list <- gList(grob.list, tmp);

	# adjust grob.list to fit and return grob.list
	grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(grob.list, rotation.degree, rotation.centre[1], rotation.centre[2]), ...);
	if (ind) { grid.draw(grob.list); }
	return(grob.list);

	}
