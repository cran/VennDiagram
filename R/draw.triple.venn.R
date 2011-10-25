### FUNCTION TO DRAW VENN DIAGRAM WITH THREE SETS #################################################
draw.triple.venn <- function(area1, area2, area3, n12, n23, n13, n123, category = rep("", 3), rotation = 1, reverse = FALSE, euler.d = TRUE, scaled = TRUE, cat.default.pos = "outer", lwd = rep(2, 3), lty = rep("solid", 3), col = rep("black", 3), label.col = rep("black", 7), cex = rep(1, 7), fontface = rep("plain", 7), fontfamily = rep("serif", 7), cat.pos = c(-40, 40, 180), cat.dist = c(0.05, 0.05, 0.025), cat.col = rep("black", 3), cat.cex = rep(1, 3), cat.fontface = rep("plain", 3), cat.fontfamily = rep("serif", 3), cat.just = list(c(0.5, 1), c(0.5, 1), c(0.5, 0)), cat.prompts = FALSE, fill = NULL, alpha = rep(0.5, 3), rotation.degree = 0, rotation.centre = c(0.5, 0.5), ind = TRUE, list.order = 1:3, offset = 0, ...) {
#area1 must be greater than area2, which must be greater than area3	
	# check parameter lengths
	if (length(category) == 1) {cat <- rep(category, 3)}
	if (length(category) != 1 & length(category) != 3) { stop("Unexpected parameter length for 'category'") }
	if (length(lwd) == 1) {lwd <- rep(lwd, 3)}
	if (length(lwd) != 1 & length(lwd) != 3) { stop("Unexpected parameter length for 'lwd'") }
	if (length(lty) == 1) {lty <- rep(lty, 3)}
	if (length(lty) != 1 & length(lty) != 3) { stop("Unexpected parameter length for 'lty'") }
	if (length(col) == 1) {col <- rep(col, 3)}
	if (length(col) != 1 & length(col) != 3) { stop("Unexpected parameter length for 'col'") }
	if (length(label.col) == 1) {label.col <- rep(label.col, 7)}
	if (length(label.col) != 1 & length(label.col) != 7) { stop("Unexpected parameter length for 'label.col'") }
	if (length(cex) == 1) {cex <- rep(cex, 7)}
	if (length(cex) != 1 & length(cex) != 7) { stop("Unexpected parameter length for 'cex'") }
	if (length(fontface) == 1) {fontface <- rep(fontface, 7)}
	if (length(fontface) != 1 & length(fontface) != 7) { stop("Unexpected parameter length for 'fontface'") }
	if (length(fontfamily) == 1) {fontfamily <- rep(fontfamily, 7)}
	if (length(fontfamily) != 1 & length(fontfamily) != 7) { stop("Unexpected parameter length for 'fontfamily'") }
	if (length(fill) == 1) {fill <- rep(fill, 3)}
	if (length(fill) != 1 & length(fill) != 3 & length(fill) != 0) { stop("Unexpected parameter length for 'fill'") }
	if (length(alpha) == 1) {alpha <- rep(alpha, 3)}
	if (length(alpha) != 1 & length(alpha) != 3 & length(alpha) != 0) { stop("Unexpected parameter length for 'alpha'") }
	if (length(cat.pos) == 1) {cat.pos <- rep(cat.pos, 3)}
	if (length(cat.pos) != 1 & length(cat.pos) != 3) { stop("Unexpected parameter length for 'cat.pos'") }
	if (length(cat.dist) == 1) {cat.dist <- rep(cat.dist, 3)}
	if (length(cat.dist) != 1 & length(cat.dist) != 3) { stop("Unexpected parameter length for 'cat.dist'") }
	if (length(cat.col) == 1) {cat.col <- rep(cat.col, 3)}
	if (length(cat.col) != 1 & length(cat.col) != 3) { stop("Unexpected parameter length for 'cat.col'") }
	if (length(cat.cex) == 1) {cat.cex <- rep(cat.cex, 3)}
	if (length(cat.cex) != 1 & length(cat.cex) != 3) { stop("Unexpected parameter length for 'cat.cex'") }
	if (length(cat.fontface) == 1) {cat.fontface <- rep(cat.fontface, 3)}
	if (length(cat.fontface) != 1 & length(cat.fontface) != 3) { stop("Unexpected parameter length for 'cat.fontface'") }
	if (length(cat.fontfamily) == 1) {cat.fontfamily <- rep(cat.fontfamily, 3)}
	if (length(cat.fontfamily) != 1 & length(cat.fontfamily) != 3) { stop("Unexpected parameter length for 'cat.fontfamily'") }
	if (!(class(cat.just) == "list" & length(cat.just) == 3)) { stop("Unexpected parameter format for 'cat.just'") }
	else if (!(length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 2 & length(cat.just[[3]]) == 2)) { stop("Unexpected parameter format for 'cat.just'") }
	
	# check uninterpretable parameter combination
	if (euler.d == FALSE & scaled == TRUE) {
		stop("Uninterpretable parameter combination\nPlease set both euler.d = FALSE and scaled = FALSE to force Venn diagrams.");
		}
	
	lwd <- lwd[list.order];
	lty <- lty[list.order];
	col <- col[list.order];
	fill <- fill[list.order];
	alpha <- alpha[list.order];
	cat.col <- cat.col[list.order];
	cat.cex <- cat.cex[list.order];
	cat.fontface <- cat.fontface[list.order];
	cat.fontfamily <- cat.fontfamily[list.order];
	
	cat.pos <- cat.pos + rotation.degree;
	
	# generate partial areas from given arguments
	a1 <- area1 - n12 - n13 + n123;
	a2 <- n12 - n123;
	a3 <- area2 - n12 - n23 + n123;
	a4 <- n13 - n123;
	a5 <- n123;
	a6 <- n23 - n123;
	a7 <- area3 - n13 - n23 + n123;

	# check for special cases and if necessary process them
	if (euler.d) {

		# figure out the special case code
		special.code <- VennDiagram::decide.special.case(a1, a2, a3, a4, a5, a6, a7);

		# and convert into a proper function name
		function.name <- paste('draw.', special.code, sep = '');

		# did we define a special-case function for this case?
		if (function.name %in% ls('package:VennDiagram')) {

			# get the special-case function
			f1 <- get(function.name);

			# run it
			rst <- f1(
				a1 = a1,
				a2 = a2,
				a3 = a3,
				a4 = a4,
				a5 = a5,
				a6 = a6,
				a7 = a7,
				category = category,
				reverse = reverse,
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
				cat.prompts = cat.prompts,
				fill = fill,
				alpha = alpha,
				...
				);

			# rotate the Venn diagram as needed
			rst <- VennDiagram::adjust.venn(
				VennDiagram::rotate.venn.degrees(
					gList1 = rst,
					angle = rotation.degree,
					x.centre = rotation.centre[1],
					y.centre = rotation.centre[2]
					),
				...
				);

			# TO COMMENT
			if (ind) { grid.draw(rst); }

			# exit the function here
			return(rst);
			}

		}
	
	rotated <- VennDiagram::rotate(c(a1, a2, a3, a4, a5, a6, a7), category, lwd, lty, col, label.col, cex, fontface, fontfamily, cat.col, cat.cex, cat.fontface, cat.fontfamily, alpha, rotation, reverse, fill);
	a1 <- rotated[[1]][1];
	a2 <- rotated[[1]][2];
	a3 <- rotated[[1]][3];
	a4 <- rotated[[1]][4];
	a5 <- rotated[[1]][5];
	a6 <- rotated[[1]][6];
	a7 <- rotated[[1]][7];
	category <- rotated[[2]];
	lwd <- rotated$lwd;
	lty <- rotated$lty;
	col <- rotated$col;
	label.col <- rotated$label.col;
	cex <- rotated$cex;
	fontface <- rotated$fontface;
	fontfamily <- rotated$fontfamily;
	cat.col <- rotated$cat.col;
	cat.cex <- rotated$cat.cex;
	cat.fontface <- rotated$cat.fontface;
	cat.fontfamily <- rotated$cat.fontfamily;
	fill <- rotated$fill;
	alpha <- rotated$alpha
	
	# check plausibility and 0 partial areas
	if (any(a1 < 0, a2 < 0, a3 < 0, a4 < 0, a5 < 0, a6 < 0, a7 < 0)) { stop("Impossible: partial areas negative") }
	if (any(a1 == 0, a2 == 0, a3 == 0, a4 == 0, a5 == 0, a6 == 0, a7 == 0)) { scaled <- FALSE; }

	# check if defaults are being used
	is.defaults <- TRUE;
	if (is.expression(category)) { is.defaults <- FALSE; }

	# check category label defaults
	if (cat.default.pos != "outer" & cat.default.pos != "text" & !is.defaults & cat.prompts) { # PCB: removed this check from the if, needs verification: & isTRUE(category != rep("", 2))
		print("No default location recognized.  Automatically changing to 'outer'");
		cat.default.pos <- "outer";
		}
	if (cat.default.pos == "outer" & !is.defaults & cat.prompts) {
		print("Placing category labels at default outer locations.  Use 'cat.pos' and 'cat.dist' to modify location.");
		print(paste("Current 'cat.pos':", cat.pos[1], "degrees,", cat.pos[2], "degrees"));
		print(paste("Current 'cat.dist':", cat.dist[1], ",", cat.dist[2]));
		}
	if (cat.default.pos == "text" & !is.defaults & cat.prompts) {
		print("Placing category labels at default text locations.  Use 'cat.pos' and 'cat.dist' to modify location.");
		print(paste("Current 'cat.pos':", cat.pos[1], "degrees,", cat.pos[2], "degrees"));
		print(paste("Current 'cat.dist':", cat.dist[1], ",", cat.dist[2]));
		}
		
	# initialize gList to hold all Grobs generated
	grob.list <- gList();
	
	# initialize radius values for all circles
	if (!exists("overrideTriple")) {
		r1 <- sqrt(100 / pi);
		r2 <- r1;
		r3 <- r1;
		}
		
	if (exists("overrideTriple")) {
		r1 <- sqrt(area1 / pi);
		r2 <- sqrt(area2 / pi);
		r3 <- sqrt(area3 / pi);
		}
	
	max.circle.size = 0.2;
	shrink.factor <- max.circle.size / r1;
	
	r1 <- r1 * shrink.factor;
	r2 <- r2 * shrink.factor;
	r3 <- r3 * shrink.factor;

	if (!exists("overrideTriple")) {
		a <- find.dist(100, 100, 40) * shrink.factor;
		b <- a;
		c <- a;
		}
		
	if (exists("overrideTriple")) {
		a <- find.dist(area1, area2, n12) * shrink.factor;
		b <- find.dist(area2, area3, n23) * shrink.factor;
		c <- find.dist(area1, area3, n13) * shrink.factor;
		}
		
	# obtain the centres of the three circles based on their pairwise distances
	beta <- (a^2 + c^2 - b^2) / (2 * a * c);
	gamma <- sqrt(1 - beta^2);
	x.centre.1 <- (r1 - r2 - a + 1) / 2;
	x.centre.3 <- x.centre.1 + c * beta;
	y.centre.3 <- (r3 - r1 + 1 - c * gamma) / 2;
	y.centre.1 <- y.centre.3 + c * gamma;
	x.centre.2 <- x.centre.1 + a;
	y.centre.2 <- y.centre.1;
	
	# plot the circles of the Venn diagram
	tmp <- circle(
		x = x.centre.1, 
		y = y.centre.1, 
		r = r1,
		gp = gpar(
			lty = 0,
			fill = fill[1],
			alpha = alpha[1]
			)
		);
	grob.list <- gList(grob.list, tmp);

	tmp <- VennDiagram::circle(
		x = x.centre.2, 
		y = y.centre.2, 
		r = r2,
		gp = gpar(
			lty = 0,
			fill = fill[2],
			alpha = alpha[2]
			)
		);
	grob.list <- gList(grob.list, tmp);

	tmp <- VennDiagram::circle(
		x = x.centre.3, 
		y = y.centre.3, 
		r = r3,
		gp = gpar(
			lty = 0,
			fill = fill[3],
			alpha = alpha[3]
			)
		);
	grob.list <- gList(grob.list, tmp);

	tmp <- circle(
		x = x.centre.1, 
		y = y.centre.1, 
		r = r1,
		gp = gpar(
			lwd = lwd[1],
			lty = lty[1],
			col = col[1],
			fill = "transparent"
			)
		);
	grob.list <- gList(grob.list, tmp);

	tmp <- circle(
		x = x.centre.2, 
		y = y.centre.2, 
		r = r2,
		gp = gpar(
			lwd = lwd[2],
			lty = lty[2],
			col = col[2],
			fill = "transparent"
			)
		);
	grob.list <- gList(grob.list, tmp);

	tmp <- circle(
		x = x.centre.3, 
		y = y.centre.3, 
		r = r3,
		gp = gpar(
				lwd = lwd[3],
				lty = lty[3],
				col = col[3],
				fill = "transparent"
			)
		);
	grob.list <- gList(grob.list, tmp);

	# calculate the location of the text labels of the Venn diagram and plot
	x.cept.12 <- (r1^2 - r2^2 - x.centre.1^2 + x.centre.2^2) / (2 * (x.centre.2 - x.centre.1))
	y.cept.12.1 <- sqrt(r1^2 - (x.cept.12 - x.centre.1)^2) + y.centre.1
	y.cept.12.2 <- -sqrt(r1^2 - (x.cept.12 - x.centre.1)^2) + y.centre.1
	theta <- acos((a^2 + c^2 - b^2) / (2 * a * c));
	new.x.centre.3 <- x.centre.1 + c;
	l.x.cept.13 <- (r1^2 - r3^2 - x.centre.1^2 + new.x.centre.3^2) / (2 * (new.x.centre.3 - x.centre.1));
	l.y.cept.13.1 <- sqrt(r1^2 - (l.x.cept.13 - x.centre.1)^2) + y.centre.1;
	l.y.cept.13.2 <- -sqrt(r1^2 - (l.x.cept.13 - x.centre.1)^2) + y.centre.1;
	rot <- sqrt(2 * r1^2 - 2 * r1^2 * cos(theta));
	x.cept.13.1 <- l.x.cept.13 + rot * cos(pi / 2 - atan((l.y.cept.13.1 - y.centre.1) / (l.x.cept.13 - x.centre.1)) + theta / 2);
	x.cept.13.2 <- l.x.cept.13 + rot * cos(pi / 2 - atan((l.y.cept.13.2 - y.centre.1) / (l.x.cept.13 - x.centre.1)) + theta / 2);
	y.cept.13.1 <- l.y.cept.13.1 - rot * sin(pi / 2 - atan((l.y.cept.13.1 - y.centre.1) / (l.x.cept.13 - x.centre.1)) + theta / 2);
	y.cept.13.2 <- l.y.cept.13.2 - rot * sin(pi / 2 - atan((l.y.cept.13.2 - y.centre.1) / (l.x.cept.13 - x.centre.1)) + theta / 2);
	theta <- -acos((a^2 + b^2 - c^2) / (2 * a * b));
	new.x.centre.3 <- x.centre.2 - b;
	l.x.cept.23 <- (r2^2 - r3^2 - x.centre.2^2 + new.x.centre.3^2) / (2 * (new.x.centre.3 - x.centre.2));
	l.y.cept.23.1 <- sqrt(r2^2 - (l.x.cept.23 - x.centre.2)^2) + y.centre.2;
	l.y.cept.23.2 <- -sqrt(r2^2 - (l.x.cept.23 - x.centre.2)^2) + y.centre.2;
	rot <- sqrt(2 * r2^2 - 2 * r2^2 * cos(theta));
	x.cept.23.1 <- l.x.cept.23 + rot * cos(pi / 2 - atan((y.centre.2 - l.y.cept.23.1) / (x.centre.2 - l.x.cept.23)) + theta / 2);
	x.cept.23.2 <- l.x.cept.23 + rot * cos(pi / 2 - atan((y.centre.2 - l.y.cept.23.2) / (x.centre.2 - l.x.cept.23)) + theta / 2);
	y.cept.23.1 <- l.y.cept.23.1 - rot * sin(pi / 2 - atan((y.centre.2 - l.y.cept.23.1) / (x.centre.2 - l.x.cept.23)) + theta / 2);
	y.cept.23.2 <- l.y.cept.23.2 - rot * sin(pi / 2 - atan((y.centre.2 - l.y.cept.23.2) / (x.centre.2 - l.x.cept.23)) + theta / 2);
	m <- (y.cept.23.2 - y.cept.23.1) / (x.cept.23.2 - x.cept.23.1);
	y.sect <- m * (x.cept.12 - x.cept.23.1) + y.cept.23.1;
	a5.x.pos <- x.cept.12;
	a5.y.pos <- y.sect;
	tmp <- textGrob(
		label = a5, 
		x = a5.x.pos, 
		y = a5.y.pos,
		gp = gpar(
			col = label.col[5],
			cex = cex[5],
			fontface = fontface[5],
			fontfamily = fontfamily[5]
			)
		);
	grob.list <- gList(grob.list, tmp);
	
	m <- (y.cept.13.2 - y.cept.13.1) / (x.cept.13.2 - x.cept.13.1);
	y0 <- y.centre.2;
	x0 <- x.centre.2;
	b <- y.cept.13.1 - m * x.cept.13.1;
	x.sect <- (m*y0 + x0 - m*b) / (m^2+1) + sqrt(r2^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	y.sect <- (m^2*y0 + m*x0 + b) / (m^2+1) + m * sqrt(r2^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	a3.x.pos <- (x.cept.13.1 + x.sect) / 2;
	a3.y.pos <- (y.cept.13.1 + y.sect) / 2;
	tmp <- textGrob(
		label = a3, 
		x = a3.x.pos, 
		y = a3.y.pos,
		gp = gpar(
			col = label.col[3],
			cex = cex[3],
			fontface = fontface[3],
			fontfamily = fontfamily[3]
			)
		);
	grob.list <- gList(grob.list, tmp);
	m <- (y.cept.23.2 - y.cept.23.1) / (x.cept.23.2 - x.cept.23.1);
	y0 <- y.centre.1;
	x0 <- x.centre.1;
	b <- y.cept.23.1 - m * x.cept.23.1;
	x.sect <- (m*y0 + x0 - m*b) / (m^2+1) - sqrt(r1^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	y.sect <- (m^2*y0 + m*x0 + b) / (m^2+1) - m * sqrt(r1^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	a1.x.pos <- (x.cept.23.1 + x.sect) / 2;
	a1.y.pos <- (y.cept.23.1 + y.sect) / 2;
	tmp <- textGrob(
		label = a1, 
		x = a1.x.pos, 
		y = a1.y.pos,
		gp = gpar(
			col = label.col[1],
			cex = cex[1],
			fontface = fontface[1],
			fontfamily = fontfamily[1]
			)
		);
	grob.list <- gList(grob.list, tmp);
	y.sect <- -sqrt(r3^2 - (x.cept.12 - x.centre.3)^2) + y.centre.3;
	a7.x.pos <- x.cept.12;
	a7.y.pos <- (y.cept.12.2 + y.sect) / 2;
	tmp <- textGrob(
		label = a7, 
		x = a7.x.pos, 
		y = a7.y.pos,
		gp = gpar(
			col = label.col[7],
			cex = cex[7],
			fontface = fontface[7],
			fontfamily = fontfamily[7]
			)
		);
	grob.list <- gList(grob.list, tmp);
	m <- (y.cept.23.2 - y.cept.23.1) / (x.cept.23.2 - x.cept.23.1);
	y0 <- y.centre.1;
	x0 <- x.centre.1;
	b <- y.cept.23.1 - m * x.cept.23.1;
	x.sect <- (m*y0 + x0 - m*b) / (m^2+1) + sqrt(r1^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	y.sect <- (m^2*y0 + m*x0 + b) / (m^2+1) + m * sqrt(r1^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	a6.x.pos <- (x.cept.23.2 + x.sect) / 2;
	a6.y.pos <- (y.cept.23.2 + y.sect) / 2;
	tmp <- textGrob(
		label = a6, 
		x = a6.x.pos, 
		y = a6.y.pos,
		just = c(0.5, 0.5),
		gp = gpar(
			col = label.col[6],
			cex = cex[6],
			fontface = fontface[6],
			fontfamily = fontfamily[6]
			)
		);
	grob.list <- gList(grob.list, tmp);
	m <- (y.cept.13.2 - y.cept.13.1) / (x.cept.13.2 - x.cept.13.1);
	y0 <- y.centre.2;
	x0 <- x.centre.2;
	b <- y.cept.13.1 - m * x.cept.13.1;
	x.sect <- (m*y0 + x0 - m*b) / (m^2+1) - sqrt(r2^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	y.sect <- (m^2*y0 + m*x0 + b) / (m^2+1) - m * sqrt(r2^2 - ( (y0-m*x0-b)/sqrt(1+m^2) )^2) / sqrt(1+m^2);
	a4.x.pos <- (x.cept.13.2 + x.sect) / 2;
	a4.y.pos <- (y.cept.13.2 + y.sect) / 2;
	tmp <- textGrob(
		label = a4, 
		x = a4.x.pos, 
		y = a4.y.pos,
		just = c(0.5, 0.5),
		gp = gpar(
			col = label.col[4],
			cex = cex[4],
			fontface = fontface[4],
			fontfamily = fontfamily[4]
			)
		);
	grob.list <- gList(grob.list, tmp);
	y.sect <- sqrt(r3^2 - (x.cept.12 - x.centre.3)^2) + y.centre.3;
	a2.x.pos <- x.cept.12;
	a2.y.pos <- (y.cept.12.1 + y.sect) / 2;
	tmp <- textGrob(
		label = a2, 
		x = a2.x.pos, 
		y = a2.y.pos,
		just = c(0.5, 0.5),
		gp = gpar(
			col = label.col[2],
			cex = cex[2],
			fontface = fontface[2],
			fontfamily = fontfamily[2]
			)
		);
	grob.list <- gList(grob.list, tmp);

	# find the location of category names
	if (cat.default.pos == "outer") {
		cat.pos.1 <- find.cat.pos(x.centre.1, y.centre.1, cat.pos[1], cat.dist[1], r1);
		cat.pos.2 <- find.cat.pos(x.centre.2, y.centre.2, cat.pos[2], cat.dist[2], r2);
		cat.pos.3 <- find.cat.pos(x.centre.3, y.centre.3, cat.pos[3], cat.dist[3], r3);
		}
	else if (cat.default.pos == "text") {
		cat.pos.1 <- find.cat.pos(a1.x.pos, a1.y.pos, cat.pos[1], cat.dist[1]);
		cat.pos.2 <- find.cat.pos(a3.x.pos, a3.y.pos, cat.pos[2], cat.dist[2]);
		cat.pos.3 <- find.cat.pos(a7.x.pos, a7.y.pos, cat.pos[3], cat.dist[3]);
		}
	else {
		stop("Invalid setting of 'cat.default.pos' -- should be 'outer' or 'text'");
		}

	# plot all category names
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

	# if requested, rotate the Venn Diagram
	grob.list <- VennDiagram::adjust.venn(
		VennDiagram::rotate.venn.degrees(
			gList1 = grob.list,
			angle = rotation.degree,
			x.centre = rotation.centre[1],
			y.centre = rotation.centre[2]
			),
		...
		);

	if (ind) { grid.draw(grob.list); }

	return(grob.list);
	
	}
