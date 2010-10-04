### FUNCTION TO DRAW VENN DIAGRAM WITH A SINGLE SET ###############################################
draw.single.venn <- function(area, category = "", cat.default.pos = "outer", lwd = 2, lty = "solid", col = "black", label.col = "black", cex = 1, fontface = "plain", fontfamily = "serif", cat.pos = 0, cat.dist = 0.025, cat.col = "black", cat.cex = 1, cat.fontface = "plain", cat.fontfamily = "serif", cat.just = list(c(0.5, 0.5)), cat.prompts = FALSE, fill = NULL, alpha = 0.5, rotation.degree = 0, rotation.centre = c(0.5, 0.5), ind = TRUE, ...) {
	
	# check parameter lengths
	if (length(category) != 1) { stop("Unexpected parameter length for 'category'.") }
	if (length(lwd) != 1) { stop("Unexpected parameter length for 'lwd'.") }
	if (length(lty) != 1) { stop("Unexpected parameter length for 'lty'.") }
	if (length(col) != 1) { stop("Unexpected parameter length for 'col'.") }
	if (length(label.col) != 1) { stop("Unexpected parameter length for 'label.col'.") }
	if (length(cex) != 1) { stop("Unexpected parameter length for 'cex'.") }
	if (length(fontface) != 1) { stop("Unexpected parameter length for 'fontface'.") }
	if (length(fontfamily) != 1) { stop("Unexpected parameter length for 'fontfamily'.") }
	if (length(fill) != 1 & length(fill) != 0) { stop("Unexpected parameter length for 'fill'.") }
	if (length(alpha) != 1 & length(alpha) != 0) { stop("Unexpected parameter length for 'alpha'.") }
	if (length(cat.pos) != 1) { stop("Unexpected parameter length for 'cat.pos'.") }
	if (length(cat.dist) != 1) { stop("Unexpected parameter length for 'cat.dist'.") }
	if (length(cat.col) != 1) { stop("Unexpected parameter length for 'cat.col'.") }
	if (length(cat.cex) != 1) { stop("Unexpected parameter length for 'cat.cex'.") }
	if (length(cat.fontface) != 1) { stop("Unexpected parameter length for 'cat.fontface'.") }
	if (length(cat.fontfamily) != 1) { stop("Unexpected parameter length for 'cat.fontfamily'.") }
	if (!(class(cat.just) == "list" & length(cat.just) == 1 & length(cat.just[[1]]) == 2)) { stop("Unexpected parameter format for 'cat.just'.") }
	
	cat.pos <- cat.pos + rotation.degree;
	
	# check category label defaults
	if (cat.default.pos != "outer" & cat.default.pos != "text" & category != "" & cat.prompts) {
		print("No default location recognized.  Automatically changing to 'outer'");
		cat.default.pos <- "outer"
		}
	if (cat.default.pos == "outer" & category != "" & cat.prompts) {
		print("Placing category labels at default outer locations.  Use 'cat.pos' and 'cat.dist' to modify location.");
		print(paste("Current 'cat.pos':", cat.pos, "degrees"));
		print(paste("Current 'cat.dist':", cat.dist));
		}
	if (cat.default.pos == "text" & category != "" & cat.prompts) {
		print("Placing category labels at default text locations.  Use 'cat.pos' and 'cat.dist' to modify location.");
		print(paste("Current 'cat.pos':", cat.pos, "degrees"));
		print(paste("Current 'cat.dist':", cat.dist));
		}
		
	max.circle.size = 0.2;
	# obtain radius corresponding to the circle with given area and convert it to Grid dimensions
	r1 <- sqrt(area / pi);
	shrink.factor <- max.circle.size / r1;
	r1 <- r1 * shrink.factor;
	
	# initialize gList to hold all Grobs generated
	grob.list <- gList();
	# plot Venn diagram
	tmp <- circle(
		x = 0.5, 
		y = 0.5, 
		r = r1,
		gp = gpar(
			lty = 0,
			fill = fill,
			alpha = alpha
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- circle(
		x = 0.5, 
		y = 0.5, 
		r = r1,
		gp = gpar(
			lwd = lwd,
			lty = lty,
			col = col,
			fill = "transparent"
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- textGrob(
		label = area,
		x = 0.5,
		y = 0.5,
		gp = gpar(
			col = label.col,
			cex = cex,
			fontface = fontface,
			fontfamily = fontfamily
			)
		);
	grob.list <- gList(grob.list, tmp);
		
	if (cat.default.pos == "outer") { cat.pos.1 <- find.cat.pos(0.5, 0.5, cat.pos, cat.dist, r1) }
	if (cat.default.pos == "text") { cat.pos.1 <- find.cat.pos(0.5, 0.5, cat.pos, cat.dist) }
	tmp <- textGrob(
		label = category,
		x = cat.pos.1$x,
		y = cat.pos.1$y,
		just = cat.just[[1]],
		gp = gpar(
			col = cat.col,
			cex = cat.cex,
			fontface = cat.fontface,
			fontfamily = cat.fontfamily
			)
		);
	grob.list <- gList(grob.list, tmp);
	
	grob.list <- adjust.venn(rotate.venn.degrees(grob.list, rotation.degree, rotation.centre[1], rotation.centre[2]), ...);
	if (ind) { grid.draw(grob.list) }
	return(grob.list);
	
	}
