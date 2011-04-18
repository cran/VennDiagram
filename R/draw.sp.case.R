### FUNCTION TO DRAW SPECIAL CASES ################################################################
draw.sp.case <- function(area.list, enabled.areas, area.x, area.y, attach.label.to, x.centres, y.centres, a.list, b.list, straight.reverse, reverse = FALSE, category, cat.default.pos = "outer", lwd = rep(2, 3), lty = rep("solid", 3), col = rep("black", 3), label.col = rep("black", 7), cex = rep(1, 7), fontface = rep("plain", 7), fontfamily = rep("serif", 7), cat.pos = c(-40, 40, 0), cat.dist = c(0.05, 0.05, 0.025), cat.col = rep("black", 3), cat.cex = rep(1, 3), cat.fontface = rep("plain", 3), cat.fontfamily = rep("serif", 3), cat.just = list(c(0.5, 1), c(0.5, 1), c(0.5, 0)), cat.prompts = FALSE, fill = NULL, alpha = rep(0.5, 3), ...) {
	grob.list <- gList();
	tmp <- ellipse(
		x = x.centres[1], 
		y = y.centres[1], 
		a = a.list[1],
		b = b.list[1], 
		gp = gpar(
			lty = 0,
			fill = fill[1],
			alpha = alpha[1]
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- ellipse(
		x = x.centres[2], 
		y = y.centres[2], 
		a = a.list[2],
		b = b.list[2], 
		gp = gpar(
			lty = 0,
			fill = fill[2],
			alpha = alpha[2]
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- ellipse(
		x = x.centres[3], 
		y = y.centres[3], 
		a = a.list[3],
		b = b.list[3], 
		gp = gpar(
			lty = 0,
			fill = fill[3],
			alpha = alpha[3]
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- ellipse(
		x = x.centres[3], 
		y = y.centres[3], 
		a = a.list[3],
		b = b.list[3], 
		gp = gpar(
			lwd = lwd[3],
			lty = lty[3],
			col = col[3],
			fill = "transparent"
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- ellipse(
		x = x.centres[1], 
		y = y.centres[1], 
		a = a.list[1],
		b = b.list[1], 
		gp = gpar(
			lwd = lwd[1],
			lty = lty[1],
			col = col[1],
			fill = "transparent"
			)
		);
	grob.list <- gList(grob.list, tmp);
	tmp <- ellipse(
		x = x.centres[2], 
		y = y.centres[2], 
		a = a.list[2],
		b = b.list[2], 
		gp = gpar(
			lwd = lwd[2],
			lty = lty[2],
			col = col[2],
			fill = "transparent"
			)
		);
	grob.list <- gList(grob.list, tmp);
	
	if (1 %in% enabled.areas) {
		tmp <- textGrob(
			label = area.list[1],
			x = area.x[1],
			y = area.y[1],
			just = c("centre", "centre"),
			gp = gpar(
				col = label.col[1],
				cex = cex[1],
				fontface = fontface[1],
				fontfamily = fontfamily[1]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}
	if (2 %in% enabled.areas) {
		tmp <- textGrob(
			label = area.list[2],
			x = area.x[2],
			y = area.y[2],
			just = c("centre", "centre"),
			gp = gpar(
				col = label.col[2],
				cex = cex[2],
				fontface = fontface[2],
				fontfamily = fontfamily[2]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}
	if (3 %in% enabled.areas) {
		tmp <- textGrob(
			label = area.list[3],
			x = area.x[3],
			y = area.y[3],
			just = c("centre", "centre"),
			gp = gpar(
				col = label.col[3],
				cex = cex[3],
				fontface = fontface[3],
				fontfamily = fontfamily[3]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}
	if (4 %in% enabled.areas) {
		tmp <- textGrob(
			label = area.list[4],
			x = area.x[4],
			y = area.y[4],
			just = c("centre", "centre"),
			gp = gpar(
				col = label.col[4],
				cex = cex[4],
				fontface = fontface[4],
				fontfamily = fontfamily[4]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}
	if (5 %in% enabled.areas) {
		tmp <- textGrob(
			label = area.list[5],
			x = area.x[5],
			y = area.y[5],
			just = c("centre", "centre"),
			gp = gpar(
				col = label.col[5],
				cex = cex[5],
				fontface = fontface[5],
				fontfamily = fontfamily[5]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}
	if (6 %in% enabled.areas) {
		tmp <- textGrob(
		label = area.list[6],
			x = area.x[6],
			y = area.y[6],
			just = c("centre", "centre"),
			gp = gpar(
				col = label.col[6],
				cex = cex[6],
				fontface = fontface[6],
				fontfamily = fontfamily[6]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}
	if (7 %in% enabled.areas) {
		tmp <- textGrob(
			label = area.list[7],
			x = area.x[7],
			y = area.y[7],
			just = c("centre", "centre"),
			gp = gpar(
				col = label.col[7],
				cex = cex[7],
				fontface = fontface[7],
				fontfamily = fontfamily[7]
				)
			);
		grob.list <- gList(grob.list, tmp);
		}
	
	if (cat.default.pos == "outer") { cat.pos.1 <- find.cat.pos(x.centres[1], y.centres[1], cat.pos[1], cat.dist[1], a.list[1]) }
	if (cat.default.pos == "text") { cat.pos.1 <- find.cat.pos(area.x[attach.label.to[1]], area.y[attach.label.to[1]], cat.pos[1], cat.dist[1]) }
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
	if (cat.default.pos == "outer") { cat.pos.2 <- find.cat.pos(x.centres[2], y.centres[2], cat.pos[2], cat.dist[2], a.list[2]) }
	if (cat.default.pos == "text") { cat.pos.2 <- find.cat.pos(area.x[attach.label.to[2]], area.y[attach.label.to[2]], cat.pos[2], cat.dist[2]) }
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
	if (cat.default.pos == "outer") { cat.pos.3 <- find.cat.pos(x.centres[3], y.centres[3], cat.pos[3], cat.dist[3], a.list[3]) }
	if (cat.default.pos == "text") { cat.pos.3 <- find.cat.pos(area.x[attach.label.to[3]], area.y[attach.label.to[3]], cat.pos[3], cat.dist[3]) }
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
	grob.list <- adjust.venn(grob.list, ...);
	if (straight.reverse) { 
		if (reverse) {
			return(flip.venn(grob.list, axis = "v"))
			}
		}

	return(grob.list);
	}
