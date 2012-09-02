### FUNCTION TO ADD TEXT TO VENN DIAGRAM ##########################################################
add.title <- function(gList, x, pos = c(0.5, 1.05), cex = 1, fontface = 'plain', fontfamily = 'serif', col = 'black', just = c(0.5, 1), ...) {

	tmp <- textGrob(
		label = x,
		x = pos[1],
		y = pos[2],
		just = just,
		gp = gpar(
			col = col,
			cex = cex,
			fontface = fontface,
			fontfamily = fontfamily
			)
		);

	grob.list <- gList(gList, tmp);

	return(VennDiagram::adjust.venn(grob.list, ...))

	}
