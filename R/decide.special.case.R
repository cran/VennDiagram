### FUNCTION TO DECIDE TRIPLE SET SPECIAL CASES (EULER DIAGRAMS) ##################################
decide.special.case <- function(a1, a2, a3, a4, a5, a6, a7) {
	ao.1 <- c("N", "A", "N", "A", "N", "O", "N");
	ao.2 <- c("A", "N", "A", "N", "N", "N", "O");
	ao.3 <- c("N", "A", "N", "O", "N", "A", "N");
	ao.4 <- c("A", "N", "O", "N", "N", "N", "A");
	ao.5 <- c("N", "N", "N", "N", "N", "N", "N");
	ao.6 <- c("O", "N", "A", "N", "N", "N", "A");
	ao.7 <- c("N", "O", "N", "A", "N", "A", "N");
	ao.matrix <- rbind(ao.1, ao.2, ao.3, ao.4, ao.5, ao.6, ao.7);
	
	vector.137 <- c(a1, a3, a7);
	vector.246 <- c(a2, a4, a6);
	first.pos <- length(c(a5)[c(a5) == 0]);
	second.pos <- length(vector.246[vector.246 == 0]);
	third.pos <- length(vector.137[vector.137 == 0]);
	fourth.vector <- c("");
	if (second.pos >= 1 & third.pos >= 1 & second.pos < 3 & third.pos < 3) {
		second.indices <- c(2,4,6)[vector.246 == 0];
		third.indices <- c(1,3,7)[vector.137 == 0];
		combns <- combn(c(second.indices, third.indices), 2, simplify = FALSE);
		fourth.vector <- vector(length = length(combns), mode = "character");
		for (i in 1:length(combns)) {
			fourth.vector[i] <- ao.matrix[combns[[i]][1], combns[[i]][2]]
		}
		fourth.vector <- fourth.vector[fourth.vector != "N"]
	}
	fourth.vector <- sort(fourth.vector);
	accum <- "";
	for (i in 1:length(fourth.vector)) {
		accum <- paste(accum, fourth.vector[i], sep = "")
	}
	rst <- paste(first.pos, second.pos, third.pos, accum, sep = "")
	return(rst)
	}
