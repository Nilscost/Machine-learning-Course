library(dslabs)
data("tissue_gene_expression")
x <- tissue_gene_expression$x
d <- dist(tissue_gene_expression$x)
x_1 <- x[1,]
x_2 <- x[2,]
sqrt(sum((x_1 - x_2)^2))
x_39 <- x[39,]
x_40 <- x[40,]
sqrt(sum((x_39 - x_40)^2))
x_73 <- x[73,]
x_74 <- x[74,]
sqrt(sum((x_73 - x_74)^2))
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))
