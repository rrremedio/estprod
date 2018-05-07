m <- matrix(c(1:10), dimnames = list(NULL, c("var2","var3")), nrow = 5)

m_poly <- poly(m, degree = 2,  raw = T) * 1/2

#m_poly <- m_poly[,seq(attr(m_poly, "degree"))[attr(m_poly, "degree") > 1]]


poly_name <- function(poly_data, original_data, sep = "_"){
  #x: original poly names
  x <- colnames(poly_data)
  
  #y: original var names
  if (class(original_data) %in% c("data.frame", "tbl", "matrix")) {
    y <-  colnames(original_data)
  }else {
    y <- original_data
  }
  
  
  #x_xtable: table with the names of x: each column one var. Each line: number of times in taht interaction
  x_table <- read.table(text = x, sep = ".")

  if (length(y) != ncol(x_table)) stop("Number of variables not the same!")
  
  
  
  colnames(x_table) <- y
  
  #for each column
  for (column in 1:ncol(x_table)) {
    
    #replace in each column: concatenates each line text (all cells) repeated by the number of each cell
    x_table[column] <- sapply(1:nrow(x_table), function(i) paste0(rep(colnames(x_table[column]), x_table[i,column]), collapse = sep))
    
  }
  
  
  #concatanate each line of x_table and cleans
  new_names <- matrix()
  for (i in 1:nrow(x_table)){
    new_names[i] <- paste0(x_table[i,x_table[i,] != ""], collapse = sep)
  }

  return(new_names) 
  
}

poly_name(m_poly, m)

m_poly
