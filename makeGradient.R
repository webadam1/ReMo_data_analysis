#basis of function written by Natália Novák
makeGradient <- function(vec, palette_of_two, highlight = FALSE) {
  
  # ha nem értelmezhető adatokat kapunk, nem módosít semmit
  if (class(vec)=="factor" || class(vec)=="character") {
    return();
  }
  
  # ismétlődő elemek kiszűrése
  vec <- unique(vec)
  vec <- sort(vec, decreasing = FALSE)
  
  c1 <- col2rgb(palette_of_two[1], alpha = FALSE)
  c2 <- col2rgb(palette_of_two[2], alpha = FALSE)
  
  # skálázás
  r <- integer(0)
  g <- integer(0)
  b <- integer(0)
  j = 0
  for (i in vec) {
    i = (i-min(vec))/(max(vec)-min(vec))
    j = j+1
    r[j] <- round(c1[1] + i*(c2[1]-c1[1]), 0)
    g[j] <- round(c1[2] + i*(c2[2]-c1[2]), 0)
    b[j] <- round(c1[3] + i*(c2[3]-c1[3]), 0)
  }
  
  if (length(vec) > 2 && highlight) {
    midIndex = ceiling(length(vec)/2)
    r[midIndex] = round((r[midIndex] + 255) / 2)
    g[midIndex] = round((g[midIndex] + 255) / 2)
    b[midIndex] = round((b[midIndex] + 255) / 2)
  }
  
  # legyen minden hexa
  r <- format(as.hexmode(r), width=2)
  g <- format(as.hexmode(g), width=2)
  b <- format(as.hexmode(b), width=2)
  
  hex <- data.frame(r, g, b)
  
  # legyen n darab hex stringünk
  cols <- character(0)
  j <- 0
  while (j <= length(vec)) {
    j <- j + 1;
    cols <- c(cols, paste('#', hex[j,1], hex[j,2], hex[j,3], sep = ""))
  }
  
  return(cols)
}