# using a function to check diff combination of parameters
#
nm = c("cement", "water", "fine_agg")

flm = function(nm, data){
  flma = as.formula(paste("y ~ ", paste(nm, collapse = "+")))
  mylm = lm(flma, data = data)
}

# x is your data, which has colmns including nm items.
mylm = flm(nm, x)