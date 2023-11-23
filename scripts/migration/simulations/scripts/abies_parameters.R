## NegExp kernel for A. alba according to Zani et al.
## SDD = 100m, LDD = 710m

pdf_negexp = function(DD){
  
  a = DD/2
  
  f2 = function(r)
    2*pi*r*1/(2*pi*a^2)*exp(-r/a) # kD(r) = 2*pi*r*kL(r), see Nathan et al. (2012)
  
  return(f2)
}

int_negexp = function (DD, d) {
  cubature::cubintegrate(pdf_negexp(DD),  lower = d, upper = Inf)$integral
}

# SD kernel
ncells <- 4
disp_kernel_SD <- sapply(1:ncells, function(i) int_negexp(DD = 100, d = (i-1)*res))
disp_kernel_SD[1] <- 1

# LD kernel
ncells <- 40
disp_kernel_LD <- sapply(1:ncells, function(i) int_negexp(DD = 710, d = (i-1)*res))
disp_kernel_LD[1] <- 1

MatAge <- 30