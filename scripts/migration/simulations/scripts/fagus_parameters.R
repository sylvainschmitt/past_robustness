## 2Dt kernel for F. sylvatica according to Zani et al.
## SDD = 25m, LDD = 200m, b = 2

pdf_2dt = function(DD, b = 0){
  
  a = 2/pi*gamma(b-1)/gamma(b-3/2)*DD
  
  f2 = function(r)
    2*pi*r*(b-1)/(pi*a^2)*(1 + r^2/a^2)^(-b) # kD(r) = 2*pi*r*kL(r), see Nathan et al. (2012)
  
  return(f2)
}

int_2dt = function (DD, b = 0, d) {
  cubature::cubintegrate(pdf_2dt(DD, b),  lower = d, upper = Inf)$integral
}

# SD kernel
ncells <- 4
disp_kernel_SD <- sapply(1:ncells, function(i) int_2dt(DD = 25, b = 2, d = (i-1)*res))
disp_kernel_SD[1] <- 1

# LD kernel
ncells <- 40
disp_kernel_LD <- sapply(1:ncells, function(i) int_2dt(DD = 200, b = 2, d = (i-1)*res))
disp_kernel_LD[1] <- 1

MatAge <- 40