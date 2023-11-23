## Logistic kernel for Q. robur according to Zani et al.
## SDD = 25m, LDD = 200m, b = 3.5

pdf_logistic = function(DD, b = 0){
  
  a = (gamma(2/b)*gamma(1-(2/b)))/(gamma(3/b)*gamma(1-(3/b)))*DD
  
  f2 = function(r)
    2*pi*r*b/(2*pi*a^2*gamma(2/b)*gamma(1-(2/b)))*(1+(r^b/a^b))^(-1) # kD(r) = 2*pi*r*kL(r), see Nathan et al. (2012)
  
  return(f2)
}

int_logistic = function (DD, b = 0, d) {
  cubature::cubintegrate(pdf_logistic(DD, b),  lower = d, upper = Inf)$integral
}

# SD kernel
ncells <- 4
disp_kernel_SD <- sapply(1:ncells, function(i) int_logistic(DD = 25, b = 3.5, d = (i-1)*res))
disp_kernel_SD[1] <- 1

# LD kernel
ncells <- 40
disp_kernel_LD <- sapply(1:ncells, function(i) int_logistic(DD = 200, b = 3.5, d = (i-1)*res))
disp_kernel_LD[1] <- 1

# MatAge <- 40 # litterature
MatAge <- 20 # pers. observations (I. Chuine)