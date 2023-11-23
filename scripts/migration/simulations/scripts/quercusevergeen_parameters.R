## Log-hyperbolic secant kernel for Q. ilex according to Zani et al.
## SDD = 25m, LDD = 200m, b = 0.4

pdf_logsec = function(DD, b = 0){
  
  a = DD
  
  f2 = function(r)
    2*pi*r*(1/(pi^2*b*r^2))/((r/a)^(1/b)+(r/a)^(-1/b)) # kD(r) = 2*pi*r*kL(r), see Nathan et al. (2012)
  
  return(f2)
}

int_logsec = function (DD, b = 0, d) {
  cubature::cubintegrate(pdf_logsec(DD, b),  lower = d, upper = Inf)$integral
}

# SD kernel
ncells <- 4
disp_kernel_SD <- sapply(1:ncells, function(i) int_logsec(DD = 25, b = 0.4, d = (i-1)*res))
disp_kernel_SD[1] <- 1

# LD kernel
ncells <- 40
disp_kernel_LD <- sapply(1:ncells, function(i) int_logsec(DD = 200, b = 0.4, d = (i-1)*res))
disp_kernel_LD[1] <- 1

# MatAge <- 40 # litterature
MatAge <- 20 # pers. observations (I. Chuine)