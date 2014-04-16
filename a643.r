

sig2=function(atm1,sigtm1){
    .119*atm1^2+.881*sigtm1^2
    +(4.276-.084*sigtm1^2)*(1+exp(-10*atm1))^-1
}
