Function Sk, kr, eta

    coe = eta/(1. - eta)

    rock = (coe*Sp_beselj(kr,0))^2 + 2*coe*Sp_beselj(2*kr,0)

    return, FPU_fix(1./(1 + rock))
end
