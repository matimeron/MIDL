Function Twig, e, ec, k, rgam, tet, psi, nper

    on_error, 1
    alp = 7.297353e-3
    t = 1./3.

    nper = Default(nper,1,/dtyp)

    if n_elements(e) gt 1 then message, 'only scalar energy accepted'
    if n_elements(tet) gt 1 and n_elements(psi) gt 1 then $
    if not Arreq(tet,psi,/noval) then message, 'angle data mismatch'

    ect = ec*sqrt((1. - (rgam*tet/k)^2) > 0)
    ksi0 = 0*ect
    dum = where(ect ne 0,ndum)
    if ndum ne 0 then ksi0(dum) = e/(2*ect[dum])
    ptem = sqrt(1 + (rgam*psi)^2) 
    ksi = ksi0*ptem^3 > toler(ksi0)

    if e eq 0 then res = 0*ksi else res = 6*alp*nper*(rgam/!pi*ksi0)^2* $
    ((ptem^2*Beselk_mm(ksi,2*t))^2 + (ptem^2-1)*(ptem*Beselk_mm(ksi,t))^2)

    return, FPU_fix(res)
end
