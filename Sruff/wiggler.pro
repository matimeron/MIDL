Function Wiggler, e, ec, k, rgam, npol, gtet, gpsi, cur

    mult = 2.2181294e-6

    if abs(gtet) lt k then begin
	pf = 1./(1. + gpsi^2)
	mu = e/(2*ec*pf^1.5*sqrt(1. - (gtet/k)^2))
	res = pf*mu^2*(Beselk_mm(mu,2./3)^2 + (1 - pf)*Beselk_mm(mu,1./3)^2)
    endif else res = 0.*e

    return, mult*cur*npol*res
end
