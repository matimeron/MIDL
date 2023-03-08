Function NWiggler, e,cur, dist, ec, k, rgam, nper, npoints = np, window = win,$
    flux = flu, bandwidth = ban, power = pow, showprog = sho
    
    mult = 1.e3
    phdiv = 1.6021773e-19

    ban = Default(ban,1e-3,/dtyp)
    shofl = keyword_set(sho)
    res = 0.*e

    twin = 1.*win
    twin(0) = twin(0) < 1e3*dist*k/rgam

    tetpsi = Make_grid(1e-3/dist*([-1,1]#transpose(twin)),2*np + 1)
    tet = reform(tetpsi(0,*,*))
    psi = reform(tetpsi(1,*,*))
    dtet = (max(tet,min=min) - min)/(2*np(0))
    dpsi = (max(psi,min=min) - min)/(2*np(1))
    dom = dtet*dpsi
    arel = (1e3*dist)^2*dtet*dpsi

    for i = 0l, n_elements(res) - 1 do begin
	tem = Twig(e(i),ec,k,rgam,tet,psi,nper)
	res(i) = Partot(tem,symfringe=[-0.5,-0.5])
	if shofl then print, i, e(i), res(i)
    endfor

    res = dom*mult*cur*res
    pow = Integ(e,res,/val)
    if keyword_set(flu) then res = ban/(mult*phdiv)*res

    return, res

end
