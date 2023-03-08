Function FWiggler, e,cur, dist, ec, k, rgam, nper, npoints = np, window = win,$
    bandwidth = ban, flux = flu, power = pow, xvals = xvl, yvals = yvl, $
    showprog = sho
    
    mult = 1.e3
    phdiv = 1.6021773e-19

    wdist = 1e3*dist
    ban = Default(ban,1e-3,/dtyp)
    shofl = keyword_set(sho)
    res = make_array(n_elements(e),2*np[0]+1,2*np[1]+1,/float)

    twin = win
    twin(0) = twin(0) < wdist*k/rgam

    tetpsi = Make_grid(1/wdist*([-1,1]#transpose(twin)),2*np + 1)
    tet = reform(tetpsi(0,*,*))
    psi = reform(tetpsi(1,*,*))
    xvl = wdist*reform(tet[*,0])
    yvl = wdist*reform(psi[0,*])
    dtet = (max(tet,min=min) - min)/(2*np(0))
    dpsi = (max(psi,min=min) - min)/(2*np(1))
    dom = dtet*dpsi
    arel = (wdist)^2*dtet*dpsi

    for i = 0l, n_elements(e) - 1 do begin
	res[i,*,*] = Twig(e(i),ec,k,rgam,tet,psi,nper)
	if shofl then print, i, e(i)
    endfor

    res = mult*cur/(wdist)^2*res
    if keyword_set(flu) then res = ban/(mult*phdiv)*res
    return, res
end
