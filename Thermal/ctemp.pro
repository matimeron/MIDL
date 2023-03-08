Function Ctemp, d, t, _extra = _e

    common term_stuff, sc_ex, sc_ntr, sc_rt, $
        bc_ex, bc_eps, bc_lam, bc_ntr, bc_rt

    nd = n_elements(d)
    nt = n_elements(t)
    ptemp = dblarr(nd,nt)

    for j = nt - 1, 0, -1 do begin
	print, j, t[j]
	for i = 0, nd - 1 do begin
	    ptemp[i,j] = CBC_temp(d[i],t[j],1,1e-8,prec='hi',_extra=_e)
	endfor
    endfor

    return, ptemp
end
