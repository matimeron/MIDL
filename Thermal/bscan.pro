Function bscan, d, t, blen, eps, shape = shp, show = sho, _extra = _e

    common term_stuff, sc_ex, sc_ntr, sc_rt, $
        bc_ex, bc_eps, bc_lam, bc_ntr, bc_rt

    on_error, 1
    posib = ['circle','line']
    which = Strmatch_mm(shp,posib,2)
    if which lt 0 then message, 'Circle or line?'
 
    weps = Default(eps,1e-8)
    shofl = keyword_set(sho)
    nb = n_elements(blen)
    temp = Cast(blen,Type(d)>Type(t)>4)

    for i = 0, nb - 1 do begin
	if which eq 0 then temp[i] = $
	    CBC_temp(d,t,blen[i],weps,prec='hi',_extra=_e) $
	else temp[i] = LBC_temp(d,t,blen[i],weps,prec='hi',_extra=_e)
	if shofl then print, i, nb-1, blen[i], temp[i], $
	form = "(i4,' of ',i4,6x,f6.3,2x,f8.4)"
    endfor

    return, temp
end
