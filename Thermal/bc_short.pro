Function BC_short, d, t, lam, eps, shape = shp, show = sho, _extra = _e

	on_error, 1
	posib = ['circ','line']
	sval = Strmatch_mm(shp,posib,2)
	if sval lt 0 then message, 'Circle or line?'

	nt = n_elements(t)
	typ = Calctype(d,t,lam,def=4) > 4
	weps = Default(eps,Toler(typ=typ),/dtyp)
	shofl = keyword_set(sho)

	stemp = Cast(t,Type(d)>4)
	for i = nt - 1, 0, -1 do begin
		if sval then stemp[i] = LBC_temp(d,t[i],lam,weps,_extra=_e) $
		else stemp[i] = CBC_temp(d,t[i],lam,weps,_extra=_e)
		if shofl then print, nt-i-1, nt-1, t[i], stemp[i], $
		form = "(i4,' of ',i4,6x,f6.3,2x,f8.4)"
	endfor

	return, stemp
end