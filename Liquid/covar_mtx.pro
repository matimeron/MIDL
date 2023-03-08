Function Covar_mtx, dat, velocity = vel, $
	lcount = lcon, variance = var, _extra = _e

	siz = size(dat)
	np = siz[1]
	res = make_array(np,np,type=Calctype(dat,0.))
	var = res
	lcon = lonarr(np,np)

	for i = 0l, np-1 do begin
		for j = i, np-1 do begin
			res[i,j] = 	Dat_corr(dat,i,j,vel=vel,lcou=tlcon,var=tvar,_extra=_e)
			lcon[i,j] = tlcon
			var[i,j] = tvar
		endfor
	endfor

	res = res + transpose(res)
	lcon = lcon + transpose(lcon)
	var = var + transpose(var)
	diag = (np+1)*lindgen(np)
	res[diag] = res[diag]/2
	lcon[diag] = lcon[diag]/2
	var[diag] = var[diag]/2

	return, res
end