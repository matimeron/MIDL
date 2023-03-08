Pro PD_tilt, snum, fnum, factor = fac, range = ran, width = wid, _extra = _e

	on_error, 1

	window, 0
	PD_sum, snum, fnum, /xy_int, /raw, res = dat, _extra = _e
	if Isnum(wid) then dat = Peak_smooth(dat,wid=wid)
	n = Split_xy(dat,x=x,y=y)
	oy = Convol_pow(y,fac,ran=ran,/inv,_extra=_e)
	window, 1
	plot, x, oy, _extra = _e

	return
end