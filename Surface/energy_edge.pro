Function Energy_edge, snum, col = col, error = err, _extra=_e

	on_error, 1
	dcol = [0,-1,-2]

	if Scan_ver(snum) then dat = snum $
	else dat = Scan_scale(Scan_read(snum,col=Default(col,dcol)),-1)
	wha = Scan_ver(dat)
	ddat = Scan_der(dat,par=(Scan_ver(dat) eq 3))
	dum = max(ddat[1,*],mloc)
	fres = Peak_fit(ddat,back=3,cen=ddat[0,mloc],/show,den=3,err=ferr,_extra=_e)
	err = ferr[4]

	return, fres[4]
end