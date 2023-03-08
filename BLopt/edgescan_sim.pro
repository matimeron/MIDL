Function Edgescan_sim, step, sterr, range = ran, $
	center= cen, sigma= sig, amplitude= amp, error= err, reverse= rev, pack= pac

	on_error, 1

	cen = Default(cen,0.,/dtyp)
	amp = Default(amp,1.,/dtyp)
	x = Make_grid(cen+[-ran,ran],step,/step,dim=npo)
	werr = Default(sterr,0.,/dtyp) < step/2.
	wx = x + werr*(2*randomu(s,npo[0]) - 1)
	res = sqrt(2*!pi)*sig*amp*(1 - gaussint((wx-cen)/sig))
	if keyword_set(err) then begin
		err = abs(Dif(res,/lin)*werr/step)
		err = err > 10*sqrt(Toler())*max(err)
	endif

	if keyword_set(rev) then begin
		x = reverse(cen-x) + cen
		res = reverse(res)
		if Isnum(err) then err = reverse(err)
	endif
	if keyword_set(pac) then res = Join_xy(x,res,err)

	return, FPU_fix(res)
end