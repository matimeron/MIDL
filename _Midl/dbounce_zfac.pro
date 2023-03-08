Function Dbounce_zfac, d1, d2, refer_ene= rene, status = sta

	on_error, 1
	conv = float(!srcon.conv)

	lam = conv/rene
	if d2 lt d1 then message, 'd2 must be bigger than d1!'
	a1 = lam/(2*d1)
	a2 = lam/(2*d2)
	lfac = sqrt(1 - (d1/d2)^2)
	good = where(a1 le lfac,ngood,comp=bad,ncomp=nbad)

	res = 0*a1
	if ngood gt 0 then res[good] = $
	(sqrt((1- a2/a1)^2+ 2*a1*a2/(1+ sqrt(1-a1^2))))[good]
	if nbad gt 0 then res[bad] = !values.f_nan	
	sta = finite(res)

	return, res
end