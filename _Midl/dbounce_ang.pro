Function Dbounce_ang, ene, d1, d2, refer_ene= ren, radians= rad, degrees= deg, $
	status = sta

	on_error, 1
	if (One_of(deg,rad) > 0) eq 0 then amult = !dtor else amult = 1.
	conv = float(!srcon.conv)

	lam = conv/ren
	if lam gt 2*d1 then message, 'Reference energy must be > ' + $
	string(conv/(2*d1),form='(f6.3," kev!")')
	if d2 lt d1 then message, 'd2 must be bigger than d1!'
	a1 = lam/(2*d1)
	a2 = lam/(2*d2)
	
	kap = ene/ren
	a1k = a1/kap
	a2k = a2/kap

	res = 2/amult*(asin(a1k*sqrt(1 - a2k^2) - a2k*sqrt(1 - a1k^2)) - asin(a1))
	sta = finite(res)

	return, res
end