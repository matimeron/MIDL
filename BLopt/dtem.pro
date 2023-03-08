Function Dtem, ene, ele = ele, rad = rad, rof = rof, approx = ares

	on_error, 1

	rof = Default(rof,3000., /dtyp)

	ele = Default(ele,'Al',/dtyp)
	rad = 1e-3*Default(rad,0.2,/dtyp)
	rof = Default(rof,3000.,/dtyp)

	k = 2*!pi*ene/float(!srcon.conv)
	if Streq(ele,'C') then die = Dielect(ene,elem=ele,den=3.5156) $
	else die = Dielect(ene,elem=ele)
	alp = Real_mm(die)
	mu = -1e10*k*Imaginary_mm(die)

	ni = (k*rof*alp/2)^2
	res = rad*alp/2/(rad*mu + ni)
	ares = alp/(2*mu)

	return, res
end