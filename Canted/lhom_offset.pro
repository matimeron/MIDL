Function LHOM_offset, ene, index = ind, hor_off = hof, distance = dis, $
	longitudinal = lon, alternate = alt, angle = ang

	on_error, 1
	mult = 1e3

	hof = Default(hof,1.5,/dtyp)
	dis = Default(dis,15.,/dtyp)

	tetc = Bragg_angle(ene=ene,crys='si',ind=ind,/rad,/cold)
	tetw = Bragg_angle(ene=ene,crys='si',ind=ind,/rad)
	ang = 2e6*(tetc-tetw)

	foff = (dis - hof/tan(2*tetc))*sin(2*(tetc-tetw))/sin(2*tetw)
	loff = foff*cos(2*tetc)
	toff = foff*sin(2*tetc)

	if keyword_set(alt) then begin
		tem = loff/cos(tetw)
		toff = toff - loff*tan(tetw)
		loff = tem
	endif

	if keyword_set(lon) then res = loff else res = toff

	return, mult*res
end