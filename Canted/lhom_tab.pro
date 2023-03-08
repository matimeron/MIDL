Pro LHOM_tab, ene, index = ind, hor_off = hof, distance = dis, sdistance= sdis,$
	 angle = ang

	on_error, 1
	mult = 1e3

	hof = Default(hof,1.5,/dtyp)
	dis = Default(dis,5.8,/dtyp)

	aloc = [27.8,0]
	dloc = aloc + [dis,hof]

	tetc = Bragg_angle(ene=ene,crys='si',ind=ind,/rad,/cold)
	tetw = Bragg_angle(ene=ene,crys='si',ind=ind,/rad)
	ang = 2e6*(tetc-tetw)

	cloc = aloc + hof*[1/tan(2*tetc),1]
	foff = (dloc - cloc)[0]*sin(2*(tetc-tetw))/sin(2*tetw)
	bloc = cloc - foff*[cos(2*tetc),sin(2*tetc)]
	cploc = [bloc[0], cloc[1] - foff*tan(tetw)]
	eloc = dloc + (sdis-dloc[0])*[1,tan(2*(tetc-tetw))]

;	loff = foff*cos(2*tetc)
;	toff = foff*sin(2*tetc)

	print
	print, 'Teta_c', !radeg*tetc, form = '("	",a,"	= ",f7.4," deg.")
	print, 'Teta_w', !radeg*tetw, form = '("	",a,"	= ",f7.4," deg.")
	print
	print, 'A', aloc, form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, 'B', bloc, form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, 'C', cloc, form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, "C'", cploc, form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, 'D', dloc, form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, 'E', eloc, form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'

	return
end