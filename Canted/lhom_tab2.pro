Pro LHOM_tab2, ene, index = ind, hor_off = hof, locations = loc, fixed = fix, $
	angle = ang

	on_error, 1
	mult = 1e3

	hof = Default(hof,1.5,/dtyp)
	locnam = ['CRL','B1 ','B2 ']
	aloc = [27.8,0]

	loc = Default(loc,[33.6, 42.9, 49.8])
	if n_elements(loc) eq 3 then wloc = transpose([[loc],[hof,hof,hof]]) $
	else message, 'LOCATIONS needs 3 elements!'

	if Type(fix) eq 7  then lind = Strmatch_mm(fix,locnam,2) $
	else lind = Default(fix,0,/dtyp)
	if lind ge 0 and lind le 2 then dloc = wloc[*,lind] $
	else message, 'Nonexistant location!'

	tetc = Bragg_angle(ene=ene,crys='si',ind=ind,/rad,/cold)
	tetw = Bragg_angle(ene=ene,crys='si',ind=ind,/rad)
	ang = 2e6*(tetc-tetw)

	cloc = aloc + hof*[1/tan(2*tetc),1]
	foff = (dloc - cloc)[0]*sin(2*(tetc-tetw))/sin(2*tetw)
	bloc = cloc - foff*[cos(2*tetc),sin(2*tetc)]
	boff = mult*(bloc[1] - hof)
	rboff = boff + mult*foff*cos(2*tetc)*tan(tetw)

;	cploc = [bloc[0], cloc[1] - foff*tan(tetw)]
;	eloc = dloc + (sdis-dloc[0])*[1,tan(2*(tetc-tetw))]
;	loff = foff*cos(2*tetc)
;	toff = foff*sin(2*tetc)

	owloc = wloc
	for i = 0, 2 do  owloc[1,i] = dloc[1]+(wloc[0,i]-dloc[0])*tan(2*(tetc-tetw))
	offs = mult*(owloc[1,*] - hof)
	print
	print, 'Teta_c', !radeg*tetc, form = '("	",a,"	= ",f7.4," deg.")
	print, 'Teta_w', !radeg*tetw, form = '("	",a,"	= ",f7.4," deg.")
	print, 'Angle offset', ang, form = '("	",a,"	= ",f7.2," microrad.")
	print
	print, 'B', bloc, form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, 'CRL', owloc[*,0], form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, "B1", owloc[*,1], form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print, 'B2', owloc[*,2], form = '("	",a,"	= (",f7.4,", ",f7.4,") m")'
	print
	print, 'B offset', boff, form = '("	",a,"	= ",f6.3," mm")'
	print, 'B offset, red.', rboff, form = '("	",a,"	= ",f6.3," mm")'
	print
	print, 'CRL offset', offs[0], form = '("	",a,"	= ",f6.3," mm")'
	print, 'B1 offset', offs[1], form = '("	",a,"	= ",f6.3," mm")'
	print, 'B2 offset', offs[2], form = '("	",a,"	= ",f6.3," mm")'
	return
end