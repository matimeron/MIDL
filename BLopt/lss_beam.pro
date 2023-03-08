Pro LSS_beam, ene, rsig = rsg, asig = asg, $
	sorloc= sol, mirloc= mil, samloc= sal, vsloc= vsl, $
	vslit_size= sli, slope_err= sle, _extra= _e

	on_error, 1
	big = 2/(machar()).epsneg

	wsol = Default(sol,1.25,/dtyp)
	wmil = Default(mil,32.5,/dtyp)
	wvsl = Default(vsl,55.0,/dtyp)
	wsal = Default(sal,56.0,/dtyp)
	seg = [wmil,wvsl,wsal] - [wsol,wmil,wvsl]
	if min(seg) lt 0 then message, 'Inappropriate locations!'

	drsg = Und_beamsize(ene,/def,dis=0,ang=dasg,_extra=_e)
	wrsg = (Default(rsg,drsg,lo=5))
	wasg = (Default(asg,dasg,lo=5))

	psqi = wrsg^2*wasg^2
	qsqi = wrsg^2
	rvli = 0*wasg^2
	pqri = [psqi,qsqi,rvli]
	pqrj = PQR_prop(pqri,l=seg[0])

	iflen = pqrj[[4,5]]/pqrj[[2,3]] + 1/(seg[1]+seg[2])
	wsle = 1e-3*Default(sle,0.,/dtyp)
	pqrk= PQR_prop(pqrj,s=[0,wsle],f=[big,1]/iflen,l=seg[1])

	if Isnum(sli) then pqrm = PQR_prop(pqrk,w=[big,sli],l=seg[2],ene=ene) $
	else pqrm = PQR_prop(pqrk,l=seg[2])

	print, '	Rad. Size 	= ', sqrt(pqrm[2:3])
	print, ' 	Ang. Size 	= ', sqrt((pqrm[0:1] + pqrm[4:5]^2)/pqrm[2:3])

	return
end