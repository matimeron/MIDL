Pro SAXS_beam, ene, rsig = rsg, asig = asg, $
	mirloc = mil, sliloc = sll, samloc = sal, $
	focus = foc, colim = col, slope_err = sle, swid = wid, $
	show = sho, _extra = _e

	on_error, 1
	big = 2/(machar()).epsneg

	wmil = Default(mil,32.5,/dtyp)
	wsll = Default(sll,[53.5,60],/dtyp)
	if n_elements(wsll) ne 2 then message, '2 slit locations needed!'
	wsal = Default(sal,64.5,/dtyp)
	seg = [wmil,wsll,wsal] - [0,wmil,wsll]
	if min(seg) lt 0 then message, 'Inappropriate locations!'

	wwid = Default(wid,[0.20,0.21],/dtyp)/sqrt(2*!pi)
	if n_elements(wwid) ne 2 then message, '2 slit widths needed!'

	drsg = Und_beamsize(ene,/def,dis=0,ang=dasg,_extra=_e)
	wrsg = (Default(rsg,drsg,lo=5))
	wasg = (Default(asg,dasg,lo=5))

	psqi = wrsg^2*wasg^2
	qsqi = wrsg^2
	rvli = 0*wasg^2
	pqri = [psqi,qsqi,rvli]
	pqrj = PQR_prop(pqri,l=seg[0])

	if n_elements(sle) ne 0 then begin
		iflen = pqrj[[4,5]]/pqrj[[2,3]]
		if One_of(col,foc) then iflen = iflen + 1./(wsal-wmil)
		wsle = 1e-3*Default(sle,0.,/dtyp)
		pqrk = PQR_prop(pqrj,s=[0,wsle],f=[big,1]/iflen,l=seg[1])
	endif else pqrk = PQR_prop(pqrj,l=seg[1])

	pqrl = PQR_prop(pqrk,w=wwid[0],l=seg[2],tsq=tsq1)
	pqrm = PQR_prop(pqrl,w=wwid[1],l=seg[3],tsq=tsq2)

	print, '	Rad. Size 	= ', sqrt(pqrm[2:3])
	print, ' 	Ang. Size 	= ', sqrt((pqrm[0:1] + pqrm[4:5]^2)/pqrm[2:3])
	print, '	Transmit.	= ', sqrt(product(tsq1*tsq2))

	return
end