Pro GIX_res, snum, l4 = l4, l5 = l5, s1h = s1, s1v = s1v, s4 = s4, s5 = s5, $
	energy = ene, alpha= alp, beta = bet, dth = phi, scale = scl, _extra = _e

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if Isnum(snum) then begin
		Spec_file_check, snum, /sing, /pildet		
		cur = fildat.scan[snum]
		l5 = Default(l5,cur.pdist,/dtyp)
		l4 = Default(l4,l5 - cur.pdsdist,/dtyp)
		s1v = Default(s1v,mean(cur.sl_val[0:1,1]),/dtyp)
		s1 = Default(s1,mean(cur.sl_val[2:3,1]),/dtyp)
		s4 = Default(s4,mean(cur.sl_val[2:3,4]),/dtyp)
		s5 = Default(s5,mean(cur.sl_val[2:3,5]),/dtyp)
		wene = Default(ene,float(!srcon.conv/cur.lambda),/dtyp)
		ralp = !dtor*Default(alp,(cur.angs)[0],/dtyp)
		rbet = !dtor*Default(bet,(cur.angs)[1],/dtyp)
		rphi = !dtor*Default(phi,(cur.angs)[2],/dtyp)
	endif

	lh = s1v/sin(ralp)
	dv = (l4*s5 + l5*s4)/sqrt((l5-l4)^2 + (s4+s5)^2)
	psi = atan(s4+s5,l5-l4)
	pixan = atan(cur.pdpix,2*l5)
	lbet = atan(l5*sin(rbet),l5*cos(rbet)+lh)
	hbet = atan(l5*sin(rbet),l5*cos(rbet)-lh)
	pixan = atan(cur.pdpix,2*l5)
	lbet = rbet - sqrt((rbet-lbet)^2 + pixan^2)
	hbet = rbet + sqrt((hbet-rbet)^2 + pixan^2)

	qar = fltarr(3,5)
	qar[*,0] = Qvec(ralp,rbet,dth=rphi,ene=wene,/rad)
	qar[*,1] = Qvec(ralp,lbet,dth=rphi-psi,ene=wene,/rad)
	qar[*,2] = Qvec(ralp,lbet,dth=rphi+psi,ene=wene,/rad)
	qar[*,3] = Qvec(ralp,hbet,dth=rphi+psi,ene=wene,/rad)
	qar[*,4] = Qvec(ralp,hbet,dth=rphi-psi,ene=wene,/rad)

	xycen = qar[0:1,0]
	xyarr = qar[0:1,1:4]
	dx = max(xyarr[0,*],min=min) - min
	dy = max(xyarr[1,*],min=min) - min
	if keyword_set(scl) then begin
		if dx gt dy then tru = 'ycen' else tru = 'xcen'
	endif else tru = !null
	window, 0
	box, xycen[0]+dx*[-1,1], xycen[1]+dy*[-1,1], tru=tru, /bor, ticklen=-0.01,$
	xmar=[10,2], ymar=[4,2], xtit = 'Q!dx!n', ytit = 'Q!dy!n'
	plots, Shape_close(xyarr), thi=2, col = !pcol.green
	plots, xycen,psym=-7, thi=2, col=!pcol.red

	aqar = fltarr(2,5)
	aqar[0,*] = sqrt(total(qar[0:1,*]^2,1))
	aqar[1,*] = qar[2,*]
	xycen = aqar[0:1,0]
	xyarr = aqar[0:1,1:4]
	dx = max(xyarr[0,*],min=min) - min
	dy = max(xyarr[1,*],min=min) - min
	if keyword_set(scl) then begin
		if dx gt dy then tru = 'ycen' else tru = 'xcen'
	endif else tru = !null
	window, 1
	box, xycen[0]+dx*[-1,1], xycen[1]+dy*[-1,1], tru=tru, /bor, ticklen=-0.01,$
	xmar=[10,2], ymar=[4,2], xtit = 'Q!dxy!n', ytit = 'Q!dz!n'
	plots, Shape_close(xyarr), thi=2, col = !pcol.dgreen
	plots, xycen,psym=-7, thi=2, col=!pcol.dred

	return
end