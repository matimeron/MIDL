Pro GIX_rsho, snum, l4 = l4, l5 = l5, s1h = s1, s1v = s1v, s4 = s4, s5 = s5, $
	energy= ene, alpha= alp, beta= bet, dth= phi, npoints= npo, _extra = _e

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

	dphi = psi
	dbet = sqrt((2*lh*sin(rbet))^2 + (cur.pdpix)^2)/(2*l5)

	qar = fltarr(3,5)
	qar[*,0] = Qvec(ralp,rbet,dth=rphi,ene=wene,/rad)
	qar[*,1] = Qvec(ralp,lbet,dth=rphi-psi,ene=wene,/rad)
	qar[*,2] = Qvec(ralp,lbet,dth=rphi+psi,ene=wene,/rad)
	qar[*,3] = Qvec(ralp,hbet,dth=rphi+psi,ene=wene,/rad)
	qar[*,4] = Qvec(ralp,hbet,dth=rphi-psi,ene=wene,/rad)

	cen = reform(qar[*,0])
	cqxy = sqrt(total(cen[0:1]^2))
	cqz = cen[2]
	ceta = 0.9*cqz^2
	cres = cqxy^(ceta-2)

	xyarr = qar[0:1,1:4]
	xran = [min(xyarr[0,*],max=max),max]
	yran = [min(xyarr[1,*],max=max),max]

	wnpo = 2l^ceil(alog(Default(npo,32))/alog(2))
	delx = (xran[1]-xran[0])/(wnpo-1)
	dely = (yran[1]-yran[0])/(wnpo-1)
	wxran = xran + delx*[-1,1]
	wyran = yran + dely*[-1,1]

	warr = Make_grid([[wxran],[wyran]],wnpo)
	k = float(2*!pi*wene/!srcon.conv)
	qx = reform(warr[0,*,*])
	qy = reform(warr[1,*,*])
	qxy = sqrt(qx^2+qy^2)
	qz = k*sin(ralp) + sqrt(((k*sin(ralp))^2-qxy^2-2*k*qy*cos(ralp))>0)
	eta = 0.9*qz^2

	inarr = 0*qxy
	for i = 0, wnpo-1 do begin
		for j = 0, wnpo-1 do begin
			inarr[i,j] = Shape_in(xyarr,warr[*,i,j],/closed)
		endfor
	endfor

	res = qxy^(eta-2)*inarr
	ares = total(res)/total(inarr)
	Display_mm, res, qx, qy, /auz
	print
	print, ares, cres, 1-cres/ares
	area = Shape_area(xyarr)
	aarea = 2*k^2*sin(2*rbet)*dbet*dphi
	print, area, aarea, 1 - aarea/area

	return
end