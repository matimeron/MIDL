Pro Gixos_ref, gdat, alpha = alp, dth = dth, ene = ene, $
	sigma = sig, temp = tmp, gamma = gam, qcrit = qcr, result = res, _extra = _e

	on_error, 1
	alp = Default(alp,0.1)
	dth = Default(dth,0.3)
	ene = Default(ene,10.)
	sig = Default(sig,2.5)
	tmp = Default(tmp,295.)
	gam = Default(gam,72.)
	qcr = Default(qcr,0.02176)
	
	qz = gdat[0,*]
	gf = Gixos_factor($
	alpha=alp,dth=dth,qz=qz,ene=ene,sigma=sig,temp=tmp,gamma=gam,qcrit=qcr)
	rf = Reflect(qz=qz,ene=ene,elem=['h','o'],wei=[2,1],/form,den=1,num=[2])
	res = Scan_scale(gdat,rf/gf)
	max = max(res[1,*])
	res = Scan_scale(res,1./max)

	Scan_show, res, xtit = 'Q!dz!n', /ylog, _extra = _e
	oplot, qz, rf, col = !pcol.pink

	return
end