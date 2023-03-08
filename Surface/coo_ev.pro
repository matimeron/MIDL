Function Coo_ev, x, z, alpha= alp, beta= bet, phi= phi, dist= dst, pdist= pdst,$
	qvals= qvl, energy= ene, radians= rad, xreverse= xrv, show= sho, _extra= _e

	on_error, 1
	if keyword_set(rad) then amult = 1. else amult = !dtor
	if keyword_set(xrv) then xmult = -1 else xmult = 1

	nx = n_elements(x)
	nz = n_elements(z)
	minxz = min([nx,nz],max=maxxz)
	if minxz gt 0 then begin
		ux = xmult*Cast(x,4)
		uz = Cast(z,4)
		if minxz eq 1 then begin
			if nx eq 1 then ux = replicate(ux,nz) else uz = replicate(uz,nx)
		endif else if minxz ne maxxz then message, 'Bad input!'
	endif else message, 'Missing input(s)!'
	res = fltarr(2,maxxz)

	rphi = amult*phi
	rbet = amult*bet
	pdst = Default(pdst,dst,lo=4)
	if pdst le 0 then pdst = Cast(dst,4)
	uy = replicate(pdst,maxxz)
	if pdst ne dst then uz = uz - (dst-pdst)* $
	(uz*sin(rphi) + ux*cos(rphi)*sin(rbet))/$
	(dst*sin(rphi) - ux*cos(rphi)*cos(rbet))

	vx = ux
	vy = uy*cos(rbet) - uz*sin(rbet)
	vz = uy*sin(rbet) + uz*cos(rbet)

	res[0,*] = rphi - atan(vx,vy)
	res[1,*] = atan(vz,sqrt(vx^2 + vy^2))

	if keyword_set(qvl) then begin
		ralp = amult*alp
		apb = (ralp + res[1,*])/2
		amb = (ralp - res[1,*])/2
		res[0,*] = $
		sqrt((sin(apb)*sin(amb))^2 + cos(apb+amb)*cos(apb-amb)*sin(res[0,*]/2))
		res[1,*] = 2*sin(apb)*cos(amb)
		k = float(2*!pi*Default(ene,10)/!srcon.conv)
		res = k*res
	endif else res = res/amult
	if keyword_set(sho) then begin
		Scan_show, res, _extra = _e
		plots, res[*,0], psym=8, col=!pcol.green, symsize=0.5
		plots, res[*,-1], psym=8, col=!pcol.red, symsize=0.5
	endif

	return, res
end	