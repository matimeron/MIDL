Function UQ_ang, hq, vq, alp= alp, lam= lam, energy= ene, k= kvl, uvec= uvc, $
	radians = rad, pack = pac

	on_error, 1
	eps = Toler()
	thre = 1 + 4*eps

	typ = Calctype(hq,vq,0.,def=4)
	Input_reform, hq, vq, whq=whq, wvq=wvq, type=5, dim=dim

	if keyword_set(rad) then mult = 1d else mult = !dpi/180d
	walp = mult*alp
	if size(walp,/dim) ne 0 then message, 'ALPHA must be a scalar!'

	k = K_gen(lam=lam, energy=ene, k=kvl, typ=5,/sing)

	if n_elements(uvc) eq 3 then begin 
		arg = wvq/k - sin(walp)
		if max(abs(arg)) le thre then wbet = asin(-1 > arg < 1) $
		else message, 'Unacceptable values!'
		arg = sqrt(((whq/(2*k))^2 - (sin((wbet+walp)/2)*sin((wbet-walp)/2))^2)/$
			(cos(walp)*cos(wbet)) > 0)
		if max(arg) le thre then wdth = 2*asin(arg < 1) $
		else message, 'Unacceptable values!'
		norm = 2*sqrt(sin((wbet+walp)/2)^2 + cos(walp)*cos(wbet)*sin(wdth/2)^2)
		qsq0 = - cos(wbet)*sin(wdth)
		qsq1 = cos(wbet)*cos(wdth) - cos(walp)
		qsq2 = sin(walp) + sin(wbet)
		uuv = Univec(uvc)
		arg = (uuv[0]*qsq0 + uuv[1]*qsq1 + uuv[2]*qsq2)/(norm > eps)
		if typ lt 5 then arg = Fltround(arg,dec_dig=7)
		if max(abs(arg)) le thre then arg = -1 > arg < 1 $
		else message, 'Unacceptable values!'

		if keyword_set(pac) and n_elements(dim) eq 2 then begin
			res = make_array([4,dim],typ=5)
			res[0,*,*] = whq
			res[1,*,*] = wvq
			res[2,*,*] = acos(arg)/mult
		endif else res = acos(arg)/mult
	endif else message, 'UVEC must be a 3-element vector!'

	return, Cast(res,typ,typ,/fix)
end