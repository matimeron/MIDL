Function BC_etemp, dims, pow, kappa = kap, hval = hvl, thick = thi, $
	xangle = xang, yangle = yang, radians= rad, _extra = _e

	on_error, 1

	co1  = [  -0.784648,  0.459624,   0.0247313]
	lco1 = [-0.00910018, -0.213418, -0.00333917]
	co2  = [   0.225142, -0.176720,  -0.0504505]
	lco2 = [ 0.00616622,  0.160741,   0.0116233]

	pow = Default(pow,1.,/dtyp)

	case n_elements(dims) of
		1	:	wdims = Cast([dims,dims],4)
		2	:	wdims = Cast(dims,4)
		else:	message, 'Bad or missing size input!'
	endcase

	if keyword_set(rad) then mult = 1. else mult = !dtor
	sca = [1.,1.]
	if n_elements(xang) eq 1 then $
	if xang gt 0 then sca[0] = 1/sin(mult*xang) else message, 'Bad X-angle!'
	if n_elements(yang) eq 1 then $
	if yang gt 0 then sca[1] = 1/sin(mult*yang) else message, 'Bad Y-angle!'

	rads = dims*sca/2
	if max(rads) gt 2*thi then message, "Ain't thick plate no more", /cont

	lam = 1.*kap/(hvl*thi)
	ilam = 1/lam
	coefs = [1, $
			Poleval(ilam,co1) + alog(ilam)*Poleval(ilam,lco1),$
			Poleval(ilam,co2) + alog(ilam)*Poleval(ilam,lco2)]

	res = 2*thi*pow/(!pi*kap*rads[0]*rads[1])*$
	2/!pi*Romberg('BC_efun',[0,!pi/2],rel,par=[rads,thi,coefs],stat=stat)

	return, res
end