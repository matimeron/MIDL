Function Tilt_slit, kd, theta, tilt, degrees = deg

	on_error, 1

	if keyword_set(deg) then amul = !dtor else amult = 1.
	if n_elements(kd) ne 1 then message, 'kd must be a scalar!'
	tilt = Default(tilt,0.)
	if n_elements(tilt) ne 1 then message, 'Tilt must be a scalar!'
	wtheth = amult*theta/2
	wtilt = amult*tilt
	gen = cos(wtilt - wtheth)/cos(wtilt)
	coeff = gen*cos(wtheth)
	arg = kd*gen*sin(wtheth)

	return, FPU_fix((coeff*Sp_beselj(arg,0))^2)
end