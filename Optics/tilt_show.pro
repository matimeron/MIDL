Pro Tilt_show, theta, lam, d, dz, compare = comp, _extra = _e

	on_error, 1

	kd = 2e7*!pi*d/lam

	tilt = atan(dz,d)

	plot, theta, Tilt_slit(kd,theta,tilt), _extra = _e, /nodata
	oplot, theta, Tilt_slit(kd,theta,tilt), _extra = _e, col = !pcol.red
	if keyword_set(comp) then oplot, theta, Sp_beselj(kd*sin(theta/2),0)^2, $
	col = !pcol.green, line = 2

	return
end