Pro Scatcheck, distance, radius, theta = tet, phi = phi, radians = rad

	on_error, 1

	eps = Default(radius,0.,/dtyp)/distance
	if keyword_set(rad) then mult = 1. else mult = !dtor
	if Codims(tet,phi) then begin
		wtet = mult*tet
		wphi = mult*phi
	endif else message, 'Inconsistent angle input dimensions!'

	p0 = 2*sin(wtet)*sin(wphi)
	q0 = 2*sin(wtet)*cos(wphi)

	x = p0*(q0+eps)/(2-q0^2)
	z = q0*(1+eps*q0/2)*sqrt(4-p0^2-q0^2)/(2-q0^2)

	print
	print, x, z
	print

	s = sqrt(x^2+z^2)
	p = sqrt(2*x^2/s^2*(1+1/sqrt(1+s^2)))
	q = sqrt(2*s^2/(1+s^2+sqrt(1+s^2)))

	a = 2-q^2
	b = 2+q^2
	c = 4-p^2-q^2

	mat = [[a*q,b*p],[-a*p*q/c,b-a*q^2/c]]
	vec = eps*a*[p,q^2/2]
	corr = Solve_linsys(mat,vec,/row,/svd)
	amat = [[q,p],[-p*q,4]]
	avec = eps*[p,2*q^2]
	corr = Solve_linsys(mat,vec,/row,/svd)
	acorr = Solve_linsys(amat,avec,/row,/svd)

	print
	print, p0, q0
	print, p, q
	print
	print, p - corr[0], q - corr[1]
	print, p - acorr[0], q - acorr[1]
	print, p - eps*p/q, q - eps*[2*q^2+p^2]/4
	print, p - 2*eps*x/(x^2+z^2), q - eps/(2*(x^2+z^2))*(2*x^2+ (x^2+z^2)^2)

	return
end
