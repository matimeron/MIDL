Function Grav_serr, len, sig, thickness = thi

	on_error, 1

	lg = 1.14e7
	w = len^2/(8*sig^2)
	fun = $
	(15*Igamma_mm(w,3.5)-9*Igamma_mm(w,2.5)^2/Igamma_mm(w,1.5))/Igamma_mm(w,0.5)
	res = 4*sig^3/(lg*thi^2)*sqrt(fun)

	return, res
end