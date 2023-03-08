Pro Cotab

	n = 256

	ib = [ 1.  , 1.01, 1.17, 1.34, 1.34, 1.22, 0.95, 0.63, 0.47, $
		   0.42, 0.44, 0.47, 0.48, 0.41, 0.30, 0.15, 0.05,  0.0]

	ir = [1.   , 0.98, 0.82, 0.62, 0.40, 0.33, 0.33, 0.36, 0.41, $
		   0.47, 0.62, 0.88, 1.04, 1.05, 1.03, 1.02, 1.01,  1.0]

	ig = 2 - ir - ib

	l = [n_elements(ir),n_elements(ig),n_elements(ib)]
	if max(l) gt min(l) then message, 'Bad input' else numv = max(l)
	ks = round(1.*(n-1)/(numv-1)*findgen(numv))

	k = lindgen(n)
	fac = (n-1)*alog(k+1)/alog(n)

	red = fac*Splin_eval(k,Splin_coeffs(ks,ir))
	green = fac*Splin_eval(k,Splin_coeffs(ks,ig))
	blue = fac*Splin_eval(k,Splin_coeffs(ks,ib))

	tvlct, red, green, blue
	return
end

