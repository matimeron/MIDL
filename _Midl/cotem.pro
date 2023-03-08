Pro Cotem, ir, ig, ib, asinh = asn, go = go

	n = 256l
	k = lindgen(n)
	km = n-1

	l = [n_elements(ir),n_elements(ig),n_elements(ib)]
	if max(l) gt min(l) then message, 'Bad input' else numv = max(l)

	ks = round(1.*(n-1)/(numv-1)*findgen(numv))

;	red = sqrt(k*(n-1))*Splin_eval(k,Splin_coeffs(ks,ir))
;	green = sqrt(k*(n-1))*Splin_eval(k,Splin_coeffs(ks,ig))
;	blue = sqrt(k*(n-1))*Splin_eval(k,Splin_coeffs(ks,ib))

	if keyword_set(asn) then fac= km*alog(k+sqrt(k^2+1))/alog(km+sqrt(km^2+1))$
	else fac = km*alog(k+1)/alog(n)

	red   = byte(fac*Splin_eval(k,Splin_coeffs(ks,ir)))
	green = byte(fac*Splin_eval(k,Splin_coeffs(ks,ig)))
	blue  = byte(fac*Splin_eval(k,Splin_coeffs(ks,ib)))
	plotot, k, [[replicate(n-1,n)],[red],[green],[blue],[replicate(0,n)]], $
	color=[0,!pcol.red,!pcol.green,!pcol.blue,0], line=[0,0,0,0,0]

	print, max(red), max(green), max(blue)
	print, min(red), min(green), min(blue)
	if keyword_set(go) then tvlct, red, green, blue
	return
end

