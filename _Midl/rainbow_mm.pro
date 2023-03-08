Pro Rainbow_mm, light = lit, dark = drk, shuffle = shf, show = sho

	on_error, 1

	n = 256l
	k = lindgen(n)
	km = n-1

	whi = One_of(lit,drk) > 0

	if whi then begin
		ib = [ 0.750, 0.900, 1.190, 1.345, 1.292, 1.100, 0.800, 0.510, 0.310, $
			   0.170, 0.075, 0.010, 0.000, 0.001, 0.032, 0.110, 0.195, 0.250]
		ir = [ 0.750, 0.598, 0.310, 0.140, 0.060, 0.011, 0.000, 0.020, 0.080, $
			   0.222, 0.410, 0.610, 0.770, 0.900, 0.980, 1.000, 1.002, 1.000]
		ig = 1.5 - ir - ib
	endif else begin
		ib = [ 1.000, 1.053, 1.215, 1.348, 1.302, 1.243, 1.158, 0.930, 0.672, $
			   0.415, 0.185, 0.023, 0.012, 0.108, 0.227, 0.338, 0.434, 0.500]
		ir = [ 1.000, 0.947, 0.785, 0.618, 0.430, 0.205, 0.035, 0.014, 0.197, $
			   0.470, 0.722, 0.928, 1.030, 1.044, 1.033, 1.022, 1.011, 1.000]
		ig = 2 - ir - ib
	endelse

	numv = n_elements(ig)

	ks = round(1.*(n-1)/(numv-1)*findgen(numv))
	fac = km*alog(k+sqrt(k^2+1))/alog(km+sqrt(km^2+1))

	red = byte(round(fac*Splin_eval(k,Splin_coeffs(ks,ir))))
	green = byte(round(fac*Splin_eval(k,Splin_coeffs(ks,ig))))
	blue = byte(round(fac*Splin_eval(k,Splin_coeffs(ks,ib))))

	if Isnum(shf,/int) then begin
		if n_elements(shf) eq 3 then begin
			shf = shf mod 3
			tem = [[red],[green],[blue]]
			red = tem[*,shf[0]]
			green = tem[*,shf[1]]
			blue = tem[*,shf[2]]
		endif else message, '3 entries needed'
	endif

	tvlct, red, green, blue
	if keyword_set(sho) then begin
		plvar_keep, act = 'sav'
		device, deco = 0
		arr = bytarr(2*n,2*n)
		for i = 0l, 2*n - 1 do arr[*,i] = i/2
		tvscl, arr
		plvar_keep, act = 'res'
	endif

	return
end