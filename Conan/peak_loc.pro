Function Peak_loc, x, sdy, ran, width = wid, sigma = sig, amplitude = amp, $
	status = stat

	on_error, 1
	doub = Isnum(x,/doub) or Isnum(y,/doub)
	sinf = machar(double = doub)
	erv = sinf.xmax

	stat = 0
	loc = erv
	sig = erv
	amp = erv

	n = n_elements(x)
	if n_elements(sdy) ne n then message, 'Size mismatch!'
	dx = 1.*(max(x,min=min) - min)/(n-1)
	ran = 0 > Default(ran,[0l,n-1],/dtyp) < (n-1)
	if n_elements(ran) ne 2 then message, 'Range needs two elements!'
	wwid = Default(wid,3,/dtyp) > 3
	wwid = wwid/2*2+1

	t = x[ran[0]:ran[1]]
	f = sdy[ran[0]:ran[1]]
	spc2 = Splin_coeffs(t,f)
	spc3 = Splin_coeffs(t,Splin_eval(t,spc2,der=1))
	zcr = Splinroot(spc3,x[ran],1e2*sinf.eps,/rel,multi=3,stat=s)

	if Arreq(s,[1,1,1]) then begin
		cc = exp(-1.5)
		wcor = (3*wwid^2 - 13)/168
		exf = Splin_eval(zcr,spc2)
		sdif = exf[0]+exf[2]-2*exf[1]
		loc = (zcr[1] + cc*(zcr[0] + zcr[2]))/(1 + 2*cc)
		sig0 = (zcr[2] - zcr[0])/sqrt(12)
		sig = 0.5*(sig0 + sqrt((sig0^2 - 4*wcor*dx^2)>0))
		amp = (sig/dx)^2*sig/(4*sig-3*sig0)*sdif/(2*(1+2*cc))
		stat = 1
	endif else stat = 0

	return, loc
end