Function UND_int, erat, k=k, gtet=gte, phi=phi, nper=nper, taper=tap, $ 
	accuracy = acc, _extra=_e

	on_error, 1

	posib = ['lo','med','hi']
	amult = [2l,4l,8l]
	whi = abs(Strmatch_mm(acc,posib,2))

	gte = Default(gte,0d,/dtyp)
	phi = Default(phi,0d,/dtyp)
	check = Codims(k,gte,phi,dim=dim,same=sam)
	if sam and (dim eq 0) then begin
		typ = Calctype(erat,k,0.)
		nen = n_elements(erat)
		res = dblarr(nen)
		wk = Cast(k,5)
		fac0 = 1 + wk^2/2
		stet = gte/(wk > Toler())
		fac = fac0 + (wk*stet)^2
		u = wk^2/(4*fac)
		v = 8*u*stet*cos(phi)
		srat = erat*fac/fac0
		t = Default(tap,0d,/dtyp)/(2*!dpi*nper)
		ran = [0d,2*!dpi*nper]
		nden = amult(whi)*(ceil(max(srat)) + 1) + 1
		s = Make_grid(ran,nper*nden+1)

		ots = 1 - t*s
		phas = (s - ots*u*(ots*sin(2*s) - t*cos(2*s)) + $
			v*(ots*cos(s) + t*sin(s)) - 2*u*t*s^2*(2 + ots)/3)#srat
		f = exp(dcomplex(0,1)*phas)
		for i = 0, nen-1 do begin
			fint = Integ(s,ots*sin(s)*f[*,i],/val)
			sint = Integ(s,f[*,i],/val)
			vres = ([fint,0] - sint*stet*[cos(phi),sin(phi)])/(!dpi*nper)
			res[i] = total(Abs_mm(vres)^2)
		endfor
		res = erat^2*res
	endif else message, 'K, GTE and PHI must be scalars!'

	return, Cast(res,typ,typ,/fix)
end