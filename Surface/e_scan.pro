Pro E_scan, eran = ern, step = stp, alpha = alp, qz = qz, linear = lin

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1

	tpos = ['Alpha','Qz']
	hpos = ['Qz (1/A)','Alpha (deg)']
	angs = ['phi','chi','ttet']

	e = Make_grid(ern,stp,/step)
	phi = Bragg_angle(ener=e,crys='si',ind=[1,1,1],/unc,/rad)
	lam = conv/e
	whi = One_of(alp,qz,val=inp)
	if whi eq 0 then begin
	 	salp = sin(!dtor*alp)
		qz = 4*!pi/lam*salp
		sho = qz
	endif else begin
		salp = lam/(4*!pi)*qz
		alp = !radeg*asin(salp)
		sho = alp
	endelse

	chi = asin(salp/sin(2*phi))
	ttet = atan(cos(chi)*tan(2*phi))

	dphi = !radeg*phi
	dchi = !radeg*chi
	dttet = !radeg*ttet
	print
	tit = 'E_scan parameters for ' + tpos(whi) + $
	' = ' + string(inp,form = '(f7.4)')
	head = ['Energy (keV)', 'Lambda (A)', hpos(whi), $
			'Phi (deg)', 'Chi (deg)', '2Theta (deg)']
	Tabulate, e, lam, sho, dphi, dchi, dttet, $
		tit=tit,head=head, form = replicate('f7.4',6)

	if Type(lin) eq 7 then begin
		wha = Strmatch_mm(lin,angs)
		case wha of
			0	:	wang = dphi
			1	:	wang = dchi
			2	:	wang = dttet
			else:	message, 'No such angle!'
		endcase
		cof = Linfit_mm(e,wang)
		print
		print, '	Fit coefficients for ' + angs[wha] + ' are: ', cof
		plot, e, Poleval(e,cof) - wang, xtit = 'E (keV)', $
		ytit = 'Delta_' + angs[wha] + ' (linearized - exact)
	endif

	return
end