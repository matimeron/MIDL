Pro Philcheck, gap, period, energy = ene, mm = mm, apperture= app, table= tab, $
	_extra = _e

	on_error, 1

	e_ring = 7.
	cur = 0.1
	dlen = 2.4
	dist = 25.
	sapp = [3.2,2.3]
	rgam = 1e6*e_ring/(!srcon.ee*!srcon.scal)
	eturns = 1e3*cur*!srcon.scal

	if keyword_set(mm) then mmult = 1e-3 else mmult = 1.
	if n_elements(gap) eq 1 then wgap = mmult*gap $
	else message, 'One and only one gap value must be given!'
	np = n_elements(period)
	if np ge 1 then wper = mmult*period else message, 'Missing period value(s)!'
	nper = floor(dlen/wper)

	etop = 2*rgam^2*!srcon.hc*!srcon.scal/wper
	kmax = wper*Idfield(wgap,wper,_extra=_e)/(2*!pi*!srcon.bere)
	ebot = etop/(1 + kmax^2/2)

	tit = 'First harmonic total flux, ' + string(dlen,gap,ene, $
	form = '(f3.1," m ID, gap = ",f4.1," mm, E_1 = ",f4.1," keV")')

	val = where(ene ge ebot and ene lt etop, nval)
	if nval gt 0 then begin
		kval = sqrt(2*(etop[val]/ene - 1))
		flux = !pi/3*!srcon.alp*dlen/wper[val]*kval^2/(1 + kval^2/2)*eturns
		plot, period[val], flux, xtit = 'period (mm)' , ytit = 'flux (ph/s)',$
		tit = tit, _extra = _e
		if keyword_set(app) then begin
			xapp = 1e3*2*dist/rgam
			yapp = kval*xapp
			afac = product(sapp)/sqrt((sapp[0]^2+xapp^2)*(sapp[1]^2 + yapp^2))
			oplot, period[val], afac*flux, line = 2
		endif else afac = 1
	endif

	if keyword_set(tab) then begin
		nper = 2*floor(dlen/wper[val]/2)
		head = ['Per.(mm)', '# of per.', 'E_min(keV)', 'E_max(keV)', $
				'Tot. Flux(ph/s)', 'Net Flux(ph/s)']
		form = ['f5.2','i3','f5.2','f5.2','g9.3','g9.3']
		print
		Tabulate, period[val], nper, ebot[val], etop[val], flux, afac*flux, $
		tit = tit, head = head, form = form
	endif

	return
end