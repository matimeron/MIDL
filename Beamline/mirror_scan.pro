Function Mirror_scan, snum, theta = tet, mir_loc = mlc, det_loc = dlc, $
	first = fir, second = sec, com = com, peak = pea, slope = slp, shape= shp,$
	refine = ref, amp = amp, full = ful, show = sho, fit = fit, _extra = _e

	on_error, 1

	whilis = ['Mir1Y','Mir2Y']
	whalis = ['Com','Peak_loc']
	mirlocs = [32.2,32.9]

	nsc = Scan_list_ver(snum,/check,lis=lis)	
	whi = One_of(fir,sec) > 0
	wha = One_of(com,pea) > 0
	sgn = 2*whi-1
	dis = dlc - Default(mlc,mirlocs[whi])

	ym = Scan_par_read(lis,whilis[whi])
	if keyword_set(ref) then begin
		yd = 0*ym
		for i = 0, nsc-1 do yd[i] = $
		Peak_cent(Scan_read(lis[i],col=[0,-1,6]),amp=amp,/aug)
	endif else yd = Scan_field_read(lis,whalis[wha])
	s = sort(ym)
	sres = Scan_prune(transpose([[ym[s]],[yd[s]]]),/part,net=nsc)
	asres = total(sres,2)/nsc
	wym = reform(sres[0,*] - asres[0])
	wyd = reform(sres[1,*] - asres[1])

	coe = Linfit_mm(wym,wyd)
	ffit = Poleval(wym,coe)
	if coe[1] eq 2 then rad = (machar()).xmax $
	else rad = 2*sgn*dis/(tet*(2-coe[1]))
	ni = (Poleval(wym,coe) - wyd)/(2e3*dis)
	slerr = sqrt((1 - 1./nsc)/(max(wym,min=min)-min)*Integ(wym,(1e6*ni)^2,/val))

	print
	print, rad, form = '("	Radius		= ",g10.3," km")'
	print, slerr, form = '("	Slope error	= ",g10.3," microrad")'
	print

	if keyword_set(ful) then begin
		coe[1] = 2
		ni = (Poleval(wym,coe) - wyd)/(2e3*dis)
		fulfl = 1
	endif else fulfl = 0

	res = 0*sres
	res[0,*] = sgn*1e3*wym/tet
	who = One_of(slp,shp) > 0
	if who then res[1,*] = Integ(reform(res[0,*]),ni) else res[1,*] = ni
	if sgn le 0 then res = reverse(res,2) 

	cwin = !d.window
	if keyword_set(sho) then begin
		window, 0
		if who then begin
			sfac = 1e3
			tit = 'Mirror shape'
			ytit = '!7l!xm'
		endif else begin
			sfac = 1e6
			tit = 'Mirror slope'
			ytit = '!7l!xrad'
		endelse
		if not fulfl then tit = tit + ' (global curvature subtracted)'
		Scan_show, Scan_scale(res,sfac,/part), xtit='mm', ytit=ytit, tit=tit,$
		_extra=_e
	endif
	if keyword_set(fit) then begin
		window, 1
		plot, wym, wyd, psym=8
		oplot, wym, ffit
	endif
	if cwin ge 0 then wset, cwin

	return, res
end