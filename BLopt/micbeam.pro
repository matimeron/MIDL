Function Micbeam, ene, mir= mir, crl= crl, sle=sle, del_foc=def, idel_foc=dei,$
	samloc= sal, slitloc= sll, aperture= ape, horizontal= hor, vertical= ver, $
	diffract= dif, optimize= opt, floptimize= flo, alternate=alt, details= det,$
	fwhm= fwhm, full=fuls, ang_res= ares, flux= flu, foc_lenghts= fol, _extra=_e

	common mic_stuff, ytit, gtit 
	on_error, 1

	typ = Calctype(ene,0.)
	nene = n_elements(ene)
	wene = Cast(ene,5)

	wha = One_of(fwhm,fuls) + 1
	facsq = [1d,alog(256d),4*!dpi]
	facun = ['sigma','fwhm','full_width']
	smult = facsq[wha]
	unam = ' ('+facun[wha]+')'
	ytit = '!7l!xm' + unam

	sol = 1.25
	if One_of(mir,crl) then begin
		optyp = 'CRL '
		opl = 46.7
		wsle = [0.,0.]
	endif else begin
		optyp = 'Mirror '
		opl = 32.5
		wsle = [0,Default(sle,1.)]*1e-3
	endelse

	sal = Default(sal,56.,/dtyp)
	if sal le opl then message, 'Sample must follow optics!'
	if Isnum(sll) then begin
		sll = Cast(sll,4)
		if sll ge opl and sll le sal then sfl = 1 else $
		message, 'Unacceptable slit location!'
	endif else sfl = 0

	gtit = [!blpar.desc + string(ene,form='(", E = ",f4.1," keV")'), $
		optyp + string(opl,form='("at ",f5.2," m")') + $
		string(sal,form='(", sample at ",f5.2," m")')]

	if Default(dif,1) then begin 
		dene = wene
		lam = 1d-4*!srcon.conv/wene
		slam = lam/(4*!dpi)
	endif else dene = []

	whi = abs(One_of(hor,ver))
	rsg = Und_beamsize(wene,/def,ang=asg,_extra=_e)
	rsg = rsg[whi,*]
	asg = asg[whi,*]
	wsle = wsle[whi]

	pqri = [rsg^2*asg^2,rsg^2,0*asg^2]
	pqrj = PQR_prop(pqri,l=opl-sol)
	pqr0 = PQR_prop(pqrj,s=wsle,rpsq= psq0,rqsq= qsq0,rr= r0)

	who = One_of(def,dei,val=val)
	l = sal-opl
	qmsq = l^2*psq0/qsq0
	dfl = keyword_set(det)
	if sfl then begin
		dl = sal - sll
		dlc = l*qmsq/(qmsq + l*slam)
		if dl lt dlc then begin
			opfl = 1
			opw = $
				sqrt((1-dl/l)^2*qmsq*dl*slam/((1-dl/l)*qmsq - dl*slam))
			opqsq1 = dl*slam/(1-dl/l)*(2 - dl*slam/((1-dl/l)*qmsq))
		endif else begin
			opfl = 0
			opw = !values.d_infinity
			opqsq1 = qmsq
		endelse

		rawf = 1./(1/l + r0/qsq0)
		if keyword_set(alt) then fopu = 0. else fopu = -dl*qsq0/(l^2*(1-dl/l))
		fopf = 1./(1/l + (r0-fopu)/qsq0)

		if keyword_set(ape) then begin
			if keyword_set(opt) then message, $
			'Either Aperture or Aperture optimization, not both!'
			w = ape/(sqrt(4*!dpi))
			sopu = dl*(1-dl/l)*psq0/w^2
			sopf = 1./(1/l + (r0-sopu)/qsq0)
			if keyword_set(flo) then u = fopu else u = sopu
			opf = 1./(1/l + (r0-u)/qsq0)
		endif else begin
			if keyword_set(opt) then begin
				if keyword_set(flo) then message, $
				'Either Aperture optimization or Flux optimization!
				if opfl then begin
					w = opw
					sopu = dl*(1-dl/l)*psq0/w^2
					opf = (sopf = 1./(1/l + (r0-sopu)/qsq0))
				endif else message, "Can't optimize"
			endif else message, 'Missing aperture size!'
		endelse
		if who eq 1 then f = opf/(1 + opf*dei) else f = opf + Default(def,0)

		if dfl then begin
			print
			print, rawf, form='("	No-slit focal length	= ",f10.4," m")'
			print, sopf, form='("	Size-opt focal length	= ",f10.4," m")'
			print, fopf, form='("	Flux-opt focal length	= ",f10.4," m")'
			print, f, form='("	Working focal length	= ",f10.4," m")
			print
		endif

		pqrk = PQR_prop(pqr0,f=f,l=l-dl)
		pqrm = PQR_prop(pqrk,ene=dene,w=w,tsq=tsq)
		pqr1 = PQR_prop(pqrm,l=dl,rpsq=psq1,rqsq=qsq1,rr=r1)

		if dfl then begin
			print
			print, l*qmsq/(qmsq + l*slam), $
				form='("	Critical delta_L 	= ",f10.4," m")
			print, sqrt(smult)*opw, unam, $
				form='("	Optimal aperture 	= ",f12.6," mm",a)
			print, sqrt(qsq1/opqsq1), $
				form='("	Size/(min_size) 	= ",f10.4)'
			print
		endif
		fol = [sopf,rawf,fopf]
		tlin = string(1e3*sqrt(4*!pi)*w,form='(f6.2," !7l!xm aperture")')
		if keyword_set(opt) then tlin = tlin + ' (optimized)'
		tlin = tlin + string(sll,form = '(" at ",f5.2," m")')
		gtit = [gtit,tlin]
	endif else begin
		opu = 0.
		opf = 1./(1/l + (r0-opu)/qsq0)
		if who eq 1 then f = opf/(1 + opf*dei) else f = opf + Default(def,0)
		u = r0 + qsq0*(1/l-1/f)
		pqr1 = PQR_prop(pqr0,f=f,l=l,rpsq=psq1,rqsq=qsq1,rr=r1)
		if dfl then begin
			print
			print, opf, form='("	Optimal focal length	= ",f10.4," m")'
			print, f, form='("	Working focal length	= ",f10.4," m")'
			print
		endif
		fol = opf
	endelse

	res = Cast(reform(sqrt(smult*qsq1)),typ,typ)
	ares = Cast(reform((sqrt(smult*Opvar_conv(pqr=pqr1)>0))[1,*]),typ,typ)
	flu = Cast(sqrt(Default(reform(tsq),replicate(1.,nene>1))),typ,typ)
	fol = Cast(fol,typ,typ)

	if dfl then begin
		print
		print, res, unam, form='("	Beam size at sample	= ",f12.6," mm",a)'
		print, ares, unam, form='("	Angular size		= ",f12.6," mr",a)'
		print, flu, form='("	Relative flux 		= ",f12.6)'
		print
	endif

	return, res
end