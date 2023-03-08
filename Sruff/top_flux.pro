Pro Top_flux, eran, estep, period= per, bandwidth= ban, emin= emi, kmax= kma, $
	def = def, scu = scu, nper = npr, und_length = udl, min_gap= mgp, $
	ring_energy=ren, rgam=rgm, current=cur, radial_size=rsg, angular_size=asg, $
	correct= crr, optimize= opt,  aperture= ape, distance= dst, coherent= coh, $
	progress = prg, result = res, _extra = _e

;+
; NAME:
;		TOP_FLUX
; VERSION:
;		8.72
; PURPOSE:
;		Calculates the flux of a synchrotron undulator source over a broad 
;		energy range.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		TOP_FLUX, ERAN [,ESTEP] , PERIOD = PER [, keywords]
; INPUTS:
;	ERAN
;		Photon energy range, in keV.  Can be given as:
;			1) 	Scalar.  In this case the value is assumed to be the top of the
;				range (in keV), with 0 being the bottom.
;			2)  A 2 element vector, taken as a [bottom,top] range.
; OPTIONAL INPUT PARAMETERS:
;	ESTEP
;		Scalar, the step between consecutive energy values in ERAN.  Defaults
;		to 0.5 keV.
; KEYWORD PARAMETERS:
;	PERIOD
;		Scalar, the length of the undulator period.  If the value is less then
;		1, length in meters is assumed, else in milimeters.  Mandatory, no 
;		defaults.
;	BANDWIDTH
;		The bandwidth used in calculation.  Default is 1e-3.
;	EMIN										|
;		Scalar, minimal first harmonic energy.	|	No more than one of these
;	KMAX										|	two keywords may be used.
;		Scalar, maximal K value to use			|
;
;		Note:	By default, minimal energy or maximal K (these are exchangeable
;				is set by the value of the undulator's minimal gap, from !BLPAR.
;				If, instead, EMIN or KMAX is given, it overrides the default 
;				value, provided the explicit value falls within the range
;				allowed for the first harmonic.  If not, it is ignored.
;	/DEF
;		Switch.  Set by default, can be disabled by explicitly setting DEF = 0.
;		When DEF is set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
;	NPER
;		Number of periods.  If not given, calculated	|
;		from period(s) and UND_LENGTH.					|	At most one of
;	UND_LENGTH											|	these two may
;		The length of the undulator in meters.  If		|	be given.
;		needed and not given, replaced by the default 	|
;		value in !BLPAR. 		|
;	MIN_GAP
;		Minimal undulator gap, in mm.  If not given, but /DEF is set, the
;		default value from !BLPAR is used.
;	RING_ENERGY									|	At most one of these two may
;		Synchrotron energy, in GeV.   			|	be given.  If none is and
;	RGAM										|	/DEF is set, value is taken
;		The relativistic Gamma of the source.	|	from !BLPAR.
;	CURRENT
;		Source electron current, in Amp.  If not given and /DEF is set, value
;		is taken from !BLPAR.
;	RADIAL_SIZE
;		Source spatial size in the format of [horizontal_sigma,vertical_sigma].
;		If not given and /DEF is set, values are taken from !BLPAR.
;	ANGULAR_SIZE
;		Source angular size in the format of [horizontal_sigma,vertical_sigma].
;		If not given and /DEF is set, values are taken from !BLPAR.
;	/CORRECT
;		Switch.  If set, higher order corrections to radiative spatial and
;		angular width are calculated.  This has a very small effect, except for
;		very high harmonics.
;	/OPTIMIZE
;		Switch.  If set, the flux at E is calculated assuming the undulator is 
;		tuned not to E but to the energy yielding highest flux at E (which 
;		energy will be slightly higher then E).
;	APERTURE
;		The aperture for flux calculation, given as a 2-element vector in
;		[x-size,y-size] format, in mm.  If not given and /DEF is set, the
;		standard aperture from !BLPAR is used.
;	DISTANCE
;		The distance of the aperture (see above) from source, in meters.  If not
;		given and /DEF is set, the standard distance from !BLPAR is used.
;	/COHERENT
;		Switch.  If set, coherent flux is evaluated.
;	/PROGRESS
;		Switch.  If set, the number of the currently processed harmonic is 
;		displayed to the screen and partial results are plotted without waiting 
;		for calculation's end.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword for transfering keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Screen output of flux as function of energy, within the required range.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the energy-flux curve as a [2,N] array.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Calculates through repeated calls to UND_FLUX (see there).  At each 
;		energy the harmonic yielding highest flux at said energy is assumed.
;		Calls BL_DEFAULTS, ID_CONV and UND_FLUX.  Calls DEFAULT, HOW_MANY,
;		MAKE_GRID, NULLIFY, ONE_OF and SORPURGE, from MIDL.
; MODIFICATION HISTORY:
;		Modified 15-NOV-2015 by Mati Meron.
;		Modified 20-JAN-2016 by Mati Meron.  Added keywords EMIN and KMAX.
;		Modified 15-JUN-2016 by Mati Meron.  Internal changes.
;		Modified 20-JAN_2018 by Mati Meron.  Internal changes.  Added keyword
;		MIN_GAP.
;		Modified 30-MAY-2019 by Mati Meron.  Added keyword COHERENT.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1
	step = 0.5

	ban = Default(ban,1e-3,/dtyp)
	btem = byte(string(1e2*ban,form='(f6.4)'))
	dum = where(btem ne 48, ndum)
	if ndum gt 0 then stem = string(btem[0:dum[ndum-1]]) + '%bw' $
	else message, 'Invalid bandwidth!'
	xtit = 'Energy (keV)'
	ytit = 'Flux (ph/s/' + stem + ')'
	if keyword_set(coh) then begin
		tit = 'Coherent flux '
		cohfl = 1
	endif else begin
		tit = 'Flux '
		cohfl = 0
	endelse

	case n_elements(eran) of
		0	:	message, 'Missing energy range!'
		1	:	weran = [0,eran>0]
		2	:	weran = [min(eran,max=max),max]
		else:	message, 'ERAN needs no more than 2 elements!'
	endcase

	wdef = Default(def,1,/dtyp)
	if per lt 1 then wper = 1d3*per else wper = 1d*per
	wha = How_many(fir=ren,sec=rgm,whi=whi)
	if wdef then $
	BL_defaults, ene= ren,gamm= rgm,cur= cur,ape= ape,dist= dst,scu= scu
	BL_defaults, dev = tudl, min_gap = mgp, scu = scu
	wudl = Default(udl,tudl)
	ID_conv, gap=mgp, per=wper, ene=emn, emax=emx, rgam=rgm, scu=scu, _extra=_e
	hape = string(ape[0],form='(f3.1)')
	vape = string(ape[1],form='(f3.1)')
	sdst = string(dst,form='(f5.2)')
	if keyword_set(scu) then sund = 'SCU-' else sund = 'U-'
	if wper eq floor(wper) then swper = string(wper,form='(i0)') $
	else swper = string(wper,form='(f4.1)')
	tit = tit + 'through a '+hape+'!9X!x'+vape+'mm aperture at '+sdst+'m'+'!c'
	if keyword_set(crr) then tit = tit + 'Corrected;  '
	if keyword_set(opt) then tit = tit + 'Optimized;  '
	tit = tit + sund + swper + string(wudl,form='(" , ",f3.1,"m long , ")') + $
	string(mgp,form='("min. gap = ",f4.1,"mm")')
	stit = !blpar.sync+ ' ;  Ring energy = '+ string(ren,form='(f5.2,"GeV")')+ $
	' ;  Ring current = ' + string(fix(1e3*cur),form ='(i0,"mA")')
	Nullify, whi, fir=ren, sec=rgm

	case One_of(emi,kma,val=val) of
		-1	:	val = emn
		0	:
		1	:	val = emx/(1 + val^2/2)
	endcase
	emn = emn > val < emx

	if weran[1] gt emn then begin
		wstep = Default(estep,step)
		ene = Make_grid(weran,wstep,/step)
		hmax = floor(max(weran)/emn)
		nhar = hmax - hmax/2
		ihar = 2*indgen(nhar) + 1
		ene = [ene,ihar*emn,ihar*emx]
		ene = ene[where(ene ge (emn > weran[0]) and ene le weran[1])]
		ene = ene[Sorpurge(ene)]
		res = 0*ene

		prfl = keyword_set(prg)
		for i = 0l, nhar-1 do begin
			if prfl then begin
				if i eq 0 then begin
					print
					print, '	Top harmonic :	', string(ihar[-1],form='(i0)')
					print
				endif
				print, '	Processing :	', string(ihar[i],form='(i0)')
			endif
			dum = where(ene ge ihar[i]*emn and ene le ihar[i]*emx,ndum)
			if ndum gt 0 then begin
				tene = ene[dum]
				if cohfl then tres = $
					Und_coflux(tene,per= wper,har= ihar[i],ban= ban,def= wdef,$
					scu= scu,nper= npr,und_len= udl,min_gap= mgp,ring_ene= ren,$
					rgam= rgm,current= cur,radial_size= rsg,angular_size= asg,$
					correct= crr, aperture= ape, distance= dst, _extra=_e) $
				else tres = $
					Und_flux(tene,per= wper,har= ihar[i],ban= ban,def= wdef,$
					scu= scu,nper= npr,und_len= udl,min_gap= mgp,ring_ene= ren,$
					rgam= rgm,current= cur,radial_size= rsg,angular_size= asg,$
					correct= crr, optimize= opt, aperture= ape, distance= dst,$
					_extra=_e)				
				Nullify, whi, fir=ren, sec=rgm
				tdum = where(finite(tres))
				tene = tene[tdum]
				tres = tres[tdum]
				lo = where(ene eq tene[0])
				hi = where(ene eq tene[-1])
				res[lo:hi] = res[lo:hi] > tres
				wait, 0.001
				if prfl then plot, ene, res, tit= tit, subtit= stit, $
				xtit= xtit, ytit= ytit, xmar= [12,3], ymar= [6,4], _extra= _e
			endif else print, '					Not in range'
		endfor
	endif else message, 'Impossible energy range!'

	plot, ene, res, tit= tit, subtit= stit, xtit= xtit, ytit= ytit, $
	xmar = [12,3], ymar = [6,4], _extra= _e

	res = transpose([[ene],[res]])
	return
end