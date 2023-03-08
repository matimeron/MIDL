Function WIG_spec, ene, def= def, period= per, nper = npr, scu = scu, $
	rgam = rgm, ering = ern, ecrit = ecr, kval = kvl, $
	current= cur, distance= dst, aperture= apr, tet_range= tet, psi_range= psi,$
	filters = filt, filthicks = flth, mirrors = mirr, mirangles = mran, $
	target = targ, tarthick = tath, power = pow, band = ban, $
	show = sho, total = tot, _extra = _e

;+
; NAME:
;		WIG_SPEC
; VERSION:
;		8.44
; PURPOSE:
;		Calculates the energy spectrum of a wiggler synchrotron source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = WIG_SPEC( ENE [,/DEF] PERIOD = PER [,NPER = NPR], $
;		{RGAM=RGM or ERING=ERN} {ECRIT = ECR or KVAL = KVL} $
;						 [,keywords])
; INPUTS:
;	ENE
;		Numeric scalar or vector, energy in keV units.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/DEF
;		Switch.  If set and some inputs are missing, they're replaced by
;		defaults from !BLPAR.
;	PERIOD
;		The length of the wiggler period.  If the value is less then 1, it is
;		assumed to be in meters, else it is assumed to be in milimeters.
;		Mandatory, no defaults.
;	NPER
;		Number of periods.  If not given, it is estimated based on PERIOD and on
;		the standard device length from !BLPAR.
;	/SCU
;		Switch.  If set, supeconducting insertion device parameters are used.
;	RGAM											|
;		The relativistic Gamma of the source.		|	One and only one of
;	ERING											|	these two is needed.
;		The electron energy (in GeV) of the source.	|
;
;		Note:	If neither of these two is given and /DEF is set, the data is
;				taken from the structure !BLPAR.
;	ECRIT
;		Critical energy, in keV.	|
;	KVAL							|	One and only one of these two is needed.
;		The K-value of the wiggler	|
;	CURRENT
;		Source electron current, in Amp.  Default is 1 Amp if /DEF is not set,
;		else it is the corresponding value from !BLPAR.
;	DISTANCE
;		The distance from source, in meters.  Used only in conjuction with
;		APERTURE (see below).
;	APERTURE
;		aperture size ([X,Y] order) in mm.  If given and DISTANCE is given, the
;		angular ranges are derived from aperture and distance, overriding the
;		values in TET_RANGE and PSI_RANGE.
;	TET_RANGE
;		The Theta (horizontal) angular extent of the source, in miliradians.
;		Default is the full wiggler width, i.e +-KVAL/RGAM
;	PSI_RANGE
;		The PSI (vertical) angular extent of the source, in miliradians.
;		Default is +_4/RGAM.
;	FILTERS														| If given,
;		Character scalar or vector, a list of (single element)	| these two must
;		filters.												| have same
;	FILTHICKS													| number of
;		Numeric scalar or vector, a list of filter thicknesses,	| elements
;		in mm.
;	MIRRORS														| If given,
;		Character scalar or vector, a list of (single element)	| these two must
;		mirror coatings.										| have same
;	MIRANGLES													| number of
;		Numeric scalar or vector, a list mirror angles in mrad.	| elements.
;	TARGET
;		Character scalar, the target element.
;	TARTHICK
;		Numeric scalar, the target thickness in mm.  Optional, even if target
;		is given.  Taken as infinite (i.e. full absorption) if not given.
;	/POWER
;		Switch.  If set, the power spectrum in Watt/keV is output.  The default
;		output is in photons/keV.
;	BAND
;		Numeric value.  If given and /POWER is not set, the output is in
;		photons per bandwidth, with the bandwidth value given by BAND.
;	/SHOW
;		Switch.  If set, a plot of the spectrum is displayed.
;	TOTAL
;		Optional output, see below
;	_EXTRA
;		A formal keyword used to pass keywords to the PLOT.  Not to be used
;		directly.
; OUTPUTS:
;		By default returns the photon spectrum, for the energy values provided,
;		in units of photons/kev or, if BAND is given, in photons/kev/BAND.
;		If the keyword /POWER is set, the power spectrum in Watts/kev is
;		returned.
;
;		The output is of same length and form as the input ENE.
; OPTIONAL OUTPUT PARAMETERS:
;	TOTAL
;		Returns the total power within the covered area.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Integrates the wiggler distribution function (calculated according to
;		the Dejus/Viccaro paper) over angles, for all energy values provided,
;		using using BL_DEFAULTS, THRUPUT and WIG_FUN from SRUFF_LIB.  Calls
;		CALCTYPE, CAST, DEFAULT, INTEG, ISNUM, MAKE_GRID, ONE_OF, PARTOT and
;		TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-FEB-2007 by Mati Meron.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting insertion device option.
;-

	on_error, 1

	amult = 1d-3
	emult = 1d6
	tdef = 1d
	pdef = 4d
	tstep = 0.01d
	pstep = 0.2d

	typ = Calctype(ene,0.)
	if n_elements(per) eq 1 then begin
		if per gt 1 then wper = amult*per else wper = 1d*per
		Bl_defaults, dev = dlen, scu = scu
		wnpr = Default(npr,floor(dlen/wper),/dtyp)
	endif else message, 'Missing Period length!'

	defl = keyword_set(def)
	misfl = 0

	case One_of(rgm,ern) of
		-1	:	begin
					if defl then misfl = 1 else message, $
					'Either gamma or ring energy is needed!'
				end
		0	:	wrgm = Cast(rgm,typ)
		1	:	wrgm = emult*ern/(!srcon.ee*!srcon.scal)
	endcase

	if n_elements(cur) eq 0 then begin
		if defl then misfl = 1 else pcur = 1.
	endif else pcur = cur

	if misfl then BL_defaults, gamma = wrgm, cur = pcur
	pcur = pcur/amult

	case One_of(ecr,kvl) of
		-1	:	message, 'Either critical energy or K value is needed!'
		0	:	begin
					wecr = Cast(ecr,typ)
					wkvl = !srcon.alp*wper*wecr/$
					(3*!pi*wrgm^2*!srcon.re*!srcon.ee*!srcon.scal)
				end
		1	:	begin
					wkvl = Cast(kvl,typ)
					wecr = wkvl/(!srcon.alp*wper)*$
					(3*!pi*wrgm^2*!srcon.re*!srcon.ee*!srcon.scal)
				end
	endcase

	wdst = Default(dst,1d,/dtyp)
	if Isnum(apr) then begin
		if n_elements(apr) eq 2 then begin
			tet = apr[0]/wdst
			psi = apr[1]/wdst
		endif else message, 'Aperture needs two elements!'
	endif

	if Isnum(tet) then gtet = amult*wrgm*tet/(2*wkvl) else gtet = tdef + tstep
	if Isnum(psi) then gpsi = amult*wrgm*psi/2 else gpsi = pdef
	tp = Make_grid([-1,1]#[gtet,gpsi],2*ceil([gtet,gpsi]/[tstep,pstep])+1,$
		dimvec=ntp,fun=tem)
	dtp = 2.*[gtet,gpsi]/(ntp-1)
	arel = product(dtp)
	wtet = reform(tp[0,*,*])
	wpsi = reform(tp[1,*,*])

	esc = ene/wecr
	nesc = n_elements(esc)
	res = make_array(nesc,type=typ)

	pfac = Thruput(wecr*esc,filte=filt,filth=flth,mirro=mirr,miran=mran, $
	targe=targ,tarth=tath)
	for i = 0l, nesc-1 do begin
		tem = pfac[i]*Wig_fun(wtet,wpsi,esc[i])
		res[i] = Partot(tem,symfringe=[-0.5,-0.5])
	endfor

	res = 6/!dpi^2*!srcon.alp*pcur*wnpr*wkvl*arel*res
	tot = Cast(Integ(ene,res,/val),typ,typ,/fix)

	if not keyword_set(pow) then begin
		res = !srcon.scal*res
		if Isnum(ban) then begin
			res = ban*res
			ytit = 'Flux ' + $
			strcompress(string(ban,form='("(ph/s/",f6.4,"bw)")'),/rem)
		endif else begin
			res = res/(ene > Toler())
			ytit = 'Flux (ph/keV)'
		endelse
	endif else ytit = 'Power (W/keV)'

	if keyword_set(sho) then plot, ene, res, $
	xtit = 'Energy (keV)', ytit = ytit, _extra = _e

	return, Cast(res,typ,typ,/fix)
end