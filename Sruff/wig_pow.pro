Function WIG_pow, npoints = npo, def= def, period= per, nper = npr, scu = scu, $
	rgam = rgm, ering = ern, ecrit = ecr, kval = kvl, $
	current= cur, distance= dst, aperture= apr, tet_range= tet, psi_range= psi,$
	filters = filt, filthicks = flth, mirrors = mirr, mirangles = mran, $
	target = targ, tarthick = tath, show = sho, total = tot, _extra = _e

;+
; NAME:
;		WIG_POW
; VERSION:
;		8.44
; PURPOSE:
;		Calculates the power distribution of a wiggler synchrotron source.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = WIG_POW( NPOINTS = NPO [,/DEF] PERIOD = PER [,NPER = NPR], $
;		{RGAM=RGM or ERING=ERN} {ECRIT = ECR or KVAL = KVL} [,keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	NPOINTS
;		Two element vector, the number of points to be used over a data quadrant
;		in the horizontal and vertical direction (the full number of points is
;		2*NPOINTS + 1.  Generated internally if not given.
;	/DEF
;		Switch.  If set and some machine specific inputs are missing, they're
;		replaced by defaults from !BLPAR.
;	PERIOD
;		The length of the wiggler period.  If the value is less then 1, it is
;		assumed to be in meters, else it is assumed to be in milimeters.
;		Mandatory, no defaults.
;	NPER
;		Number of periods.  If not given, it is estimated based on PERIOD and on
;		the standard device length from !BLPAR.
;	/SCU
;		Switch.  If set, supeconducting insertion device parameters are used.		|
;	RGAM
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
;		The distance from source, in meters.  If given, the angular ranges (TET
;		and PSI) are converted to spatial ranges.
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
;	/SHOW
;		Switch.  If set, an image of the 2D distribution is displayed.
;	TOTAL
;		Optional output, see below
;	_EXTRA
;		A formal keyword used to pass keywords to the function DISPLAY_MM.  Not
;		to be used directly.
; OUTPUTS:
;		Returns the 2D power distribution (units of W/mrad^2), per unit solid
;		angle, over the angular range provided.  If DISTANCE is given, power
;		distribution per unit area (W/mm^2) is returned instead.
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
;		the Dejus/Viccaro paper) over energy, for all angle points provided.
;		Using BL_DEFAULTS, THRUPUT and WIG_FUN from SRUFF_LIB.  Calls CALCTYPE,
;		CAST, DEFAULT, DISPLAY_MM, ISNUM, MAKE_GRID, ONE_OF, PARTOT and SEQLIM,
;		from MIDL.
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
	escmax = 8d
	tstep = 0.01d
	pstep = 0.2d
	estep = 0.05d

	typ = Calctype(tet,psi,0.,def=4)
	if n_elements(per) eq 1 then begin
		if per gt 1 then wper = amult*per else wper = 1d*per
		Bl_defaults, dev = dlen, scu = scu
		wnpr = Default(npr,floor(dlen/wper),/dtyp)
	endif else message, 'Missing Period length!'

	npofl = n_elements(npo) eq 2
	if npofl then tstep = tdef/(npo[0]-1)
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

	dstfl = 0
	if n_elements(dst) eq 1 then begin
		wdst = Cast(dst,typ)
		dstfl = 1
	endif else wdst = 1d
	if Isnum(apr) then begin
		if n_elements(apr) eq 2 then begin
			tet = apr[0]/wdst
			psi = apr[1]/wdst
		endif else message, 'Aperture needs two elements!'
	endif

	if Isnum(tet) then gtet = amult*wrgm*tet/(2*wkvl) else gtet = tdef + tstep
	if Isnum(psi) then gpsi = amult*wrgm*psi/2 else gpsi = pdef
	if npofl then wnpo = npo else wnpo = ceil([gtet,gpsi]/[tstep,pstep])
	tp = Make_grid([-1,1]#[gtet,gpsi],2*wnpo+1,dimvec=ntp)
	dtp = 2.*[gtet,gpsi]/(ntp-1)
	arel = product(dtp)
	wtet = reform(tp[0,*,*])
	wpsi = reform(tp[1,*,*])

	jmax = 4
	kesc = ceil(escmax/(2^jmax*estep))
	nesc = 2^jmax*kesc + 1
	esc = Make_grid([0,escmax],nesc)
	westep = 1.*escmax/(nesc-1)
	tem = make_array([nesc,ntp],type=typ)
	stem = make_array([jmax+1,ntp],type=typ)
	seq = make_array(jmax+1,type=typ)

	pfac = Thruput(wecr*esc,filte=filt,filth=flth,mirro=mirr,miran=mran, $
	targe=targ,tarth=tath)
	for i = 0l, nesc-1 do tem[i,*,*] = pfac[i]*Wig_fun(wtet,wpsi,esc[i])
	fri = reform(0.5*(tem[0,*,*] + tem[nesc-1,*,*]))
	for j = 0, jmax do begin
		ind = 2^(jmax-j)*lindgen(2^j*kesc+1)
		stem[j,*,*] = westep*(total(tem(ind,*,*),1) - fri)
		seq[j] = 2^(jmax-j)*Partot(reform(stem[j,*,*]),symfri=[-0.5,-0.5])
	endfor

	slim = Seqlim(seq,stat=sta)
	if sta then cfac = slim/seq[jmax] else cfac = 1.
	coeff = 6*cfac/!dpi^2*!srcon.alp*wnpr*wecr*pcur
	tot = Cast(coeff*wkvl*arel*seq[jmax],typ,typ,/fix)
	afac = amult*wrgm/wdst
	res = make_array([3,ntp],type=typ)
	res[0,*,*] = wkvl*wtet/afac
	res[1,*,*] = wpsi/afac
	res[2,*,*] = coeff*afac^2*stem[jmax,*,*]

	if keyword_set(sho) then begin
		if dstfl then begin
			tit = strcompress('Power density (W/mm!e2!n) at ' + $
				string(wdst,form='(f6.2,"m!c")'))
			xtit = 'X (mm)'
			ytit = 'Y (mm)'
		endif else begin
			tit = 'Power density (W/mrad!e2!n)!c'
			xtit = 'Xang (mrad)'
			ytit = 'Yang (mrad)'
		endelse
		Display_mm, res, /auz, /rest, xtit=xtit, ytit=ytit, tit=tit, _extra=_e
	endif

	return, Cast(res,typ,typ,/fix)
end