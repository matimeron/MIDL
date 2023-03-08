Function Liqliq, energy = ekev, wavelength = lamb, theta = tet, qz = qzv, $
	qconst = qcn, elements = elar, num_elems = numels, dens= rarr, dfac= dfac, $
	weights = garr, thicks = tarr, roughness = roff, path = pth, $
	formula = form, degrees = deg, field = fld, ret_vals = rtv

;+
; NAME:
;		LIQLIQ
; VERSION:
;		6.2
; PURPOSE:
;		Calculates the reflection of X-rays off a flat surface, within medium.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = LIQLIQ ([ENERGY = EKEV ... WAVELENGTH = LAMB],
;							[THETA = TET ... QZ = QZV], [QCONST = Q],
;							ELEMENTS = ELAR [, optional keywords])
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY									|	Either Energy or Wavelength,
;		Photon energy (in keV).				|	but never both, must be given.
;	WAVELENGTH								|	However, see QCONST below.
;		Photon wavelength (in Angstrem).	|
;	THETA									|	Either THETA or QZ, but never
;		Angle(s) of incidence (glancing).	| 	both, must be given.  However
;		In radians, unless /DEGREES is set.	|	see QCONST below.
;	QZ										|
;		Value(s) of Qz (in inverse Angstrem)|
;	QCONST
;		A constant value for QZ.  If given one and only one of ENERGY,
;		WAVELENGTH, THETA must be given while QZV cannot be given.
;	ELEMENTS
;		Accepts a list of one or more elements which comprise the target.  Can
;		be provided as character array (each element represented by its
;		chemical symbol) or numeric array (each element represented by its Z).
;	NUM_ELEMS
;		Integer scalar or array.  Contains the numbers of elements in each
;		layer.  If only pure, single element layers are present, (i.e. if all
;		the entries in NUM_ELEMS are 1) can be ignored.  IMPORTANT:  even if
;		only one entry is not 1, all the entries must be provided.
;	DENS
;		Array of layer densities (same length as NUM_ELEMS).  Not needed if
;		only pure elemental layers are present, mandatory otherwise.
;	DFAC
;		Density multiplier, used to account for the possibility that layer
;		densities differ from bulk densities.  Can be either a scalar or an
;		array of same length as NUM_ELEMS.  Default value is 1.
;	WEIGHTS
;		Array of partial weights of the elements in each layer (same length as
;		ELEMENTS), in order.  Not needed if only pure elemental layers are
;		present, mandatory otherwise.
;	THICKS
;		Array of layer thicknesses, in Angstrem.  Not needed if only a single
;		interface (i.e. 2 layers) is present.
;	ROUGHNESS
;		Value of surface roughness, in Angstrem. Default value is 0.  Can be
;		provided as scalar (in which case the value provided applies to all
;		interfaces or a vector of same length as number of interfaces (in which
;		case consecutive roughnesses are applied to consecutive interfaces).
;	PATH
;		The length of the path (in and out) through the first layer, in mm.
;		If given, LIQLIQ corrects output for absorption in said layer.
;	/FORMULA
;		Switch.  If set and WEIGHTS (see above) are provided, they're taken as
;		"formula weights".
;	/DEGREES
;		Switch.  Specifies that the angle is given in degrees. Default is
;		radians.
;	/FIELD
;		Switch.  If set, reflected field values are returned, normalized so that
;		the incoming incident field is 1.  Note, these values are complex.
;	RET_VALS
;		Optional output, see below.
; OUTPUTS:
;		Returns the reflection (or, optionally, transmition) coefficient for
;		all the angles or qz values provided, unless /FIELD is set in which case
;		field values are returned.  Output form is same as input.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_VALS
;		If THETA is given, returns the corresponding QZ and vice versa.
; COMMON BLOCKS:
;		SXR_STUFF.  See LOAD_ABS_COEFFS for more information.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Only elements defined in ABCTAB may be used
; PROCEDURE:
;		Multiple interface backwards propagation.  Uses values from ABCTAB and
;		absorption coefficients provided by ABS_COEFF.  Uses calls to DIELECT,
;		ELECOMP and LOAD_ABS_COEFFS.  Also calls ABS_MM, ARREQ, CAST, DEFAULT,
;		FPU_FIX, ISNUM, ONE_OF and REAL_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 25-MAR-200& by Mati Meron as a modification of the routines
;		MIRROR/REFLECT.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	Load_abs_coeffs
	conv = float(!srcon.conv)

	if keyword_set(deg) then amult = !dtor else amult = 1.
	roffl = Isnum(roff)

	if Isnum(qcn) then begin
		if n_elements(qcn) eq 1 then qcn = qcn[0] $
		else message, 'Qz_constant must be a scalar'
		whi = One_of(ekev,lamb,tet,qzv,val=inp)
		diminp = size(inp,/dim)
		ninp = n_elements(inp)
		if not Arreq(diminp,0) then inp = reform(inp,ninp)

		case whi of
			-1	:	message, 'No input present!'
			0	:	begin
						en = inp
						k = 2*!pi/conv*en
						sang = qcn/2./k
					end
			1	:	begin
						en = conv/inp
						k = 2*!pi/inp
						sang = qcn/2./k
					end
			2	:	begin
						sang = sin(amult*inp)
						k = qcn/2/sang
						en = conv/(2*!pi)*k
					end
			3	:	message, "Can't input Qz when Qconst is used!"
		endcase

	endif else begin
		fwhi = One_of(ekev,lamb,val=finp)
		swhi = One_of(tet,qzv,val=sinp)
		lenimp = [n_elements(finp),n_elements(sinp)]
		ninp = max(lenimp,min=minp)
		if minp lt ninp then begin
			if minp eq 1 then begin
				if lenimp[0] eq 1 then begin
					diminp = size(sinp,/dim)
					finp = replicate(finp,ninp)
					sinp = reform(sinp,ninp)
				endif else begin
					diminp = size(finp,/dim)
					finp = reform(finp,ninp)
					sinp = replicate(sinp,ninp)
				endelse
			endif else message, 'Energy-angle size mismatch!'
		endif else begin
			if ninp gt 1 then begin
				diminp = size(finp,/dim)
				finp = reform(finp,ninp)
				sinp = reform(sinp,ninp)
			endif else begin
				diminp = 0l
				finp = finp[0]
				sinp = sinp[0]
			endelse
		endelse

		case fwhi of
			-1	:	message, 'Missing energy/wavelength input!'
			0	:	en = finp
			1	:	en = conv/finp
		endcase
		k = 2*!pi/conv*en

		case swhi of
			-1	:	message, 'Missing angle/qz input'
			0	:	begin
						sang = sin(amult*sinp)
						rtv = 2*k*sang
					end
			1	:	begin
						sang = sinp/(2*k)
						rtv = asin(sang)/amult
					end
		endcase
	endelse
	if max(abs(sang)) le 1 then cang = cos(asin(sang)) $
	else message, 'Impossible k or qz value(s)!'

	nels = n_elements(elar)
	nlays = n_elements(Default(numels,elar))
	defrarr = abctab[Elecomp(elar)].ro

	if nlays le 1 then message, 'At least two layers needed!'
	if nlays eq nels then begin
		numels = Default(numels,replicate(1l,nlays),/dtyp)
		wrar = Default(rarr,replicate(0.,nlays),/dtyp)
		wgar = Default(garr,replicate(1.,nels),/dtyp)
	endif else begin
		numels = long(numels)
		wrar = Cast(rarr,4)
		wgar = Cast(garr,4)
	endelse
	if total(numels) ne nels then message, 'Wrong # of elements!'
	if n_elements(wrar) ne nlays then message, 'Wrong # of layer densities!'
	if n_elements(wgar) ne nels then message, 'Wrong # of partial weights!'

	case n_elements(dfac) of
		0	:	wdfc = replicate(1.,nlays)
		1	:	wdfc = replicate(dfac,nlays)
		nlays:	wdfc = dfac
		else:	message, 'Wrong # of density factors!'
	endcase

	if n_elements(tarr) lt nlays - 2 then message, 'Wrong # of thicknesses!'
	if n_elements(tarr) eq 0 then d = [0.,0.] else d = ([0.,tarr,0.])[0:nlays-1]

	ni = (zi = (ephi = (etphi = make_array(ninp, nlays, type = 6))))
	if roffl then begin
		case n_elements(roff) of
			1			:	wrof = [0.,replicate(roff,nlays-1)]
			nlays-1		:	wrof = [0.,roff]
			else		:	message, 'Wrong # of roughnesses'
		endcase
		chi = (psi = ni)
	endif else chi = (psi = make_array(ninp,nlays,val = 1.))

	jf = 0
	for i = 0l, nlays-1 do begin
		jl = jf + numels[i] - 1
		if jl eq jf and wrar[i] le 0 then begin
			wrar[i] = defrarr[jf]
			wgar[jf] = 1.
		endif
		if i eq 0 then begin
			keep = sang^2 + cang^2*Dielect(en, elem = elar[jf:jl], $
			dens = wrar[i], dfac = wdfc[i], weights = wgar[jf:jl], form= form)
			if n_elements(pth) eq 1 then abfac = $
			exp(-0.1*pth*Abs_coeff(en, elements = elar[jf:jl], $
			dens = wrar[i], dfac = wdfc[i], weights = wgar[jf:jl], form=form)) $
			else abfac = 1
		endif
		ni[*,i] = sqrt(keep - Dielect(en, elements = elar[jf:jl], $
		dens = wrar[i], dfac = wdfc[i], weights = wgar[jf:jl], form= form))
		ephi[*,i] = exp(complex(0,k*d[i])*ni[*,i])
		etphi[*,i] = (ephi[*,i])^2
		if i gt 0 then begin
			zi[*,i] = (ni[*,i-1] - ni[*,i])/(ni[*,i-1] + ni[*,i])
			if roffl then begin
				chi[*,i] = exp(-(2*k*wrof[i]*Real_mm(ni[*,i-1]))^2/2)
				psi[*,i] = exp(-(k*wrof[i]*Real_mm(ni[*,i] - ni[*,i-1]))^2/2)
			endif
		endif
		jf = jl + 1
	endfor
	rot = replicate(complex(0.,0.), ninp)

	for i = nlays - 1, 1, -1 do begin
		dumf = psi[*,i]*etphi[*,i]*rot
		dums = zi[*,i]*dumf + 1
		rot = chi[*,i]*(dumf + zi[*,i])/dums
	endfor
	res = rot
	if not keyword_set(fld) then res = Abs_mm(res)^2
	if not Arreq(diminp,0) then res = reform(res,diminp)

	return, FPU_fix(abfac*res)
end