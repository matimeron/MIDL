Function SW_zscan, z, d, theta= tet, qz= qzv, energy= ekev, wavelength=lamb,$
	multilayer = mult, substrate = subs, srough = srof, $
	film_elems= felm, weights= fwei, dens= fden, frough= frof, formula= form, $
	degrees = deg, dsigma = sig, show = sho, reflected = ref, _extra = _e

;+
; NAME:
;		SW_ZSCAN_TA
; VERSION:
;		8.0
; PURPOSE:
;		Calculates the intensity of standing waves within a film.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = SW_QSCAN_TA( Z, D, [THETA = TET ... QZ = QZV],
;							[ENERGY = EKEV ... WAVELENGTH = LAMB,]
;							[MULTILAYER - MULT,]
;							SUBSTRATE = SUBS, FILM_ELEMS = FELM
;							[, optional keywords])
; INPUTS:
;	Z
;		Vector or scalar input, the depth(s) of the calculation point(s) within
;		the film, measured from the surface inward.
;	D
;		Scalar value, the thickness of the film.  Optional if MULTILAYER (see
;		below) is used, Mandatory otherwise.
;
;		Note: Z and D must be nonnegative.  For Z > D a zero result is returned.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	THETA									|	Either THETA or QZ, but never
;		Angle of incidence (glancing).		| 	both, must be given.
;		In radians, unless /DEGREES is set.	|
;	QZ										|
;		Value of Qz (in inverse Angstrem)	|
;
;	ENERGY									|	Either Energy or Wavelength,
;		Photon energy (in keV).				|	but never both, must be given.
;	WAVELENGTH								|
;		Photon wavelength (in Angstrem).	|
;
;		Note:	Both QZ/THETA and ENERGY/WAVELENGTH must be scalars.
;	/MULTILAYER
;		Switch.  If set, all inputs relevant to film and substrate (see keywords
;		SUBSTRATE, SROUGH, FILM_ELEMS, WEIGHTS, DENS, FROUGH and /FORMULA are
;		taken from the MULTILAYER_PARS common block.  Any values provided by
;		the keywords listed above are ignored, if given.  One exception is the
;		film thickness (see D above).  This is also provide by the common block
;		but can be overriden by an explicitly provided value if given.
;	SUBSTRATE
;		The element (single, at present) comprising the substrate.  Can be
;		provided as a character contant (the chemical symbol of the element) or
;		a numeric constant (the Z value of the element).  Mandatory unless
;		/MULTILAYER is set.
;	SROUGH
;		Scalar, substrate roughness in Angstrem.  Optional, defaults to zero.
;	FILM_ELEMS
;		Accepts a list of one or more elements which comprise the film.  Can
;		be provided as character array (each element represented by its
;		chemical symbol) or numeric array (each element represented by its Z).
;		Mandatory unless /MULTILAYER is set.
;	WEIGHTS
;		Array of partial weights of the elements in the film (same length as
;		FILM_ELEMS), in order.  Not needed if the film consists of single
;		element, mandatory otherwise.
;	DENS
;		Scalar, the value of the film's density.  Not needed if the film
;		consists of a single element at bulk density, mandatory otherwise.
;	/FORMULA
;		Switch.  If set and WEIGHTS (see above) are provided, they're taken as
;		"formula weights".
;	FROUGH
;		Scalar, film roughness in Angstrem.  Optional, defaults to zero.
;	/DEGREES
;		Switch.  Specifies that the angle is given in degrees. Default is
;		radians.
;	DSIGMA
;		The sigma value for the thickness distribution.  If given, the result is
;		convoluted with the density distribution, assumed to be a Gaussian with
;		sigma = DSIGMA.
;	/SHOW
;		Switch.  If set the calculated results are displayed as a function of Z.
;	REFLECTED
;		Optional output, see below.
;	_EXTRA
;		Formal keyword, used to transfer plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		Returns the intensity (square of amplitude) of the field within the film
;		as a function of the normal location Z (measured from the substrate up),
;		for the value QZ given.  The output has same format as Z.
; OPTIONAL OUTPUT PARAMETERS:
;	REFLECTED
;		Returns the reflectivity of the film/substrate combination.  Note, the
;		result is a scalar, corresponding to the value of QZ (or THETA) used.
; COMMON BLOCKS:
;		SXR_STUFF.  See LOAD_ABS_COEFFS for more information.
;		MULTILAYER_PARS.  Provides multilayer parameters, see MULTILAYER_GEN for
;		details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Only elements defined in ABCTAB may be used.
;		Z must be within limits specified above.
; PROCEDURE:
;		Multiple interface backwards propagation.  Uses calls to ELECOMP,
;		DIELECT, LOAD_ABS_COEFFS, QCRIT and REFLECT from SRUFF.  Also calls
;		ABS_MM, CAST, DEFAULT, FPU_FIX, ISNUM, LEGEND_MM, MAKE_GRID, ONE_OF,
;		REAL_MM and TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAR-2006 by Mati Meron, based on the earlier SW_ZSCAN.
;		Rewritten 10-JUN-2006 by Mati Meron.  Added keyword MULTILAYER.
;		Modified 25-AUG-2010 by Mati Meron.  Replaced LEGEND with LEGEND_MM.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam
	common multilayer_pars, m_exs, m_def, m_form, $
	m_elar, m_numels, m_rarr, m_warr, m_tarr, m_roff

	on_error, 1
	Load_abs_coeffs

	multfl = keyword_set(mult)
	if min(z) ge 0 then wz = Cast(z,4) else message, 'Z must be nonnegative!'
	nz = n_elements(wz)
	if n_elements(d) eq 1 then wd = Cast(d[0],4) $
	else if ((n_elements(d) gt 1) or (not multfl)) then $
	message, 'Thickness must be a scalar!'

	case One_of(ekev,lamb,val=input) of
		0	:
		1	:	ekev = !srcon.conv/input
		else:	message, 'Missing energy/wavelength input!'
	endcase
	if n_elements(ekev) gt 1 then message, 'Energy input must be scalar!'
	k = 2*!pi*ekev[0]/!srcon.conv

	if keyword_set(deg) then amult = !dtor else amult = 1.
	case One_of(tet,qzv,val=input) of
		0	:	qzv = 2*k*sin(amult*input)
		1	:
		else:	message, 'Missing angle/qz input'
	endcase
	sang = qzv/(2*k)
	if n_elements(sang) ne 1 then message, 'Qz/angle must be a scalar!'

	refl = arg_present(ref)
	if sang gt 0 then begin
		if multfl then begin
			if Default(m_exs,0) then begin
				if m_def[0] then begin
					elar = m_elar
					numels = m_numels
					rarr = m_rarr
					warr = m_warr
					tarr = m_tarr
					if Isnum(wd) then tarr[0] = wd else wd = tarr[0]
					roff = m_roff
					form = m_form
					nef = numels[0]
				endif else message, 'Missing film definition!'
			endif else message, 'Multilayer not defined!
		endif else begin
			nef = n_elements(felm)
			case nef of
				0	:	message, 'Missing film definition!'
				1	:	if Default(fden,0) le 0 then $
						fden = abctab[Elecomp(felm)].ro
				else:	begin
							if n_elements(fwei) ne n_elements(felm) then $
							message, 'Weights discrepancy!'
							if n_elements(fden) ne 1 then message, $
							'Bad or missing density!'
						end
			endcase
			elar = [felm,subs]
			numels = [nef,1]
			rarr = [fden,abctab[Elecomp(subs)].ro]
			warr = [fwei,1.]
			tarr = [wd,0]
			roff = [Default(frof,0.,/dtyp),Default(srof,0.,/dtyp)]
			form = keyword_set(form)
		endelse

		r0 = (Reflect(qz=qzv,ene=ekev,elem=elar,num=numels, $
			dens=rarr,wei=warr,thi=tarr,rough=roff,form=form,/field))[0]

		ni = sqrt(sang^2 - Dielect(ekev, elements = elar[0:nef-1], $
				weights = warr[0:nef-1], dens = rarr[0], form = form))
		zi = (sang - ni)/(sang + ni)
		chi = exp(-0.5*(2*k*roff[0]*sang)^2)
		psi = exp(-0.5*(k*roff[0]*Real_mm(ni-sang))^2)

		pcof = chi - zi*r0
		ncof = r0 - chi*zi
		dum = where(abs(ncof) lt 2*Toler(), ndum)
		if ndum gt 0 then ncof[dum] = 0
		eiphi = exp(ni*dcomplex(0,k*(wd-wz)))
		field = (1 + zi)*(psi*pcof*eiphi + ncof/eiphi)

		nd = 1
		dfun = 1.
		if Isnum(sig) then begin
			if sig ne 0 then begin
				hran = 2*abs(sig) < wd
				ns = ceil(hran*max(qzv)/!pi) > 4
				dd = Make_grid([-hran,hran],4*ns+1,dim=nd)
				dfun = exp(-(dd^2/(2.*Cast(sig,4)^2)))
				dfun = dfun/total(dfun)
			endif else dd = 0
		endif else dd = 0.

		res = fltarr(nz,nd)
		if refl then ref = fltarr(nd)
		for i = 0l, nd - 1 do begin
			eidphi = exp(ni*dcomplex(0,k*dd[i]))
			pterm = pcof*eidphi
			nterm = ncof/eidphi
			denom = pterm + zi*nterm
			dum = where(wz le wd - dd[i],ndum)
			if ndum gt 0 then res[dum,i] = Abs_mm(field[dum]/denom)^2
			res[*,i] = dfun[i]*res[*,i]
			if refl then ref[i] = dfun[i]*Abs_mm(chi*(zi*pterm + nterm)/denom)^2
		endfor
		if nd gt 1 then begin
			res = total(res,2)
			if refl then ref = total(ref)
		endif
	endif else begin
		res = fltarr(nz)
		ref = 1.
	endelse

	if keyword_set(sho) and nz gt 1 then begin
		stit = $
		strcompress('q!dz!n = ' + string(2*k*sang,form='(f6.4," A!e-1!n")'))
		plot, z, res, subtit= stit, xtit= 'z (from substrate) (A)',_extra = _e
		qcf = Qcrit(ekev, elem=elar[0:nef-1], weights=fwei, dens=fden,form=form)
		ltext = 'Qc_film = '+string(qcf,form="(f6.4)")
		if not multfl then begin
			qcs = Qcrit(ekev, elem = elar[nef:nef+numels[1]-1], $
			weights=warr[nef:nef+numels[1]-1],dens=rarr[1],form=form)
			ltext = [ltext,'Qc_subs= '+string(qcs,form="(f6.4)")]
		endif else ltext = [ltext,'Multilayer substrate']
		Legend_mm, text = ltext
	endif

	return, FPU_fix(res)
end