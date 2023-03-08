Function Qcrit, en, enuns = dun, elements = elar, num_elems = num, $
	dens = ro, dfac = dfac, weights = warr, formula = form, $
	full = ful, complex = cmp, _extra = _e

;+
; NAME:
;		QCRIT
; VERSION:
;		8.06
; PURPOSE:
;		Calculates Qc for a given material.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = QCRIT ([EN,] ELEMENTS = ELAR [, optional keywords])
; INPUTS:
;	EN
;		Energy, assumed in the units specified by the variable ENUN in the
;		common block SXR_STUFF, unless specified otherwise by the keyword ENUNS.
;		Needed only if the keywords /FULL and/or /COMPLEX are set.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENUNS
;		Character value, specifies energy units.  Acceptable units are:
;		KEV, EV, NANOMETERS, ANGSTREM.  Only first 2 letters matter.  Default
;		units are specified by the variable ENUN in the common block SXR_STUFF,
;		initially set to the value of DENUN ('keV').  If provided, the value in
;		ENUNS replaces the previous ENUN.
;	ELEMENTS
;		Accepts a list of one or more elements which comprise the target.  Can
;		be provided as character array (each element represented by its
;		chemical symbol) or numeric array (each element represented by its Z).
;	NUM_ELEMS
;		Integer scalar or array.  Contains the numbers of elements in the 
;		superphase and subphase.  Note the following:
;			1:	If the NUM_ELEMS is scalar, or absent, it is ignored and it
;				is assumed that all the entries in ELEMENTS correspond to the
;				subphase, with the superphase being vacuum.
;			2:	If NUM_ELEMS has two entries, they represent the number of
;				elements in the superphase and subphase, respectively. In this
;				case the sum of the entries in NUM_ELEMS must equal the number
;				of entries in ELEMENTS.
;			3:	If NUM_elems has 3 or more entries, an error results.
;	DENS
;		Target density, scalar value or 2-element vector if NUM_ELEMS has 2 
;		elements.  If provided, overrides the value from the ABCTAB table.  
;		Must be provided if the target is not a pure element (i.e. if ELAR is a 
;		vector).
;	DFAC
;		Multiplier for elemental densities, used to account for the possibility
;		that layer densities differ from bulk densities.  Applies only to
;		single element targets.  Ignored if DENS is provided.  If NUM_ELEMS has
;		2 elements, DFAC, if provided, must have two elements as well.
;	WEIGHTS
;		In case the element list contains more than one element, the relative
;		partial weights of the elements in the target must be provided as a
;		numeric array, using this keyword.
;	/FORMULA
;		Switch.  If set and WEIGHTS (see above) are provided, they're taken as
;		"formula weights".
;	/FULL
;		Switch.  If set, the dispersion correction to Qc is calculated.  In
;		this case, the energy needs to be provided.
;	/COMPLEX
;		Switch.  If set, the complex form of Qc, including the absorption
;		correction, is calculated.  In this case, also, the energy needs to be
;		provided.
;	_EXTRA
;		A formal keyword used to pass keywords to the function DISPER_FUN.  Not
;		to be used directly.  See comment in RESTRICTIONS.
; OUTPUTS:
;		Returns the value(s) of Qc.  If EN is provided the format of the output
;		is same as the format of EN, else it is an energy independent scalar.
;
;		Note:	If /COMPLEX is set, the result is complex, else it is float.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SXR_STUFF.  See LOAD_ABS_COEFFS for more information.
; SIDE EFFECTS:
;		If the keyword ENUNS is used, the value of ENUN in the common block
;		SXR_STUFF will be replaced by the new value.
; RESTRICTIONS:
;		1:	The elements specified must be among those in ABCTAB.
;		2:	The restrictions on NUM_ELEMS, see above.
; PROCEDURE:
;		Standard evaluation using densities from ABCTAB (in common block
;		SXR_STUFF).  Uses calls to ABS_COEFFS, ECONV, ELECOMP, DISPER_FUN and
;		LOAD_ABS_COEFFS from SRUFF_LIB.  Calls itself recursively as needed.  
;		Also calls DEFAULT, FPU_FIX, ISNUM and TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2002 by Mati Meron as a straightforward modification of
;		the routine DIELECT.
;		Modified 10-JUN-2003 by Mati Meron.  Added keyword DFAC.
;		Modified 15-JUN-2009 by Mati Meron.  Added keyword COMPLEX, for full
;		(complex) calculation.
;		Modified 25-JUN-2011 by Mati Meron.  Added keyword NUM_ELEMS and, 
;		through it, the option of arbitrary superphase.  Redefined constants in
;		terms of fundamental constants.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	Load_abs_coeffs
	coc = float(1e-14*16*!pi*!srcon.re*!pcon.na)
	doc = float(1e-18*16/(!srcon.hc*!srcon.scal))
	ioc = float(1e-8*8*!pi/!srcon.conv)

	enun = Default(dun,enun,/strict)

	if n_elements(num) eq 2 then begin
		nel = n_elements(elar)
		if total(num) ne nel then message, 'Wrong # of elements!'
		tro = Default(ro,replicate(0.,2),/dtyp)
		tfac = Default(dfac,replicate(1.,2),/dtyp)
		twar = Default(warr,replicate(1.,nel),/dtyp)
		if n_elements(tro) ne 2 then message, 'Wrong # of densities!'
		if n_elements(tfac) ne 2 then message, 'Wrong # of density factors!'
		if n_elements(twar) ne nel then message, 'Wrong # of partial weights!'

		sup = Qcrit(en,elem=elar[0:num[0]-1],dens = tro[0],dfac = tfac[0], $
		weights= twar[0:num[0]-1],form= form, full= ful, comp= cmp, _extra= _e)
		sub = Qcrit(en,elem=elar[num[0]:*],dens = tro[1],dfac = tfac[1], $
		weights= twar[num[0]:*],form= form, full= ful, comp= cmp, _extra= _e)

		difsq = sub^2 - sup^2
		res = make_array(n_elements(en) > 1,type=Type(sup))
		dum = where(Real_mm(difsq) gt 0, ndum)
		if ndum gt 0 then res[dum] = sqrt(difsq[dum]) else $
		message, 'No reflection', /con
	endif else begin
		if n_elements(num) gt 2 then message, 'Bad NUM_ELEMS input!'
		dfac = Default(dfac,1.,/dtyp)
		efl = Isnum(en)
		if efl then wen = Econv(en, from = enun, to = denun)

		eli = Elecomp(elar, number = nel)
		if nel gt 1 then begin
			if n_elements(ro) eq 1 then begin
				if n_elements(warr) eq nel then begin
					if keyword_set(form) then twarr = warr*abctab[eli].a $
					else twarr = warr
					tro = ro/total(twarr)*twarr
				endif else message, 'Bad or missing composition data!
			endif else message, 'Bad or missing density!'
		endif else begin
			tro = Default(ro,0.)
			if tro eq 0 then tro = dfac*abctab[eli].ro
		endelse
		res = $
		replicate(coc*total(abctab[eli].z*tro/abctab[eli].a),n_elements(en)>1)
	
		if keyword_set(ful) then begin
			if efl then begin
				for j = 0l, nel - 1 do begin
					k = eli[j]
					ned = abctab[k].edlen
					jmp = abctab[k].edtab[0:ned,0]*abctab[k].edtab[0:ned,1]
					for l = 0l, ned do begin
						q = abctab[k].edtab[l,2]
						t = abctab[k].edtab[l,0]/wen
						if l eq 0 then $
						pres = jmp[0]*t^(1+q)*Disper_fun(0,q,_extra=_e) else $
						pres = pres + jmp[l]*t^(1+q)*Disper_fun(t,q,_extra=_e)
					endfor
					res = (res + doc*tro[j]*pres) > res*Toler(res)
				endfor
			endif else message, "Energy needed for full evaluation!'
		endif
	
		if keyword_set(cmp) then begin
			if efl then begin
				abc = $
				Abs_coeff(wen,elem=elar,dens=ro,dfac=dfac,wei=warr,form=form)
				res = complex(res,-ioc*wen*abc)
			endif else message, "Energy needed for complex part!'
		endif
		res = sqrt(res)
	endelse

	if n_elements(res) eq 1 then res = res[0]
	return, FPU_fix(res)
end