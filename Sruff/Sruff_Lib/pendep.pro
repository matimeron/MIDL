Function Pendep, theta = tet, qz = qzv, energy = ekev, wavelength = lamb, $
	elements = elar, num_elems = num, dens = ro, dfac = dfac, weights = warr, $
	formula = form, enuns = dun, degrees = deg, ret_vals = rtv

;+
; NAME:
;		PENDEP
; VERSION:
;		8.484
; PURPOSE:
;		Calculates the penetration depth of X-rays into a flat surface.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = PENDEP ([THETA = TET ... QZ = QZV],
;							[ENERGY = EKEV ... WAVELENGTH = LAMB],
;							ELEMENTS = ELAR [, optional keywords])
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	THETA									|	Either THETA or QZ, but never
;		Angle(s) of incidence (glancing).	| 	both, must be given.  Numeric
;		In radians, unless /DEGREES is set.	|	values of arbitrary dimensions.
;	QZ										|
;		Value(s) of Qz (in inverse Angstrem)|
;
;	ENERGY									|	Either Energy or Wavelength,
;		Photon energy (in keV).				|	but never both, must be given.
;	WAVELENGTH								|	Only scalars allowed.
;		Photon wavelength (in Angstrem).	|
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
;	ENUNS
;		Character value, specifies energy units.  Acceptable units are:
;		KEV, EV, NANOMETERS, ANGSTREM.  Only first 2 letters matter.  Default
;		units are specified by the variable ENUN in the common block SXR_STUFF,
;		initially set to the value of DENUN ('keV').  If provided, the value in
;		ENUNS replaces the previous ENUN.
;	/DEGREES
;		Switch.  Specifies that the angle is given in degrees. Default is
;		radians.
;	RET_VALS
;		Optional output, see below.
; OUTPUTS:
;		Returns the penetration depth, in Angstrem, for all the angles or qz
;		values provided.  Output form is same as input.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_VALS
;		If THETA is given, returns the corresponding QZ and vice versa.
; COMMON BLOCKS:
;		None
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Only elements defined in ABCTAB may be used
; PROCEDURE:
;		Straightforward, from definitions.  Calls DIELECT from SRUFF.  Calls
;		ARREQ, DEFAULT, FPU_FIX, IMAGINARY_MM and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 1-JUN-2006 by Mati Meron.
;		Modified 10-MAY-2017 by Mati Meron.  Added keyword NUM_ELEMS and,
;		through it, the option of arbitrary superphase.
;-

	on_error, 1

	case One_of(ekev,lamb,val=input) of
		0	:	en = input
		1	:	en = conv/input
		else:	message, 'Missing energy/wavelength input!'
	endcase
	k = 2*!pi*en/!srcon.conv

	if keyword_set(deg) then amult = !dtor else amult = 1.
	case One_of(tet,qzv,val=input) of
		0	:	begin
					sang = sin(amult*input)
					rtv = 2*k*sang
				end
		1	:	begin
					sang = input/(2*k)
					rtv = asin(sang)/amult
				end
		else:	message, 'Missing angle/qz input'
	endcase

	sandim = size(sang,/dim)
	if n_elements(num) eq 2 then begin
		nel = n_elements(elar)
		if total(num) ne nel then message, 'Wrong # of elements!'
		tro = Default(ro,replicate(0.,2),/dtyp)
		tfac = Default(dfac,replicate(1.,2),/dtyp)
		twar = Default(warr,replicate(1.,nel),/dtyp)
		if n_elements(tro) ne 2 then message, 'Wrong # of densities!'
		if n_elements(tfac) ne 2 then message, 'Wrong # of density factors!'
		if n_elements(twar) ne nel then message, 'Wrong # of partial weights!'

		sup = Dielect(en, elem= elar[0:num[0]-1], dens= tro[0], dfac= tfac[0],$
		weights= twar[0:num[0]-1], enuns= dun, form= form)
		sub = Dielect(en, elem= elar[num[0]:*], dens= tro[1], dfac= tfac[1], $
		weights= twar[num[0]:*], enuns= dun, form= form)
		die = sub - sup
	endif else begin
		if n_elements(num) gt 2 then message, 'Bad NUM_ELEMS input!'
		die = Dielect(en, elements= elar, dens= ro, dfac= dfac, $
		weights= warr, enuns= dun, form= form)
	endelse

	ni = sqrt(sang^2 - die)
	res = 1/(2*k*abs(Imaginary_mm(ni)))
	if not Arreq(sandim,0) then res = reform(res,sandim)

	return, FPU_fix(res)
end