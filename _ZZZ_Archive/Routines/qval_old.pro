Function Qval_old, crystal = crs, index = ind, diffract = dif, transmit = trn, $
	cold = cld, dzer = dzer, dref = dref, geofac = gef

;+
; NAME:
;		QVAL
; VERSION:
;		8.4
; PURPOSE:
;		Calculates the diffractive q-value for a given crystal and reflection
;		index.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = QVAL (keywords)
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	CRYSTAL
;		Character scalar translating to a name of recognized crystal.
;		Currently the recognized list includes : Diamond, Silicon, Germanium.
;		Only the first two letters are needed.
;	INDEX
;		The reflection index, given as 3 element vector.  Example: [1,1,1].
;	/DIFFRACT
;		Switch.  When set, the q-value for diffraction is calculated
;	/TRANSMIT
;		Switch.  When set, the q-value for transmition is calculated.
;
;		At most one of DIFFRACT and TRANSMIT may be set.  If none is, the
;		default mode is DIFFRACT.
;	/COLD
;		Switch.  If set and the crystal is Silicon, d-spacing is appropriately
;		adjusted for cryogenic (LN2) temperature.
;	DZER
;		Optional output, see below.
;	DREF
;		Optional output, see below.
;	GEOFAC
;		Optional output, see below.
; OUTPUTS:
;		Returns the value of the q-value for reflection (or transmission),
;		for the crystal and reflection index given.
; OPTIONAL OUTPUT PARAMETERS:
;	DZER
;		Returns the value of the cell size (for cubic crystals only).
;	DREF
;		Returns the d-value for the reflection index given.
;	GEOFAC
;		Returns the value of the geometrical coefficient of the structure
;		factor, per atom (i.e. normalized to the number of atoms per cell.
; COMMON BLOCKS:
;		MONO_STUFF.  See SAVE_CRYST_DATA for detailed description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward calculation using standard formulas (see Warren).
;		Calls ONE_OF and STRMATCH_MM from MIDL.  Also calls LOAD_CRYST_DATA and
;		SCAT_FACT from MONO_LIB.
; MODIFICATION HISTORY:
;		Created 25-APR-2002 by Mati Meron.
;		Modified 10-JAN-2007 by Mati Meron.  Internal changes.
;		Modified 10-JAN-2015 by Mati Meron.  Added keyword COLD.
;-

	common mono_stuff, nfl, conv, cer, asftab

	on_error, 1
	Load_cryst_data

	if n_elements(ind) ne 3 then message, $
	'Missing or incomplete reflection index!'
	isum = total(ind,/int)
	isumsq = total(ind^2)

	cnum = Strmatch_mm(crs,asftab.name,2)
	if cnum lt 0 then message, 'Crystal not in list!'
	if cnum eq 1 and keyword_set(cld) then crmult = 0.999776 else crmult = 1
	dzer = crmult*asftab[cnum].csiz[0]
	dref = dzer/sqrt(isumsq)

	nat = 8
	coeff = 2*cer/dzer/isumsq
	if (One_of(dif,trn) > 0) then begin
		gef = 1.
		res = Scat_fact(0,cryst=crs)
	endif else begin
		if isum mod 4 ne 2 then begin
			samfac = 1 - total((ind + shift(ind,1)) mod 2, /int)/2
			gef = samfac*abs(cos(!pi/4*isum))
		endif else gef = 0.
		res = Scat_fact(0.5/dref,cryst=crs)
	endelse

	return, nat*coeff*gef*res
end