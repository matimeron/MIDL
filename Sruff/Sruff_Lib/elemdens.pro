Function Elemdens, ele, z = z, a = a

;+
; NAME:
;		ELEMDENS
; VERSION:
;		8.7
; PURPOSE:
;		Given element's chemical symbol returns its density (and optionally its
;		atomic number and weight).
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = ELEMDENS (ELE [, Z = Z, A = A])
; INPUTS:
;	ELE
;		Chemical symbol (or, optionally, atomic number) of an element.  Array
;		input is acceptable.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	Z
;		Optional output, see below.
;	A
;		Optional output, see below.
; OUTPUTS:
;		Returns the input element's density, in gr/cm^3.
; OPTIONAL OUTPUT PARAMETERS:
;	Z
;		The name of a variable to receive the input element's atomic number.
;	A
;		The name of a variable to receive the input element's atomic weight.
;
;		Note:	If the input is an array, Z and A are arrays of same dimension.
; COMMON BLOCKS:
;		SXR_STUFF.  Contains (second entry) the table ABCTAB.
;		See SAVE_ABS_COEFFS for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the input must be a valid chemical symbol(s).
; PROCEDURE:
;		Straightforward.  Calls ELECOMP.
; MODIFICATION HISTORY:
;		Created 5-NOV-2018 by Mati Meron.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	Load_abs_coeffs

	ind = Elecomp(ele)
	z = abctab[ind].z
	a = abctab[ind].a

	return, abctab[ind].ro
end