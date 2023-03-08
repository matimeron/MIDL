Function Intersect_mm_old, far, sar, digits = dig, dec_digits = dec, netlen= nl, $
	_extra= _e

;+
; NAME:
;		INTERSECT_MM
; VERSION:
;		8.45
; PURPOSE:
;		Finds the intersection of two sets
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = INTERSECT_MM( FAR, SAR [, NETLEN = NL])
; INPUTS:
;	FAR
;		Scalar or array, arbitrary type.
;	SAR
;		Ditto.
;
;		Note:	Can't mix numeric and character types.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	DIGITS															| Only one
;		Accepts an integer value specifying the number of digits	| of these 
;		to consider while rounding.  Default value is 1 digit.		| two may
;	DEC_DIGITS														| be set.
;		Accepts an integer value specifying the number of digits	| Default is
;		after the decimal point.  Default value is 0 digits.		| DIGITS.
;
;		Note:	DIGITS and DEC_DIGITS are used for rounding of FAR and SAR prior
;				do comparisons, when FAR and/or SAR are of one of the FLOAT 
;				types.  For INTEGER types and non-numeric inputs, these keywords
;				have no effect.
;	NETLEN
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass additional keywords to FLTROUND.  Not to 
;		be used directly.
; OUTPUTS:
;		Returns the intersection of FAR and SAR, i.e. a set containing all the 
;		unique elements appearing in both FAR and SAR.  For empty intersection 
;		returns !NULL.
;		
;		Note:	The returned set is sorted in ascending order.
; OPTIONAL OUTPUT PARAMETERS:
;	NETLEN
;		Returns the number of elements in the intersection.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that numeric and character types cannot be mixed.
; PROCEDURE:
;		Trivial, from definition.  Calls FLTROUND, ISNUM, ONE_OF and SORPURGE,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-FEB-2013 by Mati Meron.
;		Modified 20-JAN-2016 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if n_elements(far) gt 0 then wfar = far else wfar = []
	if n_elements(sar) gt 0 then wsar = sar else wsar = []

	if (One_of(dig,dec) ge 0) then begin
		if Isnum(wfar) and not Isnum(wfar,/int) then $
		wfar = Fltround(wfar,dig=dig,dec=dec,_extra=_e) 
		if Isnum(wsar) and not Isnum(wsar,/int) then $
		wsar = Fltround(wsar,dig=dig,dec=dec,_extra=_e)
	endif 

	if wfar ne !null then wfar = wfar[Sorpurge(wfar)]
	if wsar ne !null then wsar = wsar[Sorpurge(wsar)]
	res = [wfar,wsar]
	if res ne !null then begin
		res = res[sort(res)]
		n = n_elements(res)
		s = lindgen(n)
		ss = Sorpurge(res,net=nn)
		nl = n - nn
		if nl gt 0 then begin
			s[ss] = -1
			res = res[where(s ge 0)]
		endif else res = []
	endif else nl = 0

	return, res
end