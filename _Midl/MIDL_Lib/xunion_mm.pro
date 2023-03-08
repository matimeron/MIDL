Function Xunion_mm, far, sar, digits = dig, dec_digits = dec, netlen= nl, $
	_extra= _e

;+
; NAME:
;		INTERSECT_MM
; VERSION:
;		8.476
; PURPOSE:
;		Finds the Exclusive Union of two sets
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = XUNION_MM( FAR, SAR [, NETLEN = NL])
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
;		Returns the exclusive union of FAR and SAR, i.e. a set containing all
;		the unique elements appearing in FAR or SAR but not both.  For empty
;		exclusive union returns !NULL.
;		
;		Note:	The returned set is sorted in ascending order.
; OPTIONAL OUTPUT PARAMETERS:
;	NETLEN
;		Returns the number of elements in the exclusive union.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that numeric and character types cannot be mixed.
; PROCEDURE:
;		Trivial, from definition.  Calls FLTROUND, ISNUM, ONE_OF, SORPURGE and
;		SORSPEC, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-OCT-2016 by Mati Meron as a variation on INTERSECT_MM.
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
	if res ne !null then res = res[Sorspec(res,net=nl)] else nl = 0l

	return, res
end