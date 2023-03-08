Function Scan_PD_tol, tol, pdstat = pds, base = bas

;+
; NAME:
;		SCAN_PD_TOL
; VERSION:
;		8.475
; PURPOSE:
;		Returns a tolerance value for the use of various area detector routines.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_TOL (tol, pdstat = pds, base = bas)
; INPUTS:
;	TOL
;		Integer scalar, the tolerance value, in pixels.  Optional.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	PDSTAT
;		Scalar integer, the PDSTAT value for the data.  See SPSCAN__DEFINE for
;		details.
;	BASE
;		Scalar integer, the base tolerance value.  Default is 3.
; OUTPUTS:
;		If TOL is provided, it is returned as is (possibly rounded to an
;		integer), else an internally generated value is returned.  Currently
;		this value is 3 for a Pilatus (100K or 1M) and 9 for an APEX.  Other
;		values may be added in the future.
;
;		Note:	For PDSTAT values < 3, the result is !NULL.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None.
;; PROCEDURE:
; 		Straightforward.  Calls DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 25-AUG-2016 by Mati Meron.
;		Modified 30-MAR-2014 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if not keyword_set(tol) then begin
		wbas = Default(bas,3l,/dtyp)
		case pds of
			0	:	res = !null
			1	:	res = !null
			2	:	res = !null
			3	:	res = wbas
			5	:	res = 3*wbas
			7	:	res = wbas
			11	:	res = wbas
			else:	message, 'Unknown detector!'
		endcase
	endif else res = ceil(tol)

	return, res
end