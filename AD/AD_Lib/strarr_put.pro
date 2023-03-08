Pro Strarr_put, sarr, sub, pos, line = lin, phrase = phr

;+
; NAME:
;		STRARR_PUT
; VERSION:
;		8.41
; PURPOSE:
;		Similar to the IDL routine STRPUT, performs substitution in selected
;		elements of a string array.
; CATEGORY:
;		String Processing.
; CALLING SEQUENCE:
;		STRARR_PUT SARR, SUB [, POS] LINE = LIN, PHRASE = PHR
; INPUTS:
;	SARR
;		Character array, mandatory.
;	SUB
;		Substitution string, mandatory.
; OPTIONAL INPUT PARAMETERS:
;	POS
;		Substitution position within the string.  If not given, defaults to 0.
;		May be given as a vector, forcing substitution in numerous locations.
; KEYWORD PARAMETERS:
;	LINE
;		Numeric, scalar or vector, the line number(s) where	|	One and only 
;		substitution is required.							|	one of these 
;	PHRASE													|	two need to
;		Character scalar, the beginning of the line(s) where|	be given.
;		substitution is required.							|
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None other then the substitution in STRARR.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls DEFAULT, ISNUM, ONE_OF, STREQ and TYPE, from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 15-APR-2015 by Mati Meron.
;-

	on_error, 1

	if Type(sarr) eq 7 then begin
		case One_of(lin,phr) of
			-1	:	wlin = []
			0	:	wlin = lin
			1	:	begin
						harr = strmid(sarr,0,strlen(phr))
						wlin = where(Streq(harr,phr),/null)
					end
		endcase

		if Isnum(wlin) then begin
			tem = sarr[wlin]
			wpos = Default(pos,0,/dtyp)
			for i = 0, n_elements(wpos)- 1 do strput, tem, sub, wpos[i]
			sarr[wlin] = tem 
		endif
	endif else message, 'Not a string array!'

	return
end