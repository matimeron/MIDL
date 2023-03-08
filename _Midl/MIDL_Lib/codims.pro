Function Codims, p_0, p_1, p_2, p_3, p_4, p_5, p_6, p_7, $
	dims = dms, same = sam, ninp = pnum

;+
; NAME:
;		CODIMS
; VERSION:
;		8.16
; PURPOSE:
;		Checking for dimensional consistency.
; CATEGORY:
;		Programing utility.
; CALLING SEQUENCE:
;		Result = CODIMS ( P_0, .... [ keywords])
; INPUTS:
;	P_0, P_1, ...
;		Parameter names or values, arbitrary.  Don't need all to be defined but
;		at least one must exist and those defined must preceed the undefined on
;		input.  Maximal number accepted is currently 8 but can be extended.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	DIMS
;		Optional output, see below.
;	SAME
;		Optional output, see below.
;	NINP
;		Optional output, see below.
; OUTPUTS:
;		Returns 1 if the inputs are dimensionally consistent, 0 otherwise.
;		Inputs are dimensionally consistent in the following cases:
;
;			1)	All inputs are scalars.
;			2)	All inputs are arrays of the same dimensionality.
;			3)	A mix of (1) and (2).
;
;		Note:	A 1-element vector is not a scalar.
; OPTIONAL OUTPUT PARAMETERS:
;	DIMS
;		Returns a vector with the dimensions of the present array inputs.  If
;		all inputs are scalars, returns 0.
;	SAME
;		Returns 1 if all inputs have same dimensionality (cases (1-2) above),
;		else returns 0.
;	NINP
;		Returns the number of recognized inputs.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls NPARDEF and ARREQ from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2008 by Mati Meron.
;		Modified 5-AUG-2012 by Mati Meron.  Slight changes.  Added keyword NINP.
;-

	on_error, 1

	nmx = 8
	pnam =  strcompress('p_' + sindgen(nmx),/remove)
	si = sindgen(nmx)

	pnum = Npardef(p_0,p_1,p_2,p_3,p_4,p_5,p_6,p_7,whi=whi)
	if pnum gt 0 then begin
		if Arreq(whi,lindgen(pnum)) then begin
			dms = 0l
			res = 1l
			pnam =  strcompress('p_' + sindgen(pnum),/remove)
			dlis = lonarr(pnum)
			for i = 0l, pnum-1 do begin
				dum= execute('dlis[i]=(size('+pnam[i]+'))[0]')
				if dlis[i] gt 1 then $
				dum= execute('dlis[i]=(size(reform('+pnam[i]+')))[0]')
			endfor
			j = where(dlis ne 0, nj)
			if nj gt 0 then begin
				dum = execute('fir = reform(' + pnam[j[0]] +')')
				dms = size(fir,/dim)
				for k = 1l, nj-1 do begin
					dum = execute('sec = reform(' + pnam[j[k]] +')')
					res = res*Arreq(fir,sec,/nov)
				endfor
			endif
			if not res then dms = 0l
			sam = long((nj eq 0) or (nj eq pnum))
		endif else message, 'Undefined inputs present!'
	endif else message, 'No valid data present!'

	return, res
end