Function Arrpack, p_0, p_1,  p_2,  p_3,  p_4,  p_5,  p_6,  p_7, $
			p_8, p_9, p_10, p_11, p_12, p_13, p_14, p_15, $
			p_16, p_17, p_18, p_19, p_20, p_21, p_22, p_23, $
			p_24, p_25, p_26, p_27, p_28, p_29, p_30, p_31, count = cnt

;+
; NAME:
;		ARRPACK
; VERSION:
;		6.4
; PURPOSE:
;		Combining variables into a string array.
; CATEGORY:
;		Programing utility.
; CALLING SEQUENCE:
;		Result = ARRPACK( P_0, .... [, WHICH = WHI])
; INPUTS:
;	P_0, P_1, ...
;		Parameter names or values, arbitrary, don't even need to be defined.
;		Maximal number accepted is currently 32 but can be extended.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COUNT
;		Optional output, see below.
; OUTPUTS:
;		Returns a character array containing the concantenation of all the
;		inputs (converted to strings when needed).
; OPTIONAL OUTPUT PARAMETERS:
;	COUNT
;		Returns the total number of elements in the output, 0 if there are none.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls NPARDEF from MIDL.
; MODIFICATION HISTORY:
;		Created 25-SEP-2007 by Mati Meron.
;		Modified 30-OCT-2007 by Mati Meron.  Added keyword COUNT.
;-

	on_error, 1

	nmx = 32
	pnam =  strcompress('p_' + sindgen(nmx),/remove)
	si = sindgen(nmx)
	np = Npardef(p_0, p_1,  p_2,  p_3,  p_4,  p_5,  p_6,  p_7, $
			p_8, p_9, p_10, p_11, p_12, p_13, p_14, p_15, $
			p_16, p_17, p_18, p_19, p_20, p_21, p_22, p_23, $
			p_24, p_25, p_26, p_27, p_28, p_29, p_30, p_31,which = whi)
	if np gt 0 then begin
		len = lonarr(np)
		for i = 0, np - 1 do dum = $
			execute('len[' + si[i] + '] = n_elements(' + pnam[whi[i]] + ')')
		res = strarr(total(len,/int))
		ind = [0,total(len,/int,/cum)]
		for i = 0, np - 1 do dum = $
			execute('res[' + string(ind[i]) + ':' + string(ind[i+1] - 1) + $
			'] = strcompress(string(' + pnam[whi[i]] +'),/rem)')
		cnt = n_elements(res)
	endif else begin
		res = ''
		cnt = 0
	endelse

	return, res
end