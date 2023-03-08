Function StrParse_mm, line, delim, list, null = nul

;+
; NAME:
;		STRPARSE_MM
; VERSION:
;		8.216
; PURPOSE:
;		Parses the string LINE using the characters in DELIM as delimiters.
;		Puts individual pieces into consecutive locations in LIST.
; CATEGORY:
;		String Processing.
; CALLING SEQUENCE:
;		Result = STRPARSE_MM( LINE, DELIM [, LIST] [,/NULL])
; INPUTS:
;	LINE
;		Character string.
;	DELIM
;		Character string.  Each character of DELIM is used as a delimiter.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/NULL
;		Switch.  Same as in the WHERE function.  See OUTPUTS and LIST below.
; OUTPUTS:
;		Returns the number of pieces found minus one i.e. the index of the last
;		element of LIST if LIST is provided.  If LINE is a null string or not a
;		string, the function returns -1l (or !NULL, if /NULL is set)
; OPTIONAL OUTPUT PARAMETERS:
;	LIST
;		Character array.  If name is provided, the pieces of LINE resulting
;		from the parsing process are returned in consecutive locations in LIST.
;		If there are no pieces to return, LIST returns zero length string (or
;		!NULL, if /NULL is set).
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Using SORPURGE and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUL-1991 by Mati Meron as STRPARSE.
;		Renamed 25-SEP-1999 to STRPARSE_MM in order to avoid naming conflicts
;		with RSI routines.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Rewritten 20-JAN-2014 by Mati Meron.  Same functionality (other than the
;		addition of the NULL keyword) but speed greatly increased.
;-

	on_error, 1

	if Type(line) eq 7 then begin
		len = strlen(line)
		if len gt 0 then begin
			if Type(delim) eq 7 then begin
				blin = byte(line)
				bdl = byte(delim)
				ind = [-1l,len]
				for i = 0, n_elements(bdl)-1 do ind= [ind,where(blin eq bdl[i])]
				ind = ind(Sorpurge(ind,net=net))
				lind = ind[0:-2] + 1
				hind = ind[1:-1] - 1
				ok = where(lind le hind,nok)
				if nok gt 0 then begin
					res = nok - 1
					list = strarr(nok)
					for j = 0, nok - 1 do $
						list[j] = string(blin[lind[ok[j]]:hind[ok[j]]])
				endif
			endif else message, 'Delimiter input missing!'
		endif else nok = 0
	endif else nok = 0

	if nok eq 0 then begin
		if keyword_set(nul) then begin
			res = !null
			list = []
		endif else begin
			res = -1l
			list = ''
		endelse
	endif

	return, res
end