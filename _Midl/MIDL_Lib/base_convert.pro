Function Base_convert, num, from = fir, to = sec

;+
; NAME:
;		BASE_CONVERT
; VERSION:
;		8.73
; PURPOSE:
;		Converts integer type numbers from one base to another
; CATEGORY:
;		Math Utility.
; CALLING SEQUENCE:
;		Result = BASE_CONVERT( NUM, FROM = FIR, to = SEC)
; INPUTS:
;	NUM
;		Either a positive scalar of integer type or a string representation of
;		such.  Thus both 12345 and '12345' are acceptable.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FROM
;		Integer scalar representing the number base of the input.  Must be
;		between 2 and 10.
;	TO
;		Integer scalar representing the required number base of the output. Must
;		be between 2 and 10.
; OUTPUTS:
; 		Returns a character string representation of the input NUM, converted,
; 		from base "FROM" to base "TO".
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the restrictions listed above for NUM, FROM and TO.
; PROCEDURE:
;		Straightforward.  Calls DEFAULT, ISNUM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2021 by Mati Meron.
;-

	on_error, 1

	if type(num) eq 7 then fnum = long(num) else fnum = num
	if Isnum(fnum,/int) then begin
		if fnum gt 0 then begin
		wfir = Default(fir,10l,/dtyp)
		wsec = Default(sec,10l,/dtyp)
			if Isnum(wfir,/int) and Isnum(wsec,/int) $
			and min([wfir,wsec],max=max) gt 1 and max le 10 then begin
				cfnum = strtrim(string(fnum),2)
				if wfir ne wsec then begin
					flen = strlen(cfnum)
					farr = lonarr(flen)
					for i = 0, flen-1 do farr[i] = long(strmid(cfnum,i,1,/rev))
					dnum = total(farr*wfir^lindgen(flen),/preserve)
					slen = floor(alog(dnum)/alog(wsec))+1
					sarr = lonarr(slen)
					i = 0
					repeat begin
						tem = dnum/wsec
						sarr[i] = dnum - tem*wsec
						dnum = tem
						if dnum eq 0 then break
						i = i + 1
					endrep until 0
					res = strjoin(string(reverse(sarr),form='(i0)'))
				endif else res = cfnum
			endif else message, $
			'"FROM" and "TO" must be scalar integers >1 and <=2!'
		endif else message, 'NUM must be positive!'
	endif else message, 'NUM must be of integer type!'

	return, res
end