Function Braket_check, str, bra=bra, ket=ket, open= opi, close= cli, reverse=rev

;+
; NAME:
;		BRAKET_CHECK
; VERSION:
;		8.41
; PURPOSE:
;		Checks for the presence and arrangement of "brackets" in a string.
; CATEGORY:
;		String Processing.
; CALLING SEQUENCE:
;		Result = BRAKET_CHECK ( STR, BRA = BRA, KET = KET [, other keywords])
; INPUTS:
;	STR
;		Character string, mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	BRA
;		1-character string, serving as opening bracket.  Mandatory.
;	KET
;		1-character string, serving as closing bracket.  Mandatory.
;	OPEN
;		Optional output, see below.
;	CLOSE
;		Optional output, see below.
;	/REVERSE
;		Switch.  If set, the order of entries in both OPEN and CLOSE is reversed
; OUTPUTS:
;		Returns the number of bracket pairs found.
; OPTIONAL OUTPUT PARAMETERS:
;	OPEN
;		Returns the locations of opening brackets in the string.  If none are
;		present, returns !NULL.
;	CLOSE
;		Returns the locations of opening brackets in the string.  If none are
;		present, returns !NULL.
;		
;		Note:	The locations in OPEN and CLOSE are ordered, so that each entry
;				in CLOSE is the closing bracket corresponding to the opening
;				one in same location, in OPEN.  The order is "from the outside 
;				to inside", unless the keyword /REVERSE is used.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The number of opening and closing brackets must be the same (can be 0).
;		At no point along the string there can be a negative balance, i.e. more
;		closing then opening brackets up to this point.
; PROCEDURE:
;		Straightforward.
; MODIFICATION HISTORY:
;		Created 5-MAR-2015 by Mati Meron.
;-

	on_error, 1

	len = strlen(str)
	if len gt 0 then begin
		if strlen(bra) eq 1 and strlen(ket) eq 1 then begin
			byt = byte(str)
			opb = byte(bra)
			clb = byte(ket)
			chg = lonarr(len)
			for i = 0, len-1 do begin
				case byt[i] of
					opb	:	chg[i] = 1
					clb	:	chg[i] = -1
					else:
				endcase
			endfor
			check = total(chg,/cum,/pres)
			if check[-1] eq 0 then begin
				if min(check) eq 0 then begin
					opi = (cli = [])
					res = (npair = total(abs(chg),/pres)/2)
					while npair gt 0 do begin
						clind = (where(chg eq -1))[0]
						opind = (where(chg[0:clind-1] eq 1))[-1]
						opi = [opi,opind]
						cli = [cli,clind]
						chg[opind] = (chg[clind] = 0)
						npair = npair - 1
					endwhile
				endif else message, 'Close before open, unacceptable!'
			endif else message, 'Unbalanced brackets!'
		endif else message, 'Bra and Ket must be 1-character strings!'
	endif else message, 'Missing input!'

	if not keyword_set(rev) then begin
		opi = reverse(opi)
		cli = reverse(cli)
	endif

	return, res
end