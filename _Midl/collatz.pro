Function Collatz, num, u64 = u64, all = all, $
	count = con, first_absent = fab, last_present = lap

;+
; NAME:
;		COLLATZ
; VERSION:
;		8.72
; PURPOSE:
;		Calculates the Collatz sequence for the input number.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = COLLATZ( NUM [, keywords])
; INPUTS:
; 	NUM
; 		Scalar integer.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	/U64
;		Switch.  If set, internal calculations used unsigned long64 values.
;		Default is unsigned long.
;	/ALL
;		Switch.  If set, all the values in the sequence are kept and counted.
;		By default, only odd values are kept and counted.
;	COUNT
;		Optional output, see below.
;	FIRST_ABSENT
;		Optional output, see below.
;	LAST_PRESENT
;		Optional output, see below.
; OUTPUTS:
;		Returns the Collatz sequence of numbers corresponding to the input.
; OPTIONAL OUTPUT PARAMETERS:
;	COUNT
;		Returns the number of values in the output (including the final 1).
;	FIRST_ABSENT												| When /ALL is
;		Returns the lowest odd value absent from the output.	| set, these
;	LAST_PRESENT												| keywords
;		Returns the highest odd value present in the output.	| return !NULL.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		Input must be a scalar integer.
; PROCEDURE:
;		Straightforward, from definition. Call XUNION_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 5-DEC-2018 by Mati Meron.
;		Documented 25-DEC-2020 by Mati Meron.
;-

	on_error, 1

	bufl = 1024l
	if keyword_set(u64) then buf = ulon64arr(bufl) else buf = ulonarr(bufl)
	res = buf
	con = 0
	inum = long(num) > 1l

	repeat begin
		res[con] = inum
		con = con + 1
		if inum eq 1 then break
		if inum mod 2 then inum = 3*inum + 1 else inum = inum/2
		if con mod bufl eq 0 then res = [res,buf]
	endrep until 0

	res = res[0:con-1]
	if not keyword_set(all) then begin
		res = res[where(res mod 2,con)]
		lap = max(res)
		comp = 2*lindgen((lap+3)/2) + 1
		fab = (Xunion_mm(res,comp))[0]
	endif else fab = (lap = !null)

	return, res
end