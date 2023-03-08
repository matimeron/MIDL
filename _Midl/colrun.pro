Function Colrun, lim, u64 = u64, show = sho, $
	count = con, first_absent = fab, last_present = lap

;+
; NAME:
;		COLRUN
; VERSION:
;		8.72
; PURPOSE:
;		Iteration of Collatz sequence.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = COLRUN( LIM [, keywords])
; INPUTS:
; 	LIM
; 		Scalar integer.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	/U64
;		Switch.  If set, internal calculations used unsigned long64 values.
;		Default is unsigned long.
;	/SHOW
;		Switch.  If set, progression of the evaluation is shown.
;	COUNT
;		Optional output, see below.
;	FIRST_ABSENT
;		Optional output, see below.
;	LAST_PRESENT
;		Optional output, see below.
; OUTPUTS:
;		Repeatedly performs Collatz sequence evaluation for progressively
;		higher values till the union of the results includes all the odd numbers
;		up to and including LIM.  Returns said union of results.
; OPTIONAL OUTPUT PARAMETERS:
;	COUNT
;		Returns the number of values in the output.
;	FIRST_ABSENT
;		Returns the lowest odd value absent from the output.
;	LAST_PRESENT
;		Returns the highest odd value present in the output.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		Input must be a scalar integer.
; PROCEDURE:
;		Straightforward, from definition (see OUTPUTS).
;		Call XUNION_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 5-DEC-2018 by Mati Meron.
;		Documented 25-DEC-2020 by Mati Meron.
;-

	on_error, 1

	shofl = keyword_set(sho)
	llim = long(lim > 1)
	res = []
	fab = (lap = 1l)
	repeat begin
		res = [res,Collatz(fab,u64=u64)]
		res = res[Sorpurge(res,net=con)]
		nlap = max(res)
		if shofl and (fab eq 1 or nlap gt lap) then print, fab, nlap
		lap = nlap
		comp = 2*lindgen((lap+3)/2) + 1
		fab = (Xunion_mm(res,comp))[0]
	endrep until fab gt llim

	return, res
end