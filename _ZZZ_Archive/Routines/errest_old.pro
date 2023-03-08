Function Errest_old, dat, emode = emd, squared = sqr, total = tot

;+
; NAME:
;		ERREST
; VERSION:
;		8.02
; PURPOSE:
;		Calculating error estimate for data
; CATEGORY:
;		Esperimental data processing.
; CALLING SEQUENCE:
;		Result = ERREST (DAT [, keywords])
; INPUTS:
;	DAT
;		Numeric, otherwise arbitary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/EMODE
;		Switch.  If set, error is calculated for "product of Poissonians", i.e.
;		for the APEX detector, else "single Poissonian" square-root error is
;		calculated.
;	/SQUARED
;		Switch.  If set, the square of the error is returned.
;	/TOTAL
;		Switch.  If set, the errors are summed in quadrature.
; OUTPUTS:
;		Returns the value(s) of data errors in the same format as DAT, unless
;		/TOTAL is set, in which case a scalar is returned.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 25-MAR-2011 by Mati Meron.
;-

	on_error, 1
	idu  = 0.1
	epf = 200.

	if keyword_set(emd) then res= idu*dat*(1+ epf+ dat/(idu*epf)) else res= dat
	if keyword_set(tot) then res = total(res)
	if not keyword_set(sqr) then res = sqrt(res > 0)

	return, FPU_fix(res)
end