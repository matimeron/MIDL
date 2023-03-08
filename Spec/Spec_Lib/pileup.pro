Function Pileup, spec, nitau

;+
; NAME:
;		PILEUP
; VERSION:
;		4.3
; PURPOSE:
;		Adding pileup to a spectrum, or removing it.
; CATEGORY:
;		Data analysis.
; CALLING SEQUENCE:
;		Result = PILEUP ( SPEC, NITAU)
; INPUTS:
;	SPEC
;		Spectrum, a 1D vector.
;	NITAU
;		Scalar, the product of frequency and time constant.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		For positive NITAU returns the spectrum produced from SPEC due to
;		pileup.  For negative NITAU, assumes that SPEC includes pileup
;		contributions and restores the original spectrum, to the extent
;		possible.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Described in the "Pileup Calculations" writeup.  Calls FPU_FIX and
;		TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 10-MAR-1991 by Mati Meron.
;		Streamlined 1-JUL-1996 by Mati Meron.
;-

	on_error, 1
	del = Toler(spec)

	res = [0.,spec(1:*)]
	eps = 1 - exp(-abs(nitau))
	if eps ge del then begin
		tot = total(res)
		if tot gt 0 then begin
			meps = 1 - exp(-nitau)
			ns = n_elements(res)
			qsiz = 2l^ceil(alog(ns*(1 + alog(del)/alog(eps)))/alog(2))
			tem = fltarr(qsiz)
			tem(0:ns-1) = res/tot
			tem = fft(tem,1,/overwrite)
			tem = (1 - meps)*tem/(1 - meps*tem)
			tem = fft(tem,-1,/overwrite)
			res = (1 - meps)*tot*float(tem(0:ns-1))
		endif
	endif

	return, FPU_fix(res)
end