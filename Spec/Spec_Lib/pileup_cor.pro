Function Pileup_cor, cnt, tau

;+
; NAME:
;		PILEUP_COR
; VERSION:
;		4.8
; PURPOSE:
;		Correcting data for pileup.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = PILEUP_COR( CNT [, TAU])
; INPUTS:
;	CNT
;		Measured count rate.
; OPTIONAL INPUT PARAMETERS:
;	TAU
;		The pileup time constant.  Defaults to 0.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns CNT corrected for pileup, in the same format as this of CNT.
;		Note that for TAU = 0 the returned result equals CNT.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Serves as a front end for ROOT (from MIDL) which solves for the
;		correction using the kernel function PLP_SOLVE_KERN.  Also calls
;		DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2004 by Mati Meron.
;-

	on_error, 1

	nc = n_elements(cnt)
	res = 0.*cnt
	ttau = 2*Default(tau,0.)
	if ttau eq 0 then res = cnt $
	else for i = 0l, nc-1 do res[i] = $
	Root('plp_solv_kern',[cnt[i],1/ttau],par=[cnt[i],ttau])

	return, res
end