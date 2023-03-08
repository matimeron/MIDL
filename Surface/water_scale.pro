Pro Water_scale, ref, range = ran, rough = rof, fit_rough = fro, $
	show_fit= shf, correct_off= cof, result= res, _extra = _e

;+
; NAME:
;		WATER_SCALE
; VERSION:
;		4.8
; PURPOSE:
;		Scales reflectivity data by water Fresnel reflectivity, optionally
;		with roughness included.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		WATER_SCALE, REF, keywords
; INPUTS:
;	REF
;		Reflectivity data, in the standard [3,*] array format.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RANGE
;		An optional 2 element vector setting lower and upper limit for Q-range
;		for evaluation.  If only one value is given, it is taken as the lower
;		limit, with the upper limit given by the data.
;	ROUGH
;		Optional roughness value to be used in the fitting.  If not provided,
;		zero is used unless FIT_ROUGH (see below) is set.
;	/FIT_ROUGH
;		Switch.  If set, Fit for roughness is peformed.
;	/SHOW_FIT
;		Switch.  If set, opens a second window and shows the original data
;		and the fit.
;	/CORRECT_OFF
;		Switch.  Active only when RESULT is used, see there.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to SCAN_SHOW.
;		Not to be used directly.
; OUTPUTS:
;		None, other than screen output.
;
;		Note:	The "Qz offset" result is what needs to be *added* to the input
;		Qz values to get the "correct" result.  Similarly, the "Norm Coeff"
;		result is what needs to be *multiplied* by the input reflectivity to
;		get the "correct" result.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the input data scaled by the fit in the standard [3,*] form:
;
;		Column 0	:	QZ
;		Column 1	:	Reflectivity divided by fitted reflectivity.
;		Column 2	:	Squared errors.
;
;		Note:	If the switch CORRECT_OFF is set, the first column of RESULT
;				is adjusted to QZ + QZ_offset.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		WATER_SCALE serves as a front end for REFFIT, with the Q_crit value
;		fixed at the water value.  See there for details.  Other than REFFIT,
;		the routine calls QCRIT from SRUFF_LIB and DEFAULT and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 15-APR-2004 by Mati Meron.
;-

	on_error, 1

	qc = (Qcrit(elem=['h','o'],wei=[2,1],/form,dens=1))[0]
	rofl = One_of(rof,fro) > 0
	wro = Default(rof,0)
	if n_elements(ran) eq 0 then if rofl then ran = [0.5,2] else ran = [0.5,8]
	ran = qc*ran

	if rofl then Reffit, $
	ref,ran=qc*[0.5,10],qc=qc,kmu=0,sho=shf,corr=cof,res=res,_extra=_e $
	else Reffit, $
	ref,ran=qc*[0.5,4],qc=qc,kmu=0,rou=wro,sho=shf,corr=cof,res=res,_extra=_e

	return
end