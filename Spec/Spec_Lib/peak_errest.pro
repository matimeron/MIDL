Function Peak_errest, y, fixed = fix,  poisson = pos

;+
; NAME:
;		PEAK_ERREST
; VERSION:
;		8.214
; PURPOSE:
;       Estimating data errors.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_ERREST ( Y [, keywords])
; INPUTS:
;	Y
;		Numeric, A vector (scalar is considered to be a vector of length 1), an
;		[2,*] array or a [3,*] array.
;		
;		Note:	If Y is actually a [2,*] or [3,*] data array, the second column
;				of this array is taken as the "working Y".
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	/FIXED												|	Only one of these
;		Switch.  If set, fixed errors are assumed.		|	two may be set.  If
;	/POISSON											|	none is, default is
;		Switch.  If set, Poisson errors are assumed		|	FIXED
; OUTPUTS:
; 		If FIXED is set, the actual (constant) magnitude of <delta_Y> is 
; 		returned.
; 		If POISSON is set, PEAK_ERREST returns a scaling coefficient K such that
; 		<delta_Y> = K*sqrt(y).
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		At least 3 points are needed for evaluation.
; PROCEDURE:
; 		Straightforward statistical evaluation.  Calls PEAK_STRIP.  Calls CAST, 
; 		FPU_FIX, ONE_OF and SPLIT_XY, from MIDL. 
; MODIFICATION HISTORY:
;		Created 5-NOV-2013 by Mati Meron.
;-

	on_error, 1

	wha = One_of(fix,pos) > 0
	nxy = Split_xy(Peak_strip(Cast(y,4)),y_ret = wy)
	if nxy ge 3 then begin
		z = (wy[0:-3] + wy[2:-1] - 2*wy[1:-2])^2/(6*(nxy-2))
		if wha then z = z/wy[1:-2]
		res = sqrt(total(z))
	endif else message, 'Need at least 3 points to evaluate!'

	return, FPU_FIX(res)
end