Function Qc_opt_fun, q, dat, order = ord, exclude = exc, _extra = _e

;+
; NAME:
;		QC_OPT_FUN
; 	VERSION:
;		8.42
; PURPOSE:
;		Evaluates a measure of the deviation of Fresnel normalized reflectivity. 
;		Primary purpose is to serve as fitting evaluation function, in QC_OPT.
; CATEGORY:
;		Mathematical, x-ray specific.
; CALLING SEQUENCE:
;		Result = QC_OPT_FUN(Q, DAT, _EXTRA = _E)
; INPUTS:
;	Q
;		Trial value(s) for Qc.  
;	DAT
;		Reflectivity data in the standard [3,N] form ([2,N] is acceptable).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	ORDER
; 		Integer scalar, the order of the numeric derivative used in the 
; 		fitting.  Woring range is 1-3, default is 1.
; 	EXCLUDE
; 		Two element vector, if given, data points within the range defined
; 		by EXCLUDE are eliminated from calculation.  For testing purposes.
; 	_EXTRA
; 		Formal keyword, has no role here other than to comply with the call
; 		requirements of QND_EXT.
; OUTPUTS:
;		Returns a measure of the variation of the Fresnel normalized
;		reflectivity, for each of the values in Q.  If the input Q is scalar, 
;		the output will be scalar as well.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The DAT input must be of proper form, as delineated above.
; PROCEDURE:
; 		Evaluates a measure of the variation of the Fresnel normalized
; 		reflectivity as
; 			res ~ Integral {((d/dq)^N (R/Rf)/(R/Rf))^2}
; 		Where N is the value of ORDER and the derivative is evaluated 
; 		numerically.  Calls SCAN_RFNORM and SCAN_VER from SPEC.  Calls CALCTYPE
; 		and DIF from MIDL.
; MODIFICATION HISTORY:
;		Created 25-MAY-2015 by Mati Meron.
;-

	on_error, 1

	if Scan_ver(dat) then begin
		n = n_elements(q)
		res = make_array(n,type=Calctype(q,dat,0.))
		x = reform(dat[0,*])
		dx = Dif(x,/cen,/lin)
		word = 1 > Default(ord,1,/dtyp) < 3
		if n_elements(exc) eq 2 then begin
			exlo = min(exc,max=exhi)
			dum = where(dat[0,*] le exlo or dat[0,*] ge exhi,ndum)
			if ndum gt 0 then wdat = dat[*,dum] else message, 'No points left!'
		endif else wdat = dat
		for i = 0, n-1 do begin
			rdat = Scan_rfnorm(wdat,q[i])
			y = reform(rdat[1,*])
			res[i] = total((Dif(y,word,/cen,/lin)/y)^2/dx^(2*word-1))
		endfor
		if (size(q))[0] eq 0 then res = res[0]
	endif else message, 'Not scan data!'

	return, res
end