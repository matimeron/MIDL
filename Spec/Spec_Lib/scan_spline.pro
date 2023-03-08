Function Scan_spline, q, scan, lcompress = lco, pack = pac, deriv = der

;+
; NAME:
;		SCAN_SPLINE
; VERSION:
;		8.23
; PURPOSE:
;		Generates additional "data" from existing data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = SCAN_SPLINE( Q, [SCAN, [keywords]])
; INPUTS:
;	Q
;		Numeric scalar or vector.  Serves as the X-coordinate of the data.
; OPTIONAL INPUT PARAMETERS:
;	SCAN
;		A single valid scan, i.e. a [3,n] array ([2,n] array also acceptable.
;		If not given, last given scan data is reused.
; KEYWORD PARAMETERS:
; 	/LCOMPRESS
; 		Switch.  If set, the source scan is logarithmically compressed prior to
; 		calculating the spline coefficients, then the result is exponenntially
;		decompressed prior to returning.  Useful with data rapidly changing over
;		a large range.
;		
;		Note:	LCOMPRESS has no effect when using previously saved data.  Also
;				while saving, LCOMPRESS is ignored if the data contains negative
;				values.
; 	/PACK
; 		Switch.  If set, the calculated output is packed together with the
; 		Q input into a [2,n] or [3,n] (if the original SCAN was [3,n]) format.
; 		
; 		Note:	If DERIV (see below) is set, the packing is into [2,n] format
; 				even if the SCAN data was [3,n].
; 	/DERIV
; 		Switch.  If set, first derivative of the function is returned instead 
; 		of the function itself.
; OUTPUTS:
; 		Returns the Y-values, calculated from the splines of prior data, in 
; 		same format as Q.  If DERIV is set, the numerical derivative of the Y
; 		values is returned.
; 		If /PACK is set, returns the Y-values packed together with the input Q
; 		in the form of a [2,n] scan or, if the original scan included error 
; 		values, in the form of a [3,n] scan.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		SCAN_SPLINE_KEEP, contains the following:
;			SPC	-	Spline coefficients from previously provided data.
;			ESPC-	Spline coefficients for the errors of previously provided
;					data.
;			LFL	-	Flag, set to 1 SPC and ESPC were generated with LCOMPRESS.
;			EFL-	Flag, set to 1 if previous data included error info.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		No result can be generated until the scan data needed to evaluate the
;		splines has been provided (can be previous call).
; PROCEDURE:
;		Straightforward, uses SCAN_VER.  Calls FPU_FIX, ISNUM, JOIN_XY, 
;		SPLIN_COEFFS and SPLIN_EVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-AUG-2012 by Mati Meron.
;		Modified and generalized 20-FEB-2014 by Mati Meron.  Added keywords
;		LCOMPRESS and DERIV. 
;-

	common scan_spline_keep, spc, espc, lfl, efl 
	on_error, 1

	if Isnum(scan) then begin
		sver = Scan_ver(scan)
		if sver then begin
			efl = Scan_ver(scan) eq 1
			lfl = keyword_set(lco) and (min(scan[1,*]) gt 0)
			if lfl then begin
					spc = Splin_coeffs(scan[0,*],alog(scan[1,*]))
					if efl then espc=Splin_coeffs(scan[0,*],scan[2,*]/scan[1,*])
			endif else begin
				spc = Splin_coeffs(scan[0,*],scan[1,*])
				if efl then espc = Splin_coeffs(scan[0,*],scan[2,*])
			endelse
		endif else message, 'Not a scan data input!'
	endif

	if Isnum(spc) then begin
		dfl = keyword_set(der)
		if lfl then begin
			res = exp(Splin_eval(q,spc))
			if dfl then res = res*Splin_eval(q,spc,/der) $
			else if efl then eres = res*Splin_eval(q,espc)
		endif else begin
			res = Splin_eval(q,spc,der=dfl)
			if efl and not dfl then eres = Splin_eval(q,espc)
		endelse
		if keyword_set(pac) then res = Join_xy(q,res,eres)
	endif else message, 'Spline not defined yet!'

	return, FPU_fix(res)
end