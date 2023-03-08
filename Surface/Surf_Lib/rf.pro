Function RF, qz, qc

;+
; NAME:
;		RF
; VERSION:
;		8.07
; PURPOSE:
;		Calculating Fresnel Reflectivity from Qz and Qc.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = RF( QZ, QC)
; INPUTS:
;	QZ
;		Numeric scalar or vector, the reflection Q_z.  Mandatory.
;	QC
;		Numeric scalar, real or complex, the reflection Q_c.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		Returns the Fresnel Reflectivity corresponding to the given Qz and Qc.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, from definition.  Calls ABS_MM, CALCTYPE and CAST, 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JUL-2011 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(0.,qz,qc)
	if typ eq 5 or typ eq 9 then rtyp = 5 else rtyp = 4
	srt = sqrt(qz^2 - dcomplex(qc)^2)
	res = (Abs_mm((qz - srt)/(qz + srt)))^2

	return, Cast(res,rtyp,rtyp,/fix)
end
	