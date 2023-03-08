Function Cosangle, dirf, dirs

;+
; NAME:
;	COSANGLE
; PURPOSE:
;	Finds the Cosine of the angle between two vectors.
; CATEGORY:
;	Geometry, General.
; CALLING SEQUENCE:
;	Result = COSANGLE ( DIRF, DIRS)
; INPUTS:
;    DIRF, DIRS
;	Either vectors (same length) or variables of type GELEM (see 
;	explanation in the MAKE_ELEM routine).  In the second case only the 
;	direction fields are used.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the Cosine of the angle between the two directions.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses the functions VSCALP and VNORM from XOPER_LIB and TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    on_error, 1

    if Type(dirf) eq 8 then tdf = dirf.dir else tdf = dirf
    if Type(dirs) eq 8 then tds = dirs.dir else tds = dirs

    if n_elements(tdf) ne n_elements(tds) then message, 'Unequal dimensions!'

    return, - 1 > Vscalp(tdf,tds)/(Vnorm(tdf)*Vnorm(tds)) < 1
end
