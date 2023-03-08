Function Ident_oper, n, type = typ

;+
; NAME:
;	IDENT_OPER
; PURPOSE:
;	Creates an identity matrix.
; CATEGORY:
;	Geometry, General.
; CALLING SEQUENCE:
;	Result = IDENT_OPER ( N [, TYPE = TYP)
; INPUTS:
;    N
;	Integer scalar, the dimension of the matrix.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    TYPE
;	Integer in the [1,6] range.  Specifies numeric type of the result.
;	Default is 4 (FLOATING).
; OUTPUTS:
;	Returns an N*N identity matrix.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Uses DEFAULT from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    res = make_array(n, n, type = Default(typ,4))
    res((n+1)*indgen(n)) = 1

    return, res
end
