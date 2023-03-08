Function Line_2po, pf, ps, name = nam, idnum = id

;+
; NAME:
;	LINE_2PO
; PURPOSE:
;	Generates a GELEM structure representing a line passing through two 
;	given points.  For description of a GELEM structure, see MAKE_ELEM.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = LINE_2PO ( PF, PS [, keywords])
; INPUTS:
;    PF
;	Either a 3 dimensional vector, or GELEM structure representing a point.
;    PS
;	Ditto.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the line.
;	Default name is "Line".
;    IDNUM
;	Accepts a number that is used as the IDN field of the line.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing a line passing through the 
;	points PF, PS.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	If PF and PS represent the same point, an "Undefined" GELEM is returned
;	instead of a line.
; PROCEDURE:
;	Uses the two points to establish direction and calls MAKE_LINE 
;	(in XOPER_LIB).  Also uses TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if Type(pf) ne 8 then tpf = pf else $
    if pf.dim eq 0 then tpf = pf.loc else message, 'Not a point!' 
    if Type(ps) ne 8 then tps = ps else $
    if ps.dim eq 0 then tps = ps.loc else message, 'Not a point!' 

    return, Make_line(tpf, tps - tpf, name = nam, idnum = id)
end
