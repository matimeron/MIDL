Function Rotate_oper, ax, tet, degrees = deg

;+
; NAME:
;	ROTATE_OPER
; PURPOSE:
;	Generates a 3 dimensional rotation operator.
; CATEGORY:
;	Geometry, 3 dimensional.
; CALLING SEQUENCE:
;	Result = ROTATE_OPER ( AX, TET [,/DEGREES] )
; INPUTS:
;    AX
;	3 dimensional vector in the direction of the rotation axis.
;    TET
;	Scalar, rotation angle.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    DEGREES
;	If set, TET is assumed to be in degrees.  Default is radians.
; OUTPUTS:
;	Returns the 3-D operator representing a rotation by TET around AX.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses the mathematical definition of the rotation operator.  Also uses
;	TYPE from MIDL and UNIVEC, IDENT_OPER and PROJECT_OPER from XOPER_LIB.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    on_error, 1
    if n_elements(ax) ne 3 then message, 'Not a 3-D vector!'
    if keyword_set(deg) then rtet = !dtor*tet else rtet = Cast(tet,4)
    rax = Univec(ax)

    return, cos(rtet)*Ident_oper(3,type = Type(rax)) + $
    (1 - cos(rtet))*Project_oper(rax) + $
    sin(rtet)*[[0,rax(2),-rax(1)],[-rax(2),0,rax(0)],[rax(1),-rax(0),0]]
end
