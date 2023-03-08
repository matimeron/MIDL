Function Samp_inter, y_samp = yor, degrees = deg

;+
; NAME:
;	SAMP_INTER
; PURPOSE:
;	Calculates position of reflected beam on sample.
; CATEGORY:
;	X-ray optics.
; CALLING SEQUENCE:
;	Result = SAMP_INTER ([ keywords])
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    Y_SAMP
;	Optional output parameter, see below.
;    DEGREES
;	Switch.  Specifies that angles are to be measured in degrees.
; OUTPUTS:
;	Returns the horizontal projection of the angle between the reflected 
;	beam and the Z direction (nominal incoming beam direction).
; OPTIONAL OUTPUT PARAMETERS:
;    Y_SAMP
;	The name of a variable to accept the the Y position (vertical 
;	intercept) of the reflected beam with the sample.
; COMMON BLOCKS:
;	Common block, X19C.  Established by XMONO_COMM.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	XMONO_COMM must be called first to initialize the common blocks.
; PROCEDURE:
;	Straightforward geometry.  Calls REFLECT_ELEM, PROJECT_OPER, VSCALP and
;	ANGLE, all from XOPER_LIB.
; MODIFICATION HISTORY:
;	Created 30-JULY-1992 by Mati Meron.
;-

    common x19c, ready, ori, x, y, z, inc, todee, rsam, rdet, totoff, beam, $
    nmo, nsp, monoc, spect, monoc_pars, spect_pars

    refbeam = Reflect_elem(beam,monoc(nmo), name = 'Reflected ray')
    inp = Cylintercept(refbeam,spect(0),rsam,lonv = rlon,trav = rtra)
    yor = Vscalp(rlon,spect(0).dir)

    return, Angle(z,rtra, degrees = deg)
end
