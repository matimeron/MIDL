Function Evun, ev, wavelength = wav, kev_units = kev

;+
; NAME:
;	EVUN
; PURPOSE:
;	Converts energy (or wavelength) into energy in Ev units.
; CATEGORY:
;	X-ray calculations.
; CALLING SEQUENCE:
;	Result = EVUN (EV [, keywords])
; INPUTS:
;    EV
;	Numeric, scalar or array.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    WAVELENGTH
;	Switch, specifies that input is wavelength(s) in Angstrem units.
;    KEV_UNITS
;	Switch, specifies that input is energy in Kev units.
; OUTPUTS:
;	Returns energy in eV units.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Only one of the keywords may be used in every call.
; PROCEDURE:
;	Straightforward.  Uses ONE_OF from MIDL.
; MODIFICATION HISTORY:
;	Created 1-MARCH-1993 by Mati Meron.
;-

    on_error, 1
    conv = 12398.

    case (1 + One_of(kev,wav)) of
	0:	res = Cast(ev,4,5)
	1:	res = 1000.*ev
	2:	res = conv/ev
    endcase

    return, res
end
